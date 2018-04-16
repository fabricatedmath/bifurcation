{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import Control.Monad (forever, forM_)

import Data.Array.Accelerate as A hiding ((>->))
import Data.Array.Accelerate.Data.Colour.RGB
import qualified Data.Array.Accelerate.Data.Colour.HSL as HSL
import Data.Array.Accelerate.Linear as A
import Data.Array.Accelerate.LLVM.PTX

import Field
import Field.Hint
import Field.Hint.Config

import Fluid
import Type

import Control.Lens
import Linear

import Pipes hiding (lift)
import qualified Pipes.Prelude as Pipes
import Pipes.Safe
import Pipes.Graphics
import Pipes.Graphics.Accelerate
import Prelude as P

import Acc.Lib

import System.Environment (getArgs)

dim2ToV2 :: DIM2 -> V2 Int
dim2ToV2 (Z :. y :. x) = V2 y x

v2ToDim2 :: V2 Int -> DIM2
v2ToDim2 (V2 y x) = (Z :. y :. x)

dim3ToV3 :: DIM3 -> V3 Int
dim3ToV3 (Z :. z :. y :. x) = V3 z y x

dim2ToV3 :: Int -> DIM2 -> V3 Int
dim2ToV3 i (Z :. y :. x) = V3 y x i

v3ToDim2 :: V3 Int -> DIM2
v3ToDim2 (V3 y x i) = (Z :. y :. x)

printer :: MonadIO m => Pipe a a m ()
printer =
  forM_ [(1::Int)..]
  (\i ->
      do
        await >>= yield
        if i `mod` 100 P.== 0 then liftIO $ print i else return ()
  )

main :: IO ()
main =
  do
    args <- getArgs
    hintDescr <- loadHintDescrFromArgs "default" args :: IO (HintDescr Float)
    let
      HintDescr
        { _hintDescrFD = fd
        , _hintDescrFS = fs
        } = hintDescr
      dim = v2ToDim2 $ fd ^. fdRes
      generator = generateCoords fd
      !idf = makeDensity_rgb dim
    print $ fd ^. fdRes
    print $ generator (V3 1 1 1)
    let
      coords = fromFunction dim (generator . dim2ToV3 0)
      func = applyFunc coords
      step = run1 (arrayToFlat . colorize . applyFunc coords)
      list = [0,0.001..]
      --list = concatMap (P.take 100 . repeat) list'
      producer = forM_ list yield
      --pipe = forever $ await >>= (\i -> yield $ step (A.fromList Z [i]))
      pipe = fluidPipe idf func
      consumer = Pipes.seq >-> forever (await >>= yield . flatToImage dim) >-> forever (Pipes.drop 100 >-> Pipes.take 1) >-> pngWriter 5 "/run/shm/bifurcation/i"
      --glConsumer = openGLConsumerFlat dim
      --glConsumer = openGLConsumer dim
      --consumer = pngWriter 5 "/run/shm/i"
    runSafeT $ runEffect $ producer >-> Pipes.take 10000 >-> pipe >-> printer >-> consumer
      --forever (await >>= liftIO . print) --forever (await >>= yield . arrayToImage) >-> consumer

applyFunc
  :: forall sh a. (Shape sh, A.Floating a)
  => Array sh (V2 a)
  -> Acc (Array DIM0 a)
  -> Acc (Array sh (V2 a))
applyFunc coords t =
  A.map (func (the t)) $ A.use coords
  where
    func :: forall a. A.Floating a => Exp a -> Exp (V2 a) -> Exp (V2 a)
    func t v' =
      let
        (V2 y x) = unlift v' :: V2 (Exp a)
        f = sin(2*sin(0.02*t)*y - 3*cos(0.03*t)*x)*exp(-abs (sin(0.11*t)*sin (3*x+1-2*y) - sin(0.19*t)*cos(x-3*y+1)))
        g = cos(2*sin(0.07*t)*y - 3*cos(0.05*t)*x)*exp(-abs (cos(0.13*t)*cos (3*x+1-2*y) - cos(0.17*t)*cos(x-3*y+1)))
      in
        lift $ V2 (g/500) (f/500) :: Exp (V2 a)


colorize :: Acc (Array DIM2 (V2 Float)) -> Acc (Array DIM2 (V3 Word8))
colorize arr =
  let
    maxV :: Exp Float
    maxV = the $ A.maximum $ flatten $ A.map A.norm $ arr

    color :: Exp (V2 Float) -> Exp Colour
    color v =
      let
        (V2 y x) = unlift v
        theta' = acos $ constant (V2 0 1) `A.dot` A.normalize v
        theta = cond (y A.< 0) (2*pi - theta') theta'
        h' = 360*theta/(2*pi)
        s' = 0.5
        v' = (*0.6) $ A.norm v / maxV
      in
        HSL.toRGB $ HSL.hsl h' s' v'
  in A.map (rgbToV3 . color) arr

rgbToV3 :: Exp Colour -> Exp (V3 Word8)
rgbToV3 c =
  let
    (RGB r' g' b') = unlift c :: RGB (Exp Float)
    r = A.round $ r' * 255
    g = A.round $ g' * 255
    b = A.round $ b' * 255
  in
    lift (V3 r g b)

vecToTup :: Exp (V2 Float) -> Exp (Float,Float)
vecToTup v =
  let
    (V2 y x) = unlift v :: V2 (Exp Float)
  in
    lift (y,x)

singleton :: Float -> Array DIM0 Float
singleton = A.fromList Z . return

fluidPipe
  :: Monad m
  => Array DIM2 (Float, RGB Float)
  -> (Acc (Array DIM0 Float) -> Acc (Array DIM2 (V2 Float)))
  -> Pipe Float (Array DIM1 Word8) m ()
fluidPipe idf func = f (idf,ivf)
  where
    !ivf = run1 (A.map vecToTup . func) $ singleton 0
    step =
      run1
      (\d ->
          let
            (t,arr) = unlift d
            e = fluid 100 0.01 0 0 arr
            (df',vf') = unlift e :: (Acc (Field RGBDensity), Acc VelocityField)
            cf' = makePicture df'
            vf'' = A.zipWith (.+.) (A.map vecToTup $ func t) $ decayVelocity 0.9999 vf'
          in
            lift (decayDensity 0.9999 df', vf'', arrayToFlat cf')
      )

    f (df,vf) =
      do
        t <- await
        let (df',vf',cf') = step (singleton t,(df,idf,vf))
        yield cf'
        f (df',vf')
