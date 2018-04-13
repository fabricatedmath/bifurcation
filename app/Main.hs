module Main where

import Lib

import Control.Monad (forever)

import Data.Array.Accelerate as A hiding ((>->))
import Data.Array.Accelerate.Data.Colour.RGB
import qualified Data.Array.Accelerate.Data.Colour.HSL as HSL
import Data.Array.Accelerate.Linear as A
import Data.Array.Accelerate.LLVM.PTX

import Field
import Field.Hint
import Field.Hint.Config

import Control.Lens
import Linear

import Pipes hiding (lift)
import Pipes.Safe
import Pipes.Graphics.Accelerate (openGLConsumer)

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
    print $ fd ^. fdRes
    print $ generator (V3 1 1 1)
    let
      coords = fromFunction dim (generator . dim2ToV3 0)
      image = run1 colorize coords
      producer = forever $ yield image
      printer = forever $ await >>= yield >> liftIO (print "yielded")
      consumer = openGLConsumer dim
    runSafeT $ runEffect $ producer >-> printer >-> consumer

colorize :: Acc (Array DIM2 (V2 Float)) -> Acc (Array DIM2 (V3 Word8))
colorize arr =
  let
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
