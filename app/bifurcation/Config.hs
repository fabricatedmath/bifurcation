{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Lens

import Language.Haskell.TH

import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Field.Hint
import Field.Hint.Config

data Descr =
  Descr
  { _optHintDescr :: HintDescr Float
  } deriving Show

makeLenses ''Descr

defaultDescr :: HintDescr Float -> Descr
defaultDescr hintDescr =
  Descr
  { _optHintDescr = hintDescr
  }

myLocation :: String
myLocation =
  $(do
       dir <- runIO getCurrentDirectory
       litE $ stringL $ dir
   )

loadConfigFromArgs :: IO Descr
loadConfigFromArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt Permute options args
    hintDescr <- loadHintDescrFromArgs "default" args
    _opts <- foldl (>>=) (pure $ defaultDescr hintDescr) actions
    return _opts

options :: [OptDescr (Descr -> IO Descr)]
options =
  [ Option "h" ["help"]
    (NoArg
      ((\_ -> do
          prg <- getProgName
          let stripType = map (fmap $ const ())
          hPutStrLn stderr $
            usageInfo prg
            ( stripType (hintDescrOptions :: [OptDescr (HintOption Double)]) ++
              stripType options
            )
          exitWith ExitSuccess
      ))
    ) "Show help"
  ]
