{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens

import Data.List (intercalate)

import Language.Haskell.TH

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

import Config

import Field.Hint

default_f :: String
default_f = "sin(2*sin(0.02*t)*y - 3*cos(0.03*t)*x)*exp(-abs (sin(0.11*t)*sin (3*x+1-2*y) - sin(0.19*t)*cos(x-3*y+1)))"

default_g :: String
default_g = "cos(2*sin(0.07*t)*y - 3*cos(0.05*t)*x)*exp(-abs (cos(0.13*t)*cos (3*x+1-2*y) - cos(0.17*t)*cos(x-3*y+1)))"

main :: IO ()
main =
  do
    args <- getArgs
    descr <- loadConfigFromArgs
    do
      let Left (Cartesian fs gs) = descr ^. optHintDescr.hintDescrFS
      writeFile (myLocation </> "f.s") fs
      writeFile (myLocation </> "g.s") gs
    setCurrentDirectory myLocation
    let yamlLoc = myLocation </> "stack.yaml"
        compileString = "stack --stack-yaml " ++ yamlLoc ++ " install :bifurcation-child-exe"
        runString =
          "time bifurcation-child-exe " ++
          (intercalate " " $ map (\a -> "\"" ++ a ++ "\"") args)
    (compileExitCode,o,e) <- readCreateProcessWithExitCode (shell compileString) ""
    print o
    print e
    print compileExitCode
    (_,_,_,h) <- createProcess $ (shell runString)
    waitForProcess h >>= exitWith
