{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH

import System.Directory
import System.Exit
import System.FilePath
import System.Process

myLocation :: String
myLocation =
  $(do
       dir <- runIO getCurrentDirectory
       litE $ stringL $ dir
   )

main :: IO ()
main =
  do
    print "dog"
    writeFile (myLocation </> "f.s") "sin(2*sin(0.02*t)*y - 3*cos(0.03*t)*x)*exp(-abs (sin(0.11*t)*sin (3*x+1-2*y) - sin(0.19*t)*cos(x-3*y+1)))"
    writeFile (myLocation </> "g.s") "cos(2*sin(0.07*t)*y - 3*cos(0.05*t)*x)*exp(-abs (cos(0.13*t)*cos (3*x+1-2*y) - cos(0.17*t)*cos(x-3*y+1)))"
    setCurrentDirectory myLocation
    let yamlLoc = myLocation </> "stack.yaml"
    let compileString = "stack --stack-yaml " ++ yamlLoc ++ " install :bifurcation-child-exe"
        runString = "time bifurcation-child-exe "
    (compileExitCode,o,e) <- readCreateProcessWithExitCode (shell compileString) ""
    print o
    print e
    print compileExitCode
    (_,_,_,h) <- createProcess $ (shell runString)
    waitForProcess h >>= exitWith
