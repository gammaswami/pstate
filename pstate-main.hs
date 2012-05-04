module Main (main) where

import Control.Monad
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO.Unsafe
import Text.Printf
import Data.Boolean.CUDD
import NetlistData
import NetlistRead (loadNetlist)
import PStateData
import PStateBDD 
import PStateFixpoint
import Data.Vector ((!), forM_, toList)

data Flag = List String |
            High String |
            Low String |
            Prob String |
            Inputfile String

main =
  let args = unsafePerformIO getArgs
      nl = unsafePerformIO $ loadNetlist $ head args      
      s0 = if length args == 2 then 
             read $ head $ tail args 
           else 0.5
      ma = mkAllNet nl
      an = mkBDDNet s0 ma
      fl = mkFixlist an
  in
   do printf "State variable fixpoint priming: %f\n" s0
      b <- iterFixList 1000 [1,2,5,10] fl
      if b  then printResult an else return ()
      return ()