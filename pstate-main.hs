module Main (main) where

import Control.Monad
import System.IO.Unsafe
import Text.Printf
import Data.Boolean.CUDD
import Netlist.Data
import Netlist.Read (loadNetlist)
import PState.Data
import PState.BDD 
import PState.Fixpoint
import PState.Options
import PState.Output
import Data.Vector ((!), forM_, toList)

main = do 
  (opts, noopts) <- processOptions
  -- printf "State variable fixpoint priming: %f\n" s0
  nl <- loadNetlist $ head $ optInput opts      
  ma <- mkAllNet nl
  an <- mkBDDNet (optPreset opts) ma
  fl <- mkFixlist an
  b <- iterFixList 1000 [1,2,5,10] fl
  if b && (optOutfile opts) then 
    (optOutput opts) $ textOutput an opts 
    else 
    if b then printResult an else return ()
  return ()