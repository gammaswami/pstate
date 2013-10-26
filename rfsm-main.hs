module Main (main) where

import System.Exit
import System.Environment (getArgs)
import System.IO.Unsafe
import Data.Maybe

import Netlist.Read
import RFSM.Gen

usage = do
  putStrLn "Usage: rfsm nr-ins nr-outs nr-state-bits nr-implicants [filename]"
  putStrLn "  Generates a random state machine with the specified number of"
  putStrLn "  inputs, outputs, binary state vector, and implicants in the" 
  putStrLn "  boolean equations. The FSM is output as an hxml formatted file"
  putStrLn "  to the optional filename, or to standard output if filename not"
  putStrLn "  not given."
  exitWith ExitSuccess

undef (Just x) = x
undef Nothing = error "undef: Nothing"

main = do
  args <- getArgs
  let fname = if (length args) == 5 then args !! 4
              else ""
      x = gInt 4 args
  if x == Nothing then usage else therest (undef x) fname
         
therest x f = do
    let rfsm = mkRFSM (x !! 0) (x !! 1) (x !! 2) (x !! 3) 
        nl = mkNetlist rfsm
    storeNetlist nl f
    
gInt :: Int -> [String] -> Maybe [Int]
gInt _ [] = Nothing
gInt 1 (s:_) = case x of
  [] -> Nothing
  [(n, _)] -> Just [n]
  where
    x = reads s :: [(Int,String)]
gInt n (s:ss) = case (x,is) of
  ([],_) -> Nothing
  (_,Nothing) -> Nothing
  ([(n,_)],Just is') -> Just (n:is')
  where
    x = reads s :: [(Int,String)]
    is = gInt (n - 1) ss