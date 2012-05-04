module Test where

import Data.Boolean.CUDD
 
testIt = do
  bddSetProb (bvar "a" :: BDD) 0.5
  bddSetProb (bvar "b" :: BDD) 0.5
  and1 <- (bvar "a" :: BDD) /\ neg (bvar "b" :: BDD)
  and2 <- neg (bvar "a" :: BDD) /\ (bvar "b" :: BDD)
  sum <- and1 \/ and2
  carry <- (bvar "a" :: BDD) /\ (bvar "b" :: BDD)
  print $ bddGetProb and1
  print $ bddGetProb and2
  print $ bddGetProb sum
  print $ bddGetProb carry
  return ()
  