module Test where

-- import Data.Boolean
import Data.Boolean.CUDD

testIt = 
  let a = (bvar "a" :: BDD)
      b = (bvar "b" :: BDD)
      and1 = a /\ (neg b)
      and2 = (neg a) /\ b
      sum = and1 \/ and2
      carry = a /\ b in
  do
    print $ bddSetProb a 0.5
    print $ bddSetProb b 0.5
    print $ bddGetProb a
    print $ bddGetProb b
    print $ bddGetProb and1
    print $ bddGetProb and2
    print $ bddGetProb sum
    print $ bddGetProb carry
    return ()
  