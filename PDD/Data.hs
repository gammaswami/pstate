module PDD.Data where

import qualified Data.Map.Strict as M

import Data.Boolean.CUDD

type PDDMap = M.Map BDD Double 

data PDD = Node BDD PDD PDD | 
           Sum PDD PDD |
           One Double | 
           Zero
         deriving (Show)

evalP :: PDDMap -> PDD -> Double
evalP _ (One p) = p
evalP _ Zero = 0.0
evalP pddm (Sum pddT pddF) = (evalP pddm pddT) + (evalP pddm pddF)
evalP pddm (Node pv pddT pddF) = pp * (evalP pddm pddT) 
                                  + (1.0 - pp) * (evalP pddm pddF) 
  where
    pp = pddm M.! pv
    
reduceP :: PDDMap -> PDD -> PDD
reduceP pm pdd = red' 1.0 pm pdd 
  where
    red' :: Double -> PDDMap -> PDD -> PDD
    red' _ _ Zero = Zero
    red' pr _ (One p) = One (p * pr)
    red' pr pm (Sum pddT pddF) = Sum (red' pr pm pddT) (red' pr pm pddF)
    red' pr pm (Node pv pddT pddF) = case M.lookup pv pm of
      Nothing -> Node pv (red' pr pm pddT) (red' pr pm pddF)
      Just p -> Sum (red' (pr * p) pm pddT) (red' (pr * (1.0 - p)) pm pddF)
    
bddTopdd :: BDD -> PDD
bddTopdd bdd 
  | bdd == true = One 1.0
  | bdd == false = Zero
  | otherwise = Node (bif bdd) (bddTopdd $ bthen bdd) (bddTopdd $ belse bdd)

ab :: PDDMap
ab = M.fromList [(bvar "a" :: BDD, 0.5), (bvar "b" :: BDD, 0.9)]