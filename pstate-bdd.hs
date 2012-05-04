module PStateBDD ( mkBDDNet
                 , getBDD 
                 , defBDD
                 ) where

import Data.Boolean.CUDD

import Data.Tuple.All

import qualified Data.Map as M

import qualified Data.Vector as V

import PStateData

------------------------------------------------------------
-- DelayNode datatype for storing info about state nodes prior values
-- and propagating them towards inputs
------------------------------------------------------------
data DelayNode = DN { name :: String
                    , ix :: Int
                    , delay :: Int 
                    }
                 
------------------------------------------------------------                 
-- Make Vector lookup and append available as !!! and +++
------------------------------------------------------------
(!!!) = (V.!)
(+++) = (V.++)
                 
------------------------------------------------------------        
-- mkBDDNet creates all the bdds in the AllNet structure
-- stateProb0: Initial priming of state variable probability
-- (net,im,mi): AllNet structure in
-- Return: AllNet structure with all bdds generated
------------------------------------------------------------
mkBDDNet :: Double -> AllNet -> AllNet
mkBDDNet stateProb0 (net, im, mi) = (net', im, mi)
-- im and mi maps are unchanged
-- net' is new PNetVec part of AllNet with bdds inserted
-- eachN maps over each PNetNode in net Vector and creates bdds one by one
  where net' = V.map (eachN stateProb0 net) net
        
------------------------------------------------------------
-- eachN: Called once for every PNetNode to either prime bdd or create
-- it
------------------------------------------------------------        
eachN :: Double -> PNetVec -> PNetNode -> PNetNode

-- Top node: Do nothing, return unchanged
eachN _ _ n@(Top i) = n

-- In node: Set probability 0.5 for input bdd, raise error if can't be
-- set
eachN _ _ n@(In s f bdd) = case (bddSetProb bdd' 0.5) of 
  -- return n unchanged since bdd is changed (unsafely) in "C" runtime
  -- library
  0 -> n
  _ -> error $ "Cannot set probability on input " ++ s
-- test if bdd exists, which it should
  where bdd' = case bdd of
          (Just x) -> x
          Nothing -> error "Cannot set prob on non-existing bdd for " s

-- Create output node bdd if it does not exist
eachN _ net (Out s ix f Nothing) = Out s ix f $ crBDD net $ net !!! ix

-- Output node bdd exists
eachN _ net n@(Out _ _ _ _) = n

-- Local wire: create bdd if it does not exist
eachN _ net (Local s ix Nothing) = Local s ix $ crBDD net $ net !!! ix

-- or just return if it exists already
eachN _ net n@(Local _ _ _) = n

-- 
eachN sP0 net (State sO sI ix po pi Nothing bddi) = 
  let bdd' = crBDD net $ net !!! ix in
  case (bddSetProb (defBDD bdd') sP0) of
    0 -> State sO sI ix po pi bdd' bddi
    _ -> error $ "Cannot set prob for output bdd in " ++ sO
  
eachN _ net n@(State _ _ _ _ _ _ _) = n
eachN _ net (Func bt sO sI ixI p Nothing) = Func bt sO sI ixI p $ crBDDf net bt ixI
eachN _ net n@(Func _ _ _ _ _ _) = n
                
crBDD :: PNetVec -> PNetNode -> MbBDD
crBDD _ (Top _) = error "Top node where it should not be!"
-- crBDD _ (Out sname _ _ _) = error "Out node  where it should not be!"
crBDD net (Out _ ix _ Nothing) = crBDD net $ net !!! ix 
crBDD _ (Out _ _ _ bdd) = bdd
crBDD _ (In s _ bdd) | bdd == Nothing = error "BDD for In node not created?"
                     | otherwise = bdd
crBDD net (Local _ ix Nothing) = crBDD net $ net !!! ix    
crBDD _ (Local _ _ bdd) = bdd
crBDD net (State sO _ _ _ _ _ bddi ) 
  | bddi == Nothing = error "BDD in State node not created?"
  | otherwise = bddi
crBDD net (Func bt _ _ ixI _ Nothing) = crBDDf net bt ixI
crBDD net (Func bt _ _ ixI _ bddi) = bddi

defBDD :: MbBDD -> BDD
defBDD Nothing = error "Def'ing a non-existing BDD"
defBDD (Just bdd) = bdd

crBDDf :: PNetVec -> BoolType -> [Int] -> MbBDD
crBDDf net BReg _ = error "Should be state, not Reg"
crBDDf net BEq ix = crBDD net $ net !!! (head ix)
crBDDf net BNot ix = Just $ neg $ defBDD $ crBDD net $ net !!! (head ix) 
crBDDf net BAnd ix = Just $ conjoin $ bDDlist net ix
crBDDf net BNand ix = Just $ neg $ conjoin $ bDDlist net ix
crBDDf net BOr ix = Just $ disjoin $ bDDlist net ix
crBDDf net BNor ix = Just $ neg $ disjoin $ bDDlist net ix
crBDDf net BXor ix = Just $ foldr xor false $ bDDlist net ix
crBDDf net BXnor ix = Just $ neg $ foldr xor false $ bDDlist net ix
  
bDDlist net ix = map (\i -> defBDD $ crBDD net $ (!!!) net i) ix

getBDD :: PNetNode -> [BDD]
getBDD (Top _) = []
getBDD (Out _ _ _ bdd) = [defBDD bdd]
getBDD (In _ _ bdd) = [defBDD bdd]
getBDD (Local _ _ bdd) = [defBDD bdd]
getBDD (State _ _ _ _ _ bddo bddi) = [defBDD bddo, defBDD bddi]
getBDD (Func _ _ _ _ _ bdd) = [defBDD bdd]