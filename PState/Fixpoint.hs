module PState.Fixpoint where

import PState.Data
import PState.BDD

import Data.Boolean.CUDD

import Data.List
import qualified Data.Vector as V

import Text.Printf
import System.IO
import Control.DeepSeq

import Foreign.C

-- Data structure for keeping BDD's and probabilities while fix point loop'ing

data Fix = Fix { bddo :: BDD
               , bddi :: BDD
               , pnow :: Double
               , pnext :: Double
               , fixed :: Bool
               } deriving (Show, Eq)
     
instance NFData Fix

type FixL = [Fix]

mkFixlist :: AllNet -> FixL
mkFixlist (pv, _, _) = mkfl $ V.toList pv
  where mkfl [] = []
        mkfl ((State _ _ _ _ _ pi bo bi) : fs) = 
          Fix { bddo = defBDD bo
              , bddi = defBDD bi
              , pnow = pi
              -- , pnow = realToFrac pi
              , pnext = bddGetProb $ defBDD bo
              -- , pnext = realToFrac $ bddGetProb $ defBDD bo
              , fixed = False
              } : mkfl fs
        mkfl (_ : fs) = mkfl fs
        
chLimit :: Double -> Double -> Fix -> Bool
chLimit limH limL (Fix {pnow = pn, pnext = px}) = 
  px == pn || ((px' >= limL * pn) && (px' <= limH * pn))
  where
    px' = px * 1000000.0
        
calcNewFix :: Fix -> Fix
calcNewFix f@Fix {bddo = bo, bddi = bi, pnext = px} = 
  f {pnow = px, pnext = (p' :: Double)}
  where p' = case (bddSetProb bi px) of
          0 -> bddGetProb bo
          _ -> error "Cannot set state probability"

-- lim(it) for checking in ppm          
chFixList :: Double -> FixL -> Bool
chFixList lim = foldr (\f t -> chLimit limH limL f && t) True
  where limH = 1000000.0 + lim
        limL = 1000000.0 - lim
        
calcFixList :: FixL -> FixL
calcFixList = map calcNewFix
        
iterOne :: Int -> Int -> Int -> FixL -> IO (Bool, Int, FixL)
iterOne 0 max lim fl =  
  do printf "Trying %d iterations at %d ppm accuracy.\n" max lim
     printf "Iteration: %12d" (0 :: Int)
     hFlush stdout
     iterOne 1 max lim fl

iterOne iter max lim fl | iter == max = 
  do printf "\nNo Fixpoint for %d iterations at %d ppm accuracy.\n" max lim 
     hFlush stdout
     return (False, max, fl)
  
iterOne iter max lim fl | (chFixList (fromIntegral lim) fl) =
  do printf "\nFixpoint found after %d iterations with %d ppm accuracy.\n"
       iter lim
     hFlush stdout
     return (True, iter, fl)
  
iterOne iter max lim fl | otherwise = 
  -- let fl' = calcFixList fl in 
  do putStr "\8\8\8\8\8\8\8\8\8\8\8\8" 
     printf "%12d" iter
     -- printPlist fl
     hFlush stdout
     -- printf "Iteration %12d:\n" iter
     iterOne (iter + 1) max lim $!! calcFixList fl    
              
iterFixList :: Int -> [Int] -> FixL -> IO Bool
iterFixList _ [] _ = return False

iterFixList max (lim:lims) fl =
  do 
    (b, _, _) <- iterOne 0 max lim fl
    if b then return True
      else iterFixList max lims fl
      
printP :: Fix -> IO ()
printP (Fix {pnow = p}) =
  do print $ isInfinite p
     printf " %9.8g, " p
     
printPlist :: FixL -> IO ()
printPlist fl = 
  do mapM_ printP fl  
     printf "\n"


printState :: PNetNode -> IO ()
printState (State regName sigName _ _ _ _ bddo _) =
  let p = bddGetProb $ defBDD bddo in
  do printf "%s %s " regName sigName
     -- print bddo
     -- print $ isInfinite p
     printf " %9.8g\n" p
     
printState _ = return ()
      
printOut :: PNetNode -> IO ()
printOut (Out n _ _ bdd) = 
  let p = bddGetProb $ defBDD bdd in
  do printf "%s: %9.8g\n" n p
     
printOut _ = return ()
      
printResult :: AllNet -> IO ()
printResult (pv, _, _) =
  do 
    varIndices
    putStr "STATE PROBABILITIES:\n"
    V.mapM_ printState pv
    putStr "OUTPUT PROBABILITIES:\n"
    V.mapM_ printOut pv
     