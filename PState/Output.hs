module PState.Output where

import PState.Data
import PState.BDD
import PState.Options
import qualified Data.Vector as V
import Data.Boolean.CUDD
import Text.Printf

textInsig :: PNetNode -> String
textInsig (In n p _) =
  printf "%s %9.8g\n" n p

textInsig _ = ""

textOutsig :: PNetNode -> String
textOutsig (Out n _ _ bdd) = 
  let p = bddGetProb $ defBDD bdd in
  printf "%s %9.8g\n" n p
     
textOutsig _ = ""

textStates :: Bool -> Bool -> PNetNode -> String
textStates nosig inst (State regName sigName _ _ _ _ bddo _) = 
  let p = bddGetProb $ defBDD bddo in
  foldr (++) "\n"
  [ if inst then printf "%s " regName else ""
  , if not nosig then printf "%s " sigName else ""
  , printf "%9.8g" p
  ]  

textStates _ _ _ = ""

textOutput :: AllNet -> Options -> String
textOutput (nl, _, _) opt =
  let 
    (Top (i,o,f,s)) = nl V.! 0 
    nll = V.toList nl 
  in
   foldr (++) ""
   [ show (i - 1) ++ "\n" 
   , foldr (++) "" $ map textInsig nll
   , show (o - i) ++ "\n"
   , foldr (++) "" $ map textOutsig nll
   , show (s - f) ++ "\n"
   , foldr (++) "" $ map (textStates (optNosignal opt) (optInstance opt)) nll
   ]