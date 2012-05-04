module Main (main) where

import Control.Monad
import System.Environment (getArgs)
import System.IO.Unsafe
import Data.Boolean.CUDD
import NetlistData
import NetlistRead (loadNetlist)
import PStateData
import PStateBDD 
import Data.Vector ((!), forM_, toList)

-- testit :: Netlist -> IO PNetVec
testit n = 
  let ma = mkAllNet n
      an = mkBDDNet ma
  in
   (ma,an)
  
showNode (ix, nn) = do
  putStr $ show $ ix
  putStr ": "
  putStr $ show $ map bddGetProb $ getBDD nn
  putStr "\n"

main =
  let args = unsafePerformIO getArgs
      nl = unsafePerformIO $ loadNetlist $ head args
      (ma,(pv,im,mi)) = testit nl
      pvl = zip [0 ..] $ toList pv
  in
   do
     -- print im
     -- print mi
     print ma
     mapM showNode pvl
     return ()