module RFSM.Gen where

import Data.Vector
import Data.Bits
import System.Random
import System.IO.Unsafe

-- import Netlist.Data
-- import Netlist.Read
-- import PState.Data

type Term = Vector Int
type Plane = Vector Term

data RFSM = RFSM { nrI :: Int -- nr of inputs
                     , nrO :: Int -- nr of outputs
                     , nrS :: Int -- nr of states
                     , nrX :: Int -- nr of implicants
                     , sAND :: Int -- size of AND term in Ints
                     , sOR :: Int -- size of OR term in Ints
                     , saP :: Int -- size of AND plane
                     , soP :: Int -- size of OR plane
                     , aPlane :: Plane -- AND terms
                     , oPlane :: Plane -- OR terms
                     } deriving (Show, Eq)
                           
myR :: Int -> Int
myR x = unsafePerformIO $ randomIO

inner :: Int -> Int -> Term
inner n m = generate n myR

outter :: Int -> Int -> Plane
outter x y = generate x (inner y)

mkRFSM :: Int -> Int -> Int -> Int -> RFSM
mkRFSM i o s x = RFSM { nrI = i
                      , nrO = o
                      , nrS = s
                      , nrX = x
                      , sAND = vISize
                      , sOR = vOSize
                      , saP = x
                      , soP = s + o
                      , aPlane = outter x vISize
                      , oPlane = outter (s + o) vOSize
                      }
  where
    vISize = ((i + s) `div` 32) + 1
    vOSize = (x `div` 64) + 1

