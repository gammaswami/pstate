module Main (main) where
       
import Text.Printf
import System.IO

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "div3.h div3"
  div3 :: CDouble -> CDouble

fixu :: (Double -> Double) -> Double -> IO Double
fixu f x = fixu' f (f x) x
    
fixu' :: (Double -> Double) -> Double -> Double -> IO Double
fixu' f x' x =
  do
    printf "%9.8g\n" x
    if x == x' then return x else fixu' f (f x') x'

d3hs = realToFrac . div3 . realToFrac

main = fixu d3hs 1.0
  
       