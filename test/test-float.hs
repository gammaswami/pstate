module Main () where
       
import Text.Printf
import System.IO


fixu :: (Float -> Float) -> Float -> IO Float
fixu f x = fixu' f (f x) x
    
fixu' :: (Float -> Float) -> Float -> Float -> IO Float
fixu' f x' x =
  do
    printf "%9.8g\n" x
    if x == x' then return x else fixu' f (f x') x'

-- main = fixu (/ 3.0) 1.0
  
       