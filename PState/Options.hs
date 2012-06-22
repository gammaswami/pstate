module PState.Options ( Options(..) 
                      , defaultOptions
                      , processOptions
                      ) where

import System.Exit
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Char
import Data.Maybe( fromMaybe )
import Data.List.Split

processOptions =
  do
    args <- getArgs
    let ( actions, nonOpts, msgs ) = getOpt Permute options args
    opts <- foldl (>>=) (return defaultOptions) actions
    return (opts, nonOpts) 

data OptListing = InS | OutS | StateS | AllS deriving (Show)

toOptListing :: String -> OptListing
toOptListing "in" = InS
toOptListing "out" = OutS
toOptListing "state" = StateS
toOptListing "all" = AllS
toOptListing x = error $ "Wrong option argument " ++ x ++ " for --list option"

data Options = Options { optInput    :: [String] -- optInput    :: IO String
                       , optOutput   :: String -> IO ()
                       , optOutfile  :: Bool
                       , optList     :: [OptListing]
                       , optPattern  :: [String]
                       , optInstance :: Bool
                       , optNosignal :: Bool
                       , optHigh     :: [String] 
                       , optLow      :: [String] 
                       , optPreset   :: Double
                       , optProb     :: [(Double, [String])]
                       , optRestrict :: [String] 
                       , optExclude  :: [String] 
                       }

defaultOptions :: Options
defaultOptions = Options { optInput  = []
                         , optOutput = putStr
                         , optOutfile = False
                         , optList = []
                         , optPattern = []
                         , optInstance = False
                         , optNosignal = False
                         , optHigh = []
                         , optLow = []
                         , optPreset = 0.5
                         , optProb = []
                         , optRestrict = []
                         , optExclude = []
                         }

options :: [OptDescr (Options -> IO Options)]
options = 
  [ 
    Option ['h'] ["help"]        (NoArg showUsage) "Show this info."
  , Option ['?'] []              (NoArg showUsage) "Show this info."
  , Option ['V'] ["version"]     (NoArg showVersion)         
    "Show version number"
  , Option ['i'] ["input"]       (ReqArg readInput "FILE")   
    "HXML input file to read"    
  , Option ['o'] ["output"]      (ReqArg writeOutput "FILE") 
    "Output file for probability results"
  , Option ['l'] ["list"]        (OptArg listType "TYPE")    
    "List in, out, and/or state signals"
  , Option ['s'] ["pattern"]     (ReqArg patternList "PATTERNS")
    "List (by -l option) only those signals that matches list of patterns"
  , Option ['I'] ["instance"]    (NoArg setInst) 
    "List instance name of register in addition to signal name"
  , Option ['N'] ["nosignal"]    (NoArg setSig) 
    "Do not list signal name of register, only instance name"
  , Option ['H'] ["high"]        (ReqArg setHList "SIGNALS")  
    "Set signals in comma separated list to logic 1"
  , Option ['L'] ["low"]         (ReqArg setLList "SIGNALS")  
    "Set signals in comma separated list to logic 0"
  , Option ['P'] ["probability"] (ReqArg setPList "PROB:SIGNALS")  
    "Set probability for signals in comma separated list to first arg in list"
  , Option ['p'] ["preset"]      (ReqArg setPreset "PROBABILITY")
    "Probability will be preset to this value except for signals in -P option list"
  , Option ['O'] ["only"]        (ReqArg setOList "SIGNALS")  
    "Limit computation to those states that influence specific output signals in list"
  , Option ['X'] ["exclude"]     (ReqArg setXList "SIGNALS")  
    "Exclude states that only influence signals in list"
  ]

showVersion _ = do
  putStrLn "pstate 0.1.3"
  exitWith ExitSuccess

showUsage _ = do
  putStrLn $ usageInfo "pstate -- compute post-synthesis state probabilities" options
  exitWith ExitSuccess  
  
-- readInput arg opt = return opt { optInput = readFile arg }
readInput arg opt = return opt { optInput = optInput opt ++ [arg] }
-- writeOutput arg opt = return opt { optOutput = writeFile arg }
writeOutput arg opt = return opt { optOutput = writeFile arg 
                                 , optOutfile = True }

listType Nothing opt = return opt { optList = [AllS] } 
listType (Just arg) opt = 
  return opt { optList = map toOptListing $ splitOneOf ";," arg }

patternList arg opt = 
  return opt { optPattern = optPattern opt ++ splitOneOf ",;" arg }

setInst opt = return opt { optInstance = True }

setSig opt = return opt { optNosignal = True }

setHList arg opt = 
  return opt { optHigh = optHigh opt ++ splitOneOf ";," arg }

setLList arg opt = 
  return opt { optLow = optLow opt ++ splitOneOf ";," arg }

setPreset arg opt = let d = if (doubleCheck arg) then read arg
                            else error "-p argument must be a real number"
                    in
                     return opt { optPreset = d }

setPList arg opt = 
  let (dl : ll) = splitOneOf ":" arg
      d = if (doubleCheck dl) then read dl 
          else error "Problist should be prob:s1,s2,..."
  in
   return opt { optProb = optProb opt ++ [(d, splitOneOf ";," $ head ll)] }

doubleCheck :: String -> Bool
doubleCheck "" = False
doubleCheck "." = False
doubleCheck s = doubleCheck' s False
  where
    doubleCheck' [] _ = True 
    doubleCheck' ('.' : _) True = False
    doubleCheck' ('.' : rest) False = doubleCheck' rest True
    doubleCheck' (d : rest) o = isDigit d && doubleCheck' rest o

setOList arg opt = return opt { optRestrict = optRestrict opt ++ splitOneOf ";," arg }

setXList arg opt = return opt { optExclude = optExclude opt ++ splitOneOf ";," arg }