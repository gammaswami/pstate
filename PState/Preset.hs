module PState.Preset ( Preset
                     , PresetMap
                     , processPresets
                     , isPreset
                     ) where

import PState.Data
import PState.Options

import qualified Data.Map as M
import qualified Data.Vector as V

data Preset = Prob Double | High | Low
              
type PresetMap = NameMap Preset

isPreset :: PresetMap -> String -> Maybe Preset 
isPreset pm name = M.lookup pm name

processPresets :: Allnet -> Options -> PresetMap
processPresets (nl, _, _) opt = M.fromList $ map eachSig' V.toList nl
  where
    eachSig' = eachSig (optHigh opt) (optLow opt) (optProb opt)
    
eachSig :: [String] -> [String] -> ProbList -> PNetNode -> (String, Preset)
eachSig hi lo pro (In name _ _) = case (eachOpt hi, eachOpt lo, eachProb pro) of
  { (True, _, _) -> (name, High)
  ; (_, True, _) -> (name, Low)
  ; (_, _, x)    -> (name, Prob x)
  }
                                  


    
