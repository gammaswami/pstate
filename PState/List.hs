module PState.List where

import PState.Data

data Preset = { prob :: Double
              , high :: Bool
              , low  :: Bool
              }
              
type PresetMap = NameMap Preset 