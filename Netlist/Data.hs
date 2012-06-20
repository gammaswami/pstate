module Netlist.Data where

import Data.Map (Map, fromList, toList)
 
data Netlist = Netlist
               { entity  :: String
               , arch    :: String
               , inputs  :: Inputs
               , outputs :: Outputs
               , signals :: Signals
               , comps   :: Components
               , state   :: Registers
               , logic   :: Gates
               , alias   :: Aliases
               }
             deriving (Show, Eq)

type Inputs = [Signal]

type Outputs = [Signal]

type Signals = [Signal]

-- data SMode = In | Out | Local 
--            deriving (Show, Eq)

data Signal = Signal
              { sName  :: String
              , sType  :: String
              -- , sMode  :: SMode
              , sMode  :: String
              }
            deriving (Show, Eq)
                     
type Components = [Component]

-- data BoolType = And | Nand | Or | Nor | Not
--               deriving (Show, Eq)

data Component = Component
                 { cName  :: String
                 -- , btype :: BoolType
                 , cType :: String
                 , cNrIn  :: Int
                 , cNrOut :: Int
                 , cPort  :: Signals
                 }
               deriving (Show, Eq)
                        
-- type Port = [Signal]
                        
type Registers = [Register]

data Register = Register
                { rType    :: String
                , rName    :: String
                , rPortMap :: Connections
                }
              deriving (Show, Eq)
                       
type Connections = [Connection]

data Connection = Connection
                  { xPort   :: String
                  , xSignal :: String
                  }
                deriving (Show, Eq)
                         
type Gates = [Gate]

data Gate = Gate
            { gName    :: String
            , gComp    :: String
            , gPortMap :: Connections
            }
          deriving (Show, Eq)
                   
type Aliases = [Alias']
               
data Alias' = Alias'
             { aName     :: String
             , aInternal :: String
             }
           deriving (Show, Eq)