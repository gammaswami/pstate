module Netlist.Read where

import Netlist.Data
import Text.XML.HXT.Core
import System.IO.Unsafe

import Text.XML.HXT.Arrow.Pickle

-- import PState.Data

instance XmlPickler Netlist where
  xpickle = xpNetlist

-- instance XmlPickler SMode where
--   xpickle = xpSMode

instance XmlPickler Signal where
  xpickle = xpSignal

-- instance XmlPickler BoolType where
--   xpickle = xpBoolType

instance XmlPickler Component where
  xpickle = xpComponent

instance XmlPickler Register where
  xpickle = xpRegister

instance XmlPickler Connection where
  xpickle = xpConnection

instance XmlPickler Gate where
  xpickle = xpGate

instance XmlPickler Alias' where
  xpickle = xpAlias
  
-- And the pickler implementations:

xpNetlist :: PU Netlist
xpNetlist
    = xpElem "netlist" $
      xpWrap ( \ ((e,a,i,o,s,c,st,l,al)) -> Netlist e a i o s c st l al,
               \ t -> ( entity t, arch t,
                        inputs t, outputs t,
                        signals t, comps t,
                        state t, logic t,
                        alias t 
                      )
             ) $
      xp9Tuple (xpAttr "entity" xpText)
               (xpAttr "architecture"   xpText)
               xpInputs
               xpOutputs
               xpSignals
               xpComponents
               xpRegisters
               xpGates
               xpAliases
               
xpInputs :: PU Inputs
xpInputs
  = xpElem "inputs" $
    -- xpWrap ( fromList,
    --          toList ) $
    xpList $
    xpSignal

xpOutputs :: PU Outputs
xpOutputs
  = xpElem "outputs" $
    -- xpWrap ( fromList,
    --          toList ) $
    xpList $
    xpSignal

xpSignals :: PU Signals
xpSignals
  = xpElem "signals" $
    -- xpWrap ( fromList,
    --          toList ) $
    xpList $
    xpSignal

xpSignal :: PU Signal
xpSignal
  = xpElem "signal" $
    xpWrap ( uncurry3 Signal,
             \s -> ( sName s, sType s, sMode s )
           ) $
    xpTriple (xpAttr "name" xpText)
             (xpAttr "type" xpText)
             (xpAttr "mode" xpText)

xpComponents :: PU Components
xpComponents
  = xpElem "components" $
    -- xpWrap ( fromList,
    --          toList ) $
    xpList $
    xpComponent

xpComponent :: PU Component
xpComponent
  = xpElem "component" $
    xpWrap ( \ ((n,b,i,o,p)) -> Component n b i o p,
             \c -> ( cName c, cType c, cNrIn c, cNrOut c, cPort c )
           ) $
    xp5Tuple (xpAttr "name"       xpText)
             (xpAttr "type"       xpText)
             (xpAttr "nr-inputs"  xpPrim)
             (xpAttr "nr-outputs" xpPrim)
             xpPort

xpPort :: PU Signals
xpPort
  = xpElem "port" $
    -- xpWrap ( fromList,
    --          toList ) $
    xpList $
    xpSignal

xpRegisters :: PU Registers
xpRegisters
  = xpElem "state" $
    -- xpWrap ( fromList,
    --          toList ) $
    xpList $
    xpRegister

xpRegister :: PU Register
xpRegister
  = xpElem "register" $
    xpWrap ( uncurry3 Register,
             \r -> ( rType r, rName r, rPortMap r )
           ) $
    xpTriple (xpAttr "type" xpText)
             (xpAttr "name" xpText)
             xpConnections

xpConnections :: PU Connections
xpConnections
  = xpElem "portmap" $
    -- xpWrap ( fromList,
    --          toList ) $
    xpList $
    xpConnection

xpConnection :: PU Connection
xpConnection
  = xpElem "connection" $
    xpWrap ( uncurry Connection,
             \x -> ( xPort x, xSignal x )
           ) $
    xpPair (xpAttr "port"   xpText)
           (xpAttr "signal" xpText)

xpGates :: PU Gates
xpGates
  = xpElem "logic" $
    -- xpWrap ( fromList,
    --          toList ) $
    xpList $
    xpGate

xpGate :: PU Gate
xpGate
  = xpElem "gate" $
    xpWrap ( uncurry3 Gate,
             \g -> ( gName g, gComp g, gPortMap g )
           ) $
    xpTriple (xpAttr "type" xpText)
             (xpAttr "name" xpText)
             xpConnections

xpAliases :: PU Aliases
xpAliases
  = xpElem "aliases" $
    -- xpWrap ( fromList,
    --          toList ) $
    xpList $
    xpAlias

xpAlias :: PU Alias'
xpAlias
  = xpElem "alias" $
    xpWrap ( uncurry Alias',
             \a -> ( aName a, aInternal a )
           ) $
    xpPair (xpAttr "name"   xpText)
           (xpAttr "internal" xpText)

loadNetlist :: String -> IO Netlist
loadNetlist file
  = do
      [p2] <- runX
            ( xunpickleDocument xpNetlist
              [ withRemoveWS yes   -- remove redundant whitespace
              , withTrace 0
              , withValidate no    -- don't validate source
              ] file
              -- >>>
              -- arrIO ( \ x -> do {print x; return x})            
            )
      return p2
      
storeNetlist :: Netlist -> String -> IO ()
storeNetlist nl file
  = do 
    runX ( constA nl
           >>>
           xpickleDocument xpNetlist
           [ withIndent yes        -- indent XML
           ] file
         )
    return ()
