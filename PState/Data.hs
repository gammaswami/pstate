module PState.Data 
       ( BoolType (..)
       , ToIndexMap
       , FromIndexMap
       , MbBDD
       , PNetNode (..)
       -- , XNode
       , PNetVec
       -- , XPNetVec
       , AllNet
       , mkAllNet
       )
       where

-- import Data.Boolean
import Data.Boolean.CUDD

import Data.Tuple.All

import qualified Data.Map as D
import Data.Map (keys, partition)

import qualified Data.Vector as V

import Netlist.Data

data BoolType = BAnd | BNand | BOr | BNor | BNot | BXor | BXnor | BReg | BEq
               deriving (Show, Eq)

data SignalType = SIn | SOut | SLocal
                deriving (Show, Eq)
                         
------------------------------------------------------------
-- Map from some type a to index
------------------------------------------------------------
type ToIndexMap a = D.Map a Int

------------------------------------------------------------
-- Map from index to some type a
------------------------------------------------------------
type FromIndexMap a = D.Map Int a

------------------------------------------------------------
-- Map from name string to some type a
------------------------------------------------------------
type NameMap a = D.Map String a

------------------------------------------------------------
-- Map of signal name (string) to the signaltype
------------------------------------------------------------
type SignalMap = NameMap SignalType

------------------------------------------------------------
-- Map of component name to tuple of logic type, output pin name and
-- list of input pin names
------------------------------------------------------------
type CompMap = NameMap (BoolType, String, [String])

------------------------------------------------------------
-- Map of register name to tuple of output and input signal names
------------------------------------------------------------
type RegMap = NameMap (String, String)

------------------------------------------------------------
-- Equivalent to CompMap, used for gate instances and their externally 
-- connected output and input signal names
------------------------------------------------------------
type GateMap = NameMap (BoolType, String, [String])

------------------------------------------------------------
-- Port is signal name and type
------------------------------------------------------------
type Port = (String, SignalType)

------------------------------------------------------------
-- Slot holder for BDD which might be Nothing (e.g. awaiting
-- initialization
------------------------------------------------------------
type MbBDD = Maybe BDD

data PNetNode = Top ( Int -- index of first Out node
                    , Int -- index of first Local node
                    , Int -- index of first Func node
                    , Int -- index of first State node
                    ) |
                -- In name, probability, BDD (bvar for input)
                In String Double MbBDD |
                -- Out name, index in name-map, probability, BDD for signal
                Out String Int Double MbBDD | 
                -- Local name, index in name-map, BDD for signal
                Local String Int MbBDD |
                -- Func gate-name, logic type, output signal name,
                -- list of input signal names, indices of input names in name-map,
                -- probability, BDD for output signal
                Func String BoolType String [String] [Int] Double MbBDD |
                -- State reg-name, output signal name, input signal name,
                -- index of input signal name in name-map, input sig probability,
                -- output signal probability, 
                -- BDD for next-state-function of state bit, 
                -- BDD for state bit (bvar)
                State String String String Int Double Double MbBDD MbBDD
              deriving (Show, Eq)

-- data XNode = XNode { x :: Bool
--                    , v :: Bool 
--                    }
--            deriving (Show, Eq)

-- type XPNetNode = (PNetNode, XNode)

------------------------------------------------------------
-- Vector of PNetNodes is the consolidated netlist
------------------------------------------------------------
type PNetVec = V.Vector PNetNode

-- type XPNetVec = V.Vector XPNetNode

------------------------------------------------------------
-- Consolidated netlist + name-map and inverse name-map
------------------------------------------------------------
type AllNet = (PNetVec, ToIndexMap String, FromIndexMap String)

(!) = (D.!)    -- Map indexing
(!!!) = (V.!)  -- Vector indexing
(+++) = (V.++) -- Vector concatenation

------------------------------------------------------------
-- Turn signal type string into specific type
------------------------------------------------------------
toSignalType :: String -> String -> SignalType
toSignalType _ "in" = SIn
toSignalType _ "out" = SOut
toSignalType _ "local" = SLocal

toSignalType n m = error $ "Unknown mode " ++ m ++ " for signal " ++ n

------------------------------------------------------------
-- Turn gate type string into specific type
------------------------------------------------------------
toBoolType :: String -> BoolType
toBoolType "not" = BNot
toBoolType "and" = BAnd
toBoolType "nand" = BNand
toBoolType "or" = BOr
toBoolType "nor" = BNor
toBoolType "xor" = BXor
toBoolType "xnor" = BXnor
toBoolType "state" = BReg
toBoolType s = error $ "Unknown Logic type " ++ s

------------------------------------------------------------
-- Turn Signal type w/strings into Port tuples
------------------------------------------------------------
toNameTypeList :: Signals -> [Port]
toNameTypeList sl = map f sl
  where f :: Signal -> Port
        f s = (sName s, toSignalType (sName s) (sMode s))

legalTypes = ["std_logic"]

------------------------------------------------------------
-- Remove signals with unknown types 
------------------------------------------------------------
fltLegal :: Signals -> Signals
fltLegal = filter (\s -> elem (sType s) legalTypes)

------------------------------------------------------------
-- Map all in, out and local signal names to their type
------------------------------------------------------------
mkSignalNameMap :: Netlist -> SignalMap
mkSignalNameMap nl = D.fromList $ toNameTypeList $ fltLegal $ (inputs nl) 
                     ++ (outputs nl) ++ (signals nl)

------------------------------------------------------------
-- Find name of singular SOut port in a portlist
------------------------------------------------------------
getOutPort :: [Port] -> String
getOutPort pl = x pl'
  where pl' = [n | (n, m) <- pl, m == SOut]
        x [a] = a
        x _ = error $ "Component with more/less than 1 output"

------------------------------------------------------------
-- Find list of names for input ports in a portlist
------------------------------------------------------------
getInPorts :: [Port] -> [String]
getInPorts pl = [n | (n, m) <- pl, m == SIn]

------------------------------------------------------------
-- Process list of component types (strings) into component map
-- by mapping the f function over list of Component. f picks out 
-- the name, the logic type, and output and input names
------------------------------------------------------------
mkCompNameMap :: Components -> CompMap
mkCompNameMap nlc = D.fromList (map f nlc)
  where f :: Component -> (String, (BoolType, String, [String])) 
        -- REG is treated specifically in order to remove unused pin names
        f (Component {cName = "REG"}) = ("REG", (BReg, "Q", ["D"]))
        f (Component {cName = "REG_N"}) = ("REG_N", (BReg, "Q", ["D"]))
        f c = (cName c, (toBoolType $ cType c, outp, inp))
          where
            p' = toNameTypeList $ cPort c
            outp = getOutPort p'
            inp = getInPorts p'

------------------------------------------------------------
-- Look for pin name s in connection list (port map), and return signal
-- connected to that pin
------------------------------------------------------------
getName :: String -> Connections -> String  
getName s [] = error $ "Port map does not contain port " ++ s
getName s ((Connection {xPort = s', xSignal = n}) : r) 
  | s == s' = n
  | otherwise = getName s r

getQ = getName "Q" -- find reg output in pin list (port map)
getD = getName "D" -- find reg input in port map

------------------------------------------------------------
-- Map f over all list of Register and for each return tuple of 
-- name and the output and input name
------------------------------------------------------------
mkRegNameMap :: Registers -> RegMap
mkRegNameMap nlr = D.fromList (map f nlr)
  where f :: Register -> (String, (String, String))
        f r = (rName r, (getQ conn, getD conn))
          where conn = rPortMap r

------------------------------------------------------------
-- Match an output pin name and list of output pin names in
-- a port map (Connections) and return the name of the signals
-- to each of these as (output, input-list) tuple
------------------------------------------------------------
splitPort :: String -> [String] -> Connections -> (String, [String])
splitPort oport iport ports = (o, i)
  where (o', i) = splitPort' oport iport ports  
        o = x o'
        x [a] = a
        x _ = error $ "More/less than one output in gate"
        splitPort' _ _ [] = ([], [])
        splitPort' oport iport (p:ps) 
          | oport == (xPort p) = ((xSignal p : os'), is')
          | elem (xPort p) iport = (os', (xSignal p : is'))
          | otherwise = (os', is')
            where (os', is') = splitPort' oport iport ps

------------------------------------------------------------
-- Create GateMap by looking up port map for each component type
-- and then splitting signals into output and input names according
-- to the port type
------------------------------------------------------------
mkGateNameMap :: CompMap -> Gates -> GateMap
mkGateNameMap compMap nlg = D.fromList (map f nlg)
  where f :: Gate -> (String, (BoolType, String, [String]))
        f g | gName g == "INV" = ((gComp g) ++ "-i", (btype, out', in'))
            | otherwise = (gComp g, (btype, out', in'))
          where
            (btype, oport, iport) = compMap ! (gName g)
            (out', in') = splitPort oport iport $ gPortMap g

------------------------------------------------------------
-- Aliases are represented as boolean Eq functions between
-- the external and internal signal name
------------------------------------------------------------
mkAliasNameMap :: Aliases -> GateMap
mkAliasNameMap nla = D.fromList (map f nla)
  where f :: Alias' -> (String, (BoolType, String, [String]))
        f (Alias' {aName=a, aInternal=i}) = (a, (BEq, a, [i]))

------------------------------------------------------------
-- Create netlist of all in, out and local signals and create
-- and append the index maps for all the signal names
------------------------------------------------------------
mkPNetSig :: Netlist -> AllNet
mkPNetSig nl = (pNet, D.fromList tim, D.fromList $ map swap tim) 
  where swap (x, y) = (y, x)
        s = mkSignalNameMap nl
        fs v = keys . fst . partition (== v)
        sI = 1 + (length $ fs SIn s)
        sO = sI + (length $ fs SOut s)
        sL = sO + (length $ fs SLocal s)
        tim = zip ((fs SIn s) ++ (fs SOut s) ++ (fs SLocal s)) [1 ..]
        pNet = (V.singleton (Top (sI, sO, sL, 0))) +++
               (V.fromList ((map f1 $ fs SIn s) ++ 
                            (map f2 $ fs SOut s) ++ 
                            (map f3 $ fs SLocal s)))
        -- f1 name = In name 0.5 Nothing
        -- Defer bdd creation until later when we know the preset conditions
        f1 name = case (bddSetProb bdd 0.5) of
                       0 -> In name 0.5 $ Just bdd  
                       _ -> error $ "Cannot create bdd for " ++ name
          where bdd = (bvar name :: BDD)
        f2 name = Out name 0 0.5 Nothing
        f3 name = Local name 0 Nothing

------------------------------------------------------------
-- Make sub netlist of all registers, change Top node indexes
-- to include reg part, and create and append the additional
-- register names index map 
------------------------------------------------------------
mkPNetReg :: Netlist -> AllNet -> AllNet
mkPNetReg nl (net, im, mi) = (net' +++ rNet, im', mi') 
  where r = mkRegNameMap $ state nl
        next = top3 $ net !!! 0
        top3 (Top (_, _, x, _)) = x
        top' = new4 (net !!! 0) (next + (D.size r))
        new4 (Top (x, y, z, _)) v = Top (x, y, z, v)
        net' = net V.// [(0, top')]
        r' = map fr $ D.toList r
        fr (_, (rO, _)) = rO
        im' = D.fromList $ (D.toList im) ++ (zip r' [next ..])
        mi' = D.fromList $ (D.toList mi) ++ (zip [next ..] r')
        rNet = V.fromList $ map frN $ D.toList r
        frN (rName, (rO, rI)) = State rName rO rI 0 0.5 0.5 Nothing $ Just bddi'
          where bddi' = case (bddSetProb bddi 0.5) of
                  0 -> bddi
                  _ -> error $ "Cannot set input prob for " ++ rO
                bddi = (bvar rO :: BDD)

------------------------------------------------------------
-- Use component map to create gate netlist, change Top indexes,
-- append gate netlist to input netlist, and append the gate names
-- index maps. Aliases are treated as gates.
------------------------------------------------------------
mkPNetGate :: Netlist -> AllNet -> AllNet
mkPNetGate nl (net, im, mi) = (net +++ gNet, im', mi')
  where c = mkCompNameMap $ comps nl
        g = mkGateNameMap c $ logic nl
        a = mkAliasNameMap $ alias nl
        next = top4 $ net !!! 0
        top4 (Top (_, _, _, x)) = x
        g' = map gr $ (D.toList g) ++ (D.toList a)
        gr (_, (_, gO, _)) = gO
        im' = D.fromList $ (D.toList im) ++ (zip g' [next ..])
        mi' = D.fromList $ (D.toList mi) ++ (zip [next ..] g')
        gNet = V.fromList $ map frG $ (D.toList g) ++ (D.toList a)
        frG (gName, (gT, gO, gI)) = Func gName gT gO gI [] 0.5 Nothing  

------------------------------------------------------------
-- Run through AllNet and find and update indexes in netlist
-- nodes according to complete name-index maps
------------------------------------------------------------
mkIndexNet :: AllNet -> AllNet
mkIndexNet (net, im, mi) = (net', im, mi)
  where ix = (!) im
        ixs = map ix
        net' = V.map fix net
        fix (Top ints) = Top ints
        fix (In s f bdd) = In s f bdd
        fix (Out name _ p bdd) = Out name (ix name) p bdd
        fix (Local name _ bdd) = Local name (ix name) bdd
        fix (State name nOut nIn _ pi po bddo bddi) = 
          State name nOut nIn (ix nIn) pi po bddo bddi
        fix (Func name bT nOut nIn _ p bdd) = Func name bT nOut nIn (ixs nIn) p bdd
        
------------------------------------------------------------
-- Put it all together! Turns parsed netlist (from hxml) into
-- indexed AllNet index structure, and returns it in the IO monad
-- for convenience at top level parse loop.
------------------------------------------------------------
mkAllNet :: Netlist -> IO AllNet
mkAllNet nl = return net
  where ms = mkPNetSig nl
        mr = mkPNetReg nl ms
        mg = mkPNetGate nl mr
        net = mkIndexNet mg
