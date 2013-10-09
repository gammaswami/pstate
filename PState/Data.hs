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

type ToIndexMap a = D.Map a Int
type FromIndexMap a = D.Map Int a
type NameMap a = D.Map String a
type SignalMap = NameMap SignalType
type CompMap = NameMap (BoolType, String, [String])
type RegMap = NameMap (String, String)
type GateMap = NameMap (BoolType, String, [String])

type Port = (String, SignalType)

type MbBDD = Maybe BDD

data PNetNode = Top (Int, Int, Int, Int) |
                In String Double MbBDD |
                Out String Int Double MbBDD | 
                Local String Int MbBDD |   
                Func String BoolType String [String] [Int] Double MbBDD |
                State String String String Int Double Double MbBDD MbBDD
              deriving (Show, Eq)

-- data XNode = XNode { x :: Bool
--                    , v :: Bool 
--                    }
--            deriving (Show, Eq)

-- type XPNetNode = (PNetNode, XNode)

type PNetVec = V.Vector PNetNode

-- type XPNetVec = V.Vector XPNetNode

type AllNet = (PNetVec, ToIndexMap String, FromIndexMap String)

(!) = (D.!)
(!!!) = (V.!)
(+++) = (V.++)

toSignalType :: String -> String -> SignalType
toSignalType _ "in" = SIn
toSignalType _ "out" = SOut
toSignalType _ "local" = SLocal

toSignalType n m = error $ "Unknown mode " ++ m ++ " for signal " ++ n

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

toNameTypeList :: Signals -> [Port]
toNameTypeList sl = map f sl
  where f :: Signal -> Port
        f s = (sName s, toSignalType (sName s) (sMode s))

legalTypes = ["std_logic"]

fltLegal :: Signals -> Signals
fltLegal = filter (\s -> elem (sType s) legalTypes)

mkSignalNameMap :: Netlist -> SignalMap
mkSignalNameMap nl = D.fromList $ toNameTypeList $ fltLegal $ (inputs nl) 
                     ++ (outputs nl) ++ (signals nl)

getOutPort :: [Port] -> String
getOutPort pl = x pl'
  where pl' = [n | (n, m) <- pl, m == SOut]
        x [a] = a
        x _ = error $ "Component with more/less than 1 output"

getInPorts :: [Port] -> [String]
getInPorts pl = [n | (n, m) <- pl, m == SIn]

mkCompNameMap :: Components -> CompMap
mkCompNameMap nlc = D.fromList (map f nlc)
  where f :: Component -> (String, (BoolType, String, [String])) 
        f (Component {cName = "REG"}) = ("REG", (BReg, "Q", ["D"]))
        f (Component {cName = "REG_N"}) = ("REG_N", (BReg, "Q", ["D"]))
        f c = (cName c, (toBoolType $ cType c, outp, inp))
          where
            p' = toNameTypeList $ cPort c
            outp = getOutPort p'
            inp = getInPorts p'

getName :: String -> Connections -> String  
getName s [] = error $ "Port map does not contain port " ++ s
getName s ((Connection {xPort = s', xSignal = n}) : r) 
  | s == s' = n
  | otherwise = getName s r

getQ = getName "Q"
getD = getName "D"

mkRegNameMap :: Registers -> RegMap
mkRegNameMap nlr = D.fromList (map f nlr)
  where f :: Register -> (String, (String, String))
        f r = (rName r, (getQ conn, getD conn))
          where conn = rPortMap r

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

mkGateNameMap :: CompMap -> Gates -> GateMap
mkGateNameMap compMap nlg = D.fromList (map f nlg)
  where f :: Gate -> (String, (BoolType, String, [String]))
        f g | gName g == "INV" = ((gComp g) ++ "-i", (btype, out', in'))
            | otherwise = (gComp g, (btype, out', in'))
          where
            (btype, oport, iport) = compMap ! (gName g)
            (out', in') = splitPort oport iport $ gPortMap g

mkAliasNameMap :: Aliases -> GateMap
mkAliasNameMap nla = D.fromList (map f nla)
  where f :: Alias' -> (String, (BoolType, String, [String]))
        f (Alias' {aName=a, aInternal=i}) = (a, (BEq, a, [i]))

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
        
mkAllNet :: Netlist -> IO AllNet
mkAllNet nl = return net
  where ms = mkPNetSig nl
        mr = mkPNetReg nl ms
        mg = mkPNetGate nl mr
        net = mkIndexNet mg
