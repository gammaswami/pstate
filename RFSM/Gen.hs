module RFSM.Gen where

import qualified Data.Vector as V hiding ((++), elem)
import Data.Bits
import System.Random
import System.IO.Unsafe

import Netlist.Data
import Netlist.Read

type Term = V.Vector Int
type Plane = V.Vector Term

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

inner :: Int -> Int -> Int -> Term
inner nzbits n m = case (bits V.! 0) `mod` (2 ^ nzbits) of
  0 -> inner nzbits n m
  _ -> bits
  where bits = V.generate n myR

outter :: Int -> Int -> Int -> Plane
outter nz x y = V.generate x (inner nz y)

mkRFSM :: Int -> Int -> Int -> Int -> RFSM
mkRFSM i o s x = RFSM { nrI = i
                      , nrO = o
                      , nrS = s
                      , nrX = x
                      , sAND = vISize
                      , sOR = vOSize
                      , saP = x
                      , soP = s + o
                      , aPlane = outter ((i + s) * 2)  x vISize
                      , oPlane = outter x (s + o) vOSize
                      }
  where
    vISize = ((i + s) `div` 32) + 1
    vOSize = (x `div` 64) + 1
    
mkNetlist :: RFSM -> Netlist
mkNetlist rfsm = Netlist e a is os ls comp reg gate []
  where
    (e, a) = mkEntArch rfsm
    (is, os, ls) = mkSignals rfsm
    (ac, ag) = mkAndCompGates rfsm
    (oc, og) = mkOrCompGates rfsm
    (rc, reg) = mkRegCompGates $ nrS rfsm
    (ic, ig) = mkInvCompGates (nrI rfsm) (nrS rfsm) 
    comp = ac ++ oc ++ rc ++ ic
    gate = ag ++ og ++ ig

mkEntArch :: RFSM -> (String, String)
mkEntArch rfsm = ( "rfsm-s-" ++ (show $ nrS rfsm),
                   "i-" ++ (show $ nrI rfsm)
                   ++ "-o-" ++ (show $ nrO rfsm)
                   ++ "-x-" ++ (show $ nrX rfsm)
                 )
                 
mkSignals :: RFSM -> (Signals, Signals, Signals)
mkSignals rfsm = (i, o, l)                 
  where
    i = map (crSig "I" "in") [1 .. nrI rfsm]
    o = map (crSig "O" "out") [1 .. nrO rfsm]
    l = map (crSig "A" "local") [1 .. nrX rfsm] ++ 
        map (crSig "NS" "local") [1 .. nrS rfsm] ++
        map (crSig "S" "local") [1 .. nrS rfsm]
        
mkAndCompGates :: RFSM -> (Components, Gates)
mkAndCompGates r = (ac, ag)
  where
    (nins, ag) = mkACG (saP r) (nrI r) (nrS r) (V.toList $ aPlane r)
    ac = map (\n -> crComp ("AND" ++ show n) "and" n) nins
    
mkOrCompGates :: RFSM -> (Components, Gates)
mkOrCompGates r = (oc, og)
  where
    (nins, og) = mkOCG (soP r) (nrO r) (nrX r) (V.toList $ oPlane r)
    oc = map (\n -> crComp ("OR" ++ show n) "or" n) nins

mkACG :: Int -> Int -> Int -> [Term] -> ([Int], Gates)
mkACG 0 _ _ _ = ([], [])
mkACG _ _ _ [] = error "mkAndCompGates: Too few Terms"
mkACG nands nin nst (t : ts) = (newin `uCons` oldins, newg : oldgs)
  where
    (newin, conns') = mkAndPorts nin nst t
    -- And, need output signal connection
    conns = (Connection "Q" ("A" ++ show nands)) : conns'
    newg = crGate2 ("AND" ++ show nands) "AND" newin conns
    (oldins, oldgs) = mkACG (nands - 1) nin nst ts
    
mkOCG :: Int -> Int -> Int -> [Term] -> ([Int], Gates)
mkOCG 0 _ _ _ = ([], [])
mkOCG _ _ _ [] = error "mkOrCompGates: Too few Terms"
mkOCG nors nout nimp (t : ts) = (newin `uCons` oldins, newg : oldgs)
  where
    (newin, conns') = mkOrPorts nimp t
    -- And, need output signal connection
    conns = (mkOrOutput nors nout) : conns'
    newg = crGate2 ("OR" ++ show nors) "OR" newin conns
    (oldins, oldgs) = mkOCG (nors - 1) nout nimp ts


mkOrOutput :: Int -> Int -> Connection
mkOrOutput nors nout | nors - nout > 0 =
  Connection "Q" ("NS" ++ show (nors - nout))
                     | otherwise =
    Connection "Q" ("O" ++ show nors)

uCons :: Eq a => a -> [a] -> [a]
uCons x [] = [x]
uCons x l = if (elem x l) then l else x : l

mkRegCompGates :: Int -> (Components, Registers)
mkRegCompGates nreg = ([c], is)
  where
    c = crComp' "REG" "state"
    is = map mks [1 .. nreg]
    mks n = crReg "REG" n [ Connection "D" ("NS" ++ show n)
                           , Connection "Q" ("S" ++ show n)
                           ]

mkInvCompGates :: Int -> Int -> (Components, Gates)
mkInvCompGates nin nreg = ([c], ii ++ is)
  where
    c = crComp' "INV" "not"
    ii = map (mki "I" 0) [1 .. nin] 
    is = map (mki "S" nin) [1 .. nreg]
    mki sig offs n = crGate "INV" (n + offs) [ Connection "D" (sig ++ show n)
                                             , Connection "Q" (sig ++ show n ++ "b")
                                             ]

mkAndPorts :: Int -> Int -> Term -> (Int, Connections)
mkAndPorts nin nst term = mkAP nin nst 32 (V.toList term)

mkOrPorts :: Int -> Term -> (Int, Connections)
mkOrPorts nimp term = mkOR nimp 64 (V.toList term)

mkAP :: Int -> Int -> Int -> [Int] -> (Int, Connections)
mkAP 0 0 _ _ = (0, [])
mkAP _ _ _ [] = error "mkAndPorts: Too few randoms"
mkAP nin nst 0 (r:rs) = mkAP nin nst 32 rs
mkAP 0 nst nbi (r:rs) = case twoBitCheck r of
  Nothing -> (l, sigs)
  Just t -> (l + 1, (crConnect (l + 1) "S" nst t) : sigs)
  where
    (l, sigs) = mkAP 0 (nst - 1) (nbi - 1) ((shift r (-2)) : rs)
mkAP nin nst nbi (r:rs) = case twoBitCheck r of
  Nothing -> (l, sigs)
  Just t -> (l + 1, (crConnect (l + 1) "I" nin t) : sigs)
  where
    (l, sigs) = mkAP (nin - 1) nst (nbi - 1) ((shift r (-2)) : rs)
    
mkOR :: Int -> Int -> [Int] -> (Int, Connections)
mkOR 0 _ _ = (0, [])
mkOR _ _ [] = error "mkOrPorts: Too few randoms"
mkOR nimp 0 (r:rs) = mkOR nimp 64 rs
mkOR nimp nbi (r:rs) = case r `mod` 2 == 1 of
  False -> (l, sigs)
  True -> (l + 1, (crConnect (l + 1) "A" nimp True) : sigs)
  where
    (l, sigs) = mkOR (nimp - 1) (nbi - 1) ((shift r (-1)) : rs)
    
twoBitCheck :: Int -> Maybe Bool
twoBitCheck i  | i `mod` 4 == 0 = Nothing
               | i `mod` 4 == 3 = Just False
               | otherwise = Just True
                 
crSignal :: String -> String -> String -> Int -> Signal
crSignal bar nstr mstr num = Signal (nstr ++ show num ++ bar) "std_logic" mstr
crSig = crSignal ""      
crSig' = crSignal "b"

crComp :: String -> String -> Int -> Component
crComp nam typ nin =
  Component { cName = nam
            , cType = typ
            , cNrIn = nin
            , cNrOut = 1
            , cPort = (Signal "Q" "std_logic" "out") : 
                      map (crSig "D" "in") [1 .. nin] 
            }

crComp' :: String -> String -> Component
crComp' nam typ =
  Component { cName = nam
            , cType = typ
            , cNrIn = 1
            , cNrOut = 1
            , cPort = [(Signal "Q" "std_logic" "out"), 
                       (Signal "D" "std_logic" "in")]
            }

crReg :: String -> Int -> Connections -> Register
crReg typ n conns =
  Register { rType = typ
           , rName = typ ++ show n
           , rPortMap = conns
           }

crGate :: String -> Int -> Connections -> Gate
crGate typ n conns =
  Gate { gComp = typ ++ show n -- Beware, name and type switched in Netlist.Read 
       , gName = typ
       , gPortMap = conns
       }

crGate2 :: String -> String -> Int -> Connections -> Gate
crGate2 nam typ n conns =
  Gate { gName = typ ++ show n -- Beware, name and type switched in Netlist.Read 
       , gComp = nam
       , gPortMap = conns
       }

crConnect :: Int -> String -> Int -> Bool -> Connection
crConnect loc pre num True = Connection ("D" ++ show loc) (pre ++ show num)
crConnect loc pre num False = Connection ("D" ++ show loc) (pre ++ show num ++ "b")
