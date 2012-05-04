:load netlist-data.hs netlist-read.hs pstate-data.hs pstate-bdd.hs pstate-fixpoint.hs
:m + NetlistRead NetlistData
:m + PStateFixpoint PStateBDD PStateData 
:m + Data.Vector Data.Boolean.CUDD
:m + System.IO.Unsafe
let nl = unsafePerformIO $ loadNetlist "dolphin.hxml"
let ma = mkAllNet nl
let an = mkBDDNet 0.5 ma
let fl = mkFixlist an
let (pv,im,mi) = an