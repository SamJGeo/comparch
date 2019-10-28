module Traffic2 where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

{-
If reset=1 the machine goes to initial state, which is red.
Otherwise the state transitions are:
  red -> blue
  blue -> green
  green -> red
-}

stateMachine reset walkRequest = (red,amber,green,walk,stop)
  where
    walk = red
    stop = or2 amber green
    red = or3 r1 r2 r3
    amber = or2 a1 a2
    green = g1
    (g1d, a1d) = demux1 walkRequest g1
    g1 = dff (or3 reset a2 g1d)
    a1 = dff (and2 reset' a1d)
    r1 = dff (and2 reset' a1)
    r2 = dff (and2 reset' r1)
    r3 = dff (and2 reset' r2)
    a2 = dff (and2 reset' r3)
    reset' = inv reset
