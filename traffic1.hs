module RBG where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

{-
If reset=1 the machine goes to initial state, which is red.
Otherwise the state transitions are:
  red -> blue
  blue -> green
  green -> red
-}

stateMachine reset = (red,amber,green)
  where
    red = or4 r1 r2 r3 r4
    amber = or2 a1 a2
    green = or3 g1 g2 g3
    g1 = dff (or2 reset a2)
    g2 = dff (and2 reset' g1)
    g3 = dff (and2 reset' g2)
    a1 = dff (and2 reset' g3) 
    r1 = dff (and2 reset' a1)
    r2 = dff (and2 reset' r1)
    r3 = dff (and2 reset' r2)
    r4 = dff (and2 reset' r3)
    a2 = dff (and2 reset' r4)
    reset' = inv reset
