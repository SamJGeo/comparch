module TrafficLight where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

count16b :: CBit a => a -> a -> [a]
count16b reset input = x0++x1++x2++x3
  where (c0,x0) = count4b reset c1
        (c1,x1) = count4b reset c2
        (c2,x2) = count4b reset c3
        (c3,x3) = count4b reset input

count4b :: CBit a => a -> a -> (a, [a])
count4b reset input = (c0, [x0,x1,x2,x3])
  where (c0,x0) = cbit reset c1
        (c1,x1) = cbit reset c2
        (c2,x2) = cbit reset c3
        (c3,x3) = cbit reset input

cbit reset cin = (cout,s)
  where
    s = dff (mux1 reset s' zero)
    (cout,s') = halfAdd cin s

controller2 reset walkRequest = (red,amber,green,walk,stop,counter)
  where
    walk = red
    stop = or2 amber green
    red = or3 r1 r2 r3
    amber = or2 a1 a2
    green = g1
    counter = count16b reset walkRequest
    (g1d, a1d) = demux1 walkRequest g1
    g1 = dff (or3 reset a2 g1d)
    a1 = dff (and2 reset' a1d)
    r1 = dff (and2 reset' a1)
    r2 = dff (and2 reset' r1)
    r3 = dff (and2 reset' r2)
    a2 = dff (and2 reset' r3)
    reset' = inv reset

	
controller1 reset = (red,amber,green)
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