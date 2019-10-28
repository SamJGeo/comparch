module Main where
import HDL.Hydra.Core.Lib
import Traffic2

main :: IO ()
main = runrbg test_data_1

runrbg :: [[Int]] -> IO ()
runrbg input = runAllInput input output
  where
-- Extract input signals from readable input
    reset = getbit input 0
    walkRequest = getbit input 1
-- Connect the circuit to its inputs and outputs
    (r,a,g,walk,stop,counter) = trafficLightsV2 reset walkRequest
-- Format the signals for output
    output =
      [string "reset = ", bit reset,
       string " walkRequest = ", bit walkRequest,
       string " walk/stop = ", bit walk, bit stop,
       string "   rag = ", bit r, bit a, bit g,
       string "  counter = ", bits counter]

test_data_1 :: [[Int]]
test_data_1 =
  [[1,0], [0,0],  [0,0],  [0,0],  [0,0],  [0,1],  [0,0],  [0,0],  [0,1],  [0,0],  [0,0],  [0,0]]

-- Tests that counter works all the way to 16th bit
test_data_2 = [1,1] : (take (2^16) (repeat [0,1]))