module Main where
import HDL.Hydra.Core.Lib
import TrafficLight

main :: IO ()
main = do
    putStrLn "*** CONTROLLER 1 TESTS ***"
    putStrLn "Test 1 - Normal Input"
    runController1 controller1_testData1
    putStrLn ""
    putStrLn "Test 2 - Reset Mid State Cycle"
    runController1 controller1_testData2
    putStrLn ""

    putStrLn "*** CONTROLLER 2 TESTS ***"
    putStrLn "Test 1.1 - Normal Input"
    runController2 controller2_testData1_1
    putStrLn ""
    putStrLn "Test 1.2 - Normal Input"
    runController2 controller2_testData1_2
    putStrLn ""
    putStrLn "Test 2 - WalkRequest During Walk"
    runController2 controller2_testData2
    putStrLn ""
    putStrLn "Test 3 - Reset After WalkCounter Increased"
    runController2 controller2_testData3
    putStrLn ""
    -- WARNING THIS TEST TAKES A LONG TIME TO COMPLETE
    putStrLn "Test 4 - Walk Counter up to 16th bit"
    runController2 controller2_testData4
    putStrLn ""

runController1 :: [[Int]] -> IO ()
runController1 input = runAllInput input output
  where
-- Extract input signals from readable input
    reset = getbit input 0
-- Connect the circuit to its inputs and outputs
    (red,amber,green) = controller1 reset
-- Format the signals for output
    output =
      [string "reset = ", bit reset,
       string "   rag = ", bit red, bit amber, bit green]
       
runController2 :: [[Int]] -> IO ()
runController2 input = runAllInput input output
  where
-- Extract input signals from readable input
    reset = getbit input 0
    walkRequest = getbit input 1
-- Connect the circuit to its inputs and outputs
    (red,amber,green,walk,stop,counter) = controller2 reset walkRequest
-- Format the signals for output
    output =
      [string "reset = ", bit reset,
       string " walkRequest = ", bit walkRequest,
       string " walk/stop = ", bit walk, string "/", bit stop,
       string " rag = ", bit red, bit amber, bit green,
       string " counter = ", bits counter]


-- Tests normal input
controller1_testData1 =
  [[1], [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0],  [0], [0], [0]]

-- Tests reset during middle state
controller1_testData2 =
    [[1], [0],  [0],  [0],  [0],  [0],  [0],  [1],  [0],  [0],  [0],  [0], [0], [0]]

-- Tests normal input of single walkRequest
controller2_testData1_1 =
  [[1,0], [0,0],  [0,0],  [0,0],  [0,0],  [0,1],  [0,0],  [0,0],  [0,0],  [0,0],  [0,0],  [0,0]]
controller2_testData1_2 =
    [[1,0], [0,0],  [0,0],  [0,1],  [0,0],  [0,0],  [0,0],  [0,0],  [0,0],  [0,0],  [0,0],  [0,0]]
  
-- Tests walkRequest whilst walk signal is active, should not extend the walk signal 
-- e.g output should be same as testData1
controller2_testData2 =
  [[1,0], [0,0],  [0,0],  [0,0],  [0,0],  [0,1],  [0,0],  [0,0],  [0,1],  [0,0],  [0,0],  [0,0]]

-- Tests reset after walkCounter has been increased
controller2_testData3 =
    [[1,0], [0,0],  [0,1],  [0,1],  [0,1],  [0,1],  [0,0],  [1,0],  [0,0],  [0,0],  [0,0],  [0,0]]

-- Tests that counter works all the way to 16th bit and overflows as expected, takes significant a multiple seconds to run
controller2_testData4 = [1,1] : (take (1+2^16) (repeat [0,1]))