-- Demonstration of Concurrency in Haskell
-- Completed by Malaya Barenio

import Control.Concurrent

main = do
  -- Create two empty MVars which are containers that can be empty
  -- In this program, they may contain an Int
  done1 <- newEmptyMVar
  done2 <- newEmptyMVar

  -- Create a thread that does the following:
  forkIO $ do
    putStrLn "Thread 1 Start!"
    threadDelay 2000000 -- Wait 2 seconds (as microseconds)
    putStrLn "Thread 1 Finished!"
    putMVar done1 100 -- Thread 1 puts the value 100 into the done1 container

  -- Create a second thread
  forkIO $ do
    putStrLn "Thread 2 Start!"
    threadDelay 1000000 -- Wait 1 second
    putStrLn "Thread 2 Finished!"
    putMVar done2 2000 -- Thread 2 puts the value 2000 into the done2 container

  -- Store the results, waiting for both threads to finish
  -- (takeMVar waits until the container has a value)
  -- If we didn't wait, the main thread ending would prematurely end all child threads
  -- Remember each thread runs concurrently.
  -- Even though thread 1 is first in the code,
  -- it will end later than thread 2 because thread 1 takes longer to run.
  result1 <- takeMVar done1
  result2 <- takeMVar done2

  -- Print out results
  putStrLn "Both threads done!"
  putStrLn ("Thread 1 result: " ++ show result1)
  putStrLn ("Thread 2 result: " ++ show result2)
