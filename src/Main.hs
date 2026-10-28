module Main (main) where

import Runtime
import System.IO

main :: IO ()
main = do
  -- Ensure written output is shown immediately
  hSetBuffering stdout NoBuffering
  loop newEnvironment

loop :: Environment -> IO ()
loop env = do
  putStr "> "
  input <- getLine
  if input == "quit" then
    putStrLn "Exiting interpreter."
  else do
    newEnv <- interpret env input
    loop newEnv
