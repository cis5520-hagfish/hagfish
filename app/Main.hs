-- \* Entry point for a Haskell application
-- Typically, this file is short and most code is part of a reusable library
module Main where

import ChessEngine
import Console
import Engine
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)

putPrompt :: (ToStyled a) => a -> IO ()
putPrompt s = putStyleLn (colored "Hagfish" Console.Cyan [Console.Bold] %% "> " %% s)

main :: IO ()
main = do
  args <- getArgs
  engine :: Either String Hagfish <- initialize args
  case engine of
    Left err -> do
      putPrompt ("Failed to initialized. " %% colored err Console.Red [])
      exitWith $ ExitFailure 1
    Right eng -> run eng
