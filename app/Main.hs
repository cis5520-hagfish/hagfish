-- \* Entry point for a Haskell application
-- Typically, this file is short and most code is part of a reusable library
module Main where

import ChessEngine
import Engine
import Game.Chess.UCI (Info (String))
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  engine :: Either String Hagfish <- initialize args
  case engine of
    Left err -> die ("Hagfish> Failed to initialize, " ++ err)
    Right eng -> run eng
