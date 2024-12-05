{-# LANGUAGE ScopedTypeVariables #-}

module ChessEngine
  ( Hagfish (..),
  )
where

import Chess
import Console
import Data.Char (toLower)
import Engine
import Game qualified
import System.Console.ANSI qualified as ANSI
import System.IO (hFlush, stdout)

data CLIEngine = CLIEngine
  { playerColor :: Color,
    engineColor :: Color,
    board :: Chess
  }

putPrompt :: (ToStyled a) => a -> IO ()
putPrompt s = putStyleLn (colored "Hagfish" Console.Cyan [Console.Bold] %% "> " %% s)

getPrompt :: IO String
getPrompt = do
  putStyle (colored "   User" Console.Purple [Console.Bold] %% "> ")
  hFlush stdout
  getLine

instance Engine CLIEngine where
  initialize :: [String] -> IO (Either String CLIEngine)
  initialize _ = do
    clear

    putPrompt "What color do you want to play?"
    putPrompt ("Choose between " %% colored "White/white/w" Console.White [Console.Bold] %% " or " %% colored "Black/black/b" Console.White [Console.Bold] %% ".")
    playerColor <- obtainColor

    putPrompt ("Game started! You are playing as " %% colored (map toLower $ show playerColor) Console.White [Console.Bold] %% ".")

    return (Right $ CLIEngine playerColor (opponent playerColor) (Game.initial Chess.White))
    where
      obtainColor = do
        c <- getPrompt
        if c `elem` ["White", "white", "w"]
          then return Chess.White
          else
            if c `elem` ["Black", "black", "b"]
              then return Chess.Black
              else do
                putPrompt ("Color '" %% c %% "' is invalid. ")
                putPrompt ("Choose between " %% colored "White/white/w" Console.White [Console.Bold] %% " or " %% colored "Black/black/b" Console.White [Console.Bold] %% ".")
                obtainColor

  run :: CLIEngine -> IO ()
  run eng@(CLIEngine playerColor computerColor c) = go eng Chess.White 1
    where
      go eng@(CLIEngine pcolor ccolor c) current n = case Game.status c current of
        Game.Win -> declareWin current n
        Game.Loss -> declareWin (opponent current) n
        Game.Draw -> declareDraw n
        Game.Ongoing -> do
          putStrLn $ prettyChess c
          putPrompt ("Move " %% show n %% ", " %% colored (map toLower (show current)) Console.White [Console.Bold] %% "'s turn.")
          if pcolor == current then doUser else doEngine
        where
          declareWin color n = do
            putPrompt ("Player " %% colored (map toLower (show color)) Console.White [Console.Bold] %% "win after " %% show n %% " moves.")

          declareDraw n = do
            putPrompt ("Draw after " %% show n %% "moves.")

          doUser = do
            putPrompt "Enter your move: "
            go eng (opponent current) (n + 1)

          doEngine = do
            putPrompt "Thinking..."
            go eng (opponent current) (n + 1)

data UCIEngine = UCIEngine {}

instance Engine UCIEngine where
  initialize :: [String] -> IO (Either String UCIEngine)
  initialize = undefined

  run :: UCIEngine -> IO ()
  run = undefined

data Hagfish
  = CLIFish CLIEngine
  | UCIFish UCIEngine

instance Engine Hagfish where
  initialize :: [String] -> IO (Either String Hagfish)
  initialize ("cli" : rst) = do
    engine <- initialize rst
    return $ CLIFish <$> engine
  initialize ("uci" : rst) = do
    engine <- initialize rst
    return $ UCIFish <$> engine
  initialize (x : _) = return $ Left ("invalid engine type " ++ x)
  initialize [] = return $ Left "engine type not provided, cli or uci"

  run :: Hagfish -> IO ()
  run (CLIFish cli) = run cli
  run (UCIFish uci) = run uci