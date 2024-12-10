{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChessEngine
  ( Hagfish (..),
  )
where

import Chess
import ChessStrategy
import Console
import Data.Char (toLower, toUpper)
import Engine
import Game qualified
import Strategy
import System.Console.ANSI qualified as ANSI
import System.IO (hFlush, stdout)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim

data Hagfish = Hagfish
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

data UserAction
  = ActionUndo
  | ActionMove Ply
  | ActionHelp
  | ActionAnalysis
  | ActionConfig

putHelp =
  putPrompt
    ( "You can use "
        %% colored ".help" Console.White [Console.Bold]
        %% " to show help, "
        %% colored ".analysis" Console.White [Console.Bold]
        %% " to show analysis,\n         "
        %% colored ".config" Console.White [Console.Bold]
        %% " to adjust engine options or "
        %% colored "make a move" Console.White [Console.Bold]
    )

instance Engine Hagfish where
  initialize :: [String] -> IO (Either String Hagfish)
  initialize _ = do
    clear

    putPrompt "What color do you want to play?"
    putPrompt ("Choose between " %% colored "White/white/w" Console.White [Console.Bold] %% " or " %% colored "Black/black/b" Console.White [Console.Bold] %% ".")
    playerColor <- obtainColor

    putPrompt ("Game started! You are playing as " %% colored (map toLower $ show playerColor) Console.White [Console.Bold] %% ".")

    return (Right $ Hagfish playerColor (opponent playerColor) (Game.initial Chess.White))
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

  run :: Hagfish -> IO ()
  run eng@(Hagfish playerColor computerColor c) = go eng Chess.White 1
    where
      go eng@(Hagfish pcolor ccolor c) current n = case Game.status c current of
        Game.Win -> declareWin current n
        Game.Loss -> declareWin (opponent current) n
        Game.Draw -> declareDraw n
        Game.Ongoing -> do
          putStrLn ""
          putPrompt ("Move " %% show n %% ", " %% colored (map toLower (show current)) Console.White [Console.Bold] %% "'s turn.")
          putStrLn $ prettyChess c
          if pcolor == current then doUser else doEngine
        where
          declareWin color n = do
            putPrompt ("Player " %% colored (map toLower (show color)) Console.White [Console.Bold] %% "win after " %% show n %% " moves.")

          declareDraw n = do
            putPrompt ("Draw after " %% show n %% "moves.")

          doUser = do
            putPrompt "Enter your move or command: "
            input <- getPrompt
            let cmd = parse pCommand "(unknown)" input
            case cmd of
              Left _ -> do
                putPrompt ("Invalid command '" %% input %% "', showing the help message")
                putHelp
                doUser
              Right ActionHelp -> putHelp >> doUser
              Right ActionUndo ->
                if n < 3
                  then putPrompt "You can not undo move anymore." >> doUser
                  else go (Hagfish pcolor ccolor (unMove (unMove c))) current (n - 2)
              Right (ActionMove move) ->
                if Game.valid move c
                  then go (Hagfish pcolor ccolor (Game.play c move)) (opponent current) (n + 1)
                  else putPrompt (colored (prettyMove move) Console.White [Console.Bold] %% " is not a valid move.") >> doUser
              _ -> error "Unimplemented user action"
            where
              pHelp = ActionHelp <$ string ".help"
              pAnalysis = ActionAnalysis <$ string ".analysis"
              pOption = ActionConfig <$ string ".config"
              pUnMove = ActionUndo <$ string ".undo"
              pMove = ActionMove <$> pPly

              pCommand = pHelp <|> pAnalysis <|> pOption <|> pUnMove <|> pMove

          -- go (Hagfish pcolor ccolor (Game.play c playerMove)) (opponent current) (n + 1)

          doEngine = do
            putPrompt "Thinking..."
            let (Just engineMove) = bestMove c
            putPrompt ("My move is " %% colored (prettyMove engineMove) Console.White [Console.Bold] %% ".")
            go (Hagfish pcolor ccolor (Game.play c engineMove)) (opponent current) (n + 1)
