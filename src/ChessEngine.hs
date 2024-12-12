{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ChessEngine
  ( Hagfish (..),
    pCommand,
    UserAction (..),
  )
where

import Chess
import ChessStrategy
import Console
import Control.Monad.Identity (Identity)
import Data.Char (toLower, toUpper)
import Engine
import Game qualified
import Strategy
import System.Console.ANSI qualified as ANSI
import System.IO (hFlush, stdout)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Read (readMaybe)

data Hagfish = Hagfish
  { playerColor :: Color,
    engineColor :: Color,
    board :: Chess,
    level :: Int
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
  | ActionLevel (Maybe Int)
  | ActionScore
  deriving (Show, Eq)

putHelp =
  putPrompt
    ( "You can use "
        %% colored ".help" Console.White [Console.Bold]
        %% " to show help, "
        %% colored ".analysis" Console.White [Console.Bold]
        %% " to show analysis,\n         "
        %% colored ".level {n}" Console.White [Console.Bold]
        %% " to adjust engine search depth or "
        %% colored "make a move" Console.White [Console.Bold]
    )

pCommand :: ParsecT String u Identity UserAction
pCommand = (char '.' >> (pAnalysis <|> pHelp <|> pOption <|> pUnMove <|> pScore)) <|> pMove
  where
    pHelp = ActionHelp <$ string "help"

    pAnalysis = ActionAnalysis <$ string "analysis"

    pOption =
      ActionLevel <$> do
        string "level"
        i <- optionMaybe (spaces >> many1 digit)
        return (readMaybe =<< i)

    pUnMove = ActionUndo <$ string "undo"

    pMove = ActionMove <$> pPly

    pScore = ActionScore <$ string "score"

instance Engine Hagfish where
  initialize :: [String] -> IO (Either String Hagfish)
  initialize _ = do
    clear

    putPrompt "What color do you want to play?"
    putPrompt ("Choose between " %% colored "White/white/w" Console.White [Console.Bold] %% " or " %% colored "Black/black/b" Console.White [Console.Bold] %% ".")
    playerColor <- obtainColor

    putPrompt ("Game started! You are playing as " %% colored (map toLower $ show playerColor) Console.White [Console.Bold] %% ".")

    return (Right $ Hagfish playerColor (opponent playerColor) (Game.initial Chess.White) 4)
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
  run eng@(Hagfish playerColor computerColor c level) = go eng Chess.White 1
    where
      go eng@(Hagfish pcolor ccolor c level) current n = case Game.status c current of
        Game.Win -> declareWin current n
        Game.Loss -> declareWin (opponent current) n
        Game.Draw -> declareDraw n
        Game.Ongoing -> do
          putStrLn ""
          putPrompt ("Move " %% show n %% ", " %% colored (map toLower (show current)) Console.White [Console.Bold] %% "'s turn.")
          putStrLn $ prettyChess c
          if pcolor == current then doUser eng else doUser eng
        where
          declareWin color n = do
            putPrompt ("Player " %% colored (map toLower (show color)) Console.White [Console.Bold] %% "win after " %% show n %% " moves.")

          declareDraw n = do
            putPrompt ("Draw after " %% show n %% "moves.")

          doUser eng'@(Hagfish pcolor' ccolor' c' level') = do
            putPrompt "Enter your move or command: "
            input <- getPrompt
            let cmd = parse pCommand "(unknown)" input
            case cmd of
              Left err -> do
                putPrompt ("Unknown syntax command or move '" %% input %% "', showing the help message")
                putHelp
                doUser eng'
              Right ActionHelp -> putHelp >> doUser eng'
              Right ActionUndo ->
                if n < 3
                  then putPrompt "You can not undo move anymore. This is your first move!" >> doUser eng'
                  else go (Hagfish pcolor' ccolor' (unMove (unMove c')) level') current (n - 2)
              Right (ActionMove move) ->
                if Game.valid move c
                  then go (Hagfish pcolor' ccolor' (Game.play c' move) level') (opponent current) (n + 1)
                  else putPrompt (colored (prettyMove move) Console.Red [Console.Bold] %% " is not a valid move.") >> doUser eng'
              Right (ActionLevel Nothing) ->
                putPrompt
                  ( "Current search depth is "
                      %% colored (show level') Console.Green [Console.Bold]
                      %% "."
                  )
                  >> doUser eng'
              Right (ActionLevel (Just l)) ->
                putPrompt
                  ( "Setting search depth to "
                      %% colored (show l) Console.Green [Console.Bold]
                      %% "."
                  )
                  >> doUser (Hagfish pcolor' ccolor' c' l)
              Right ActionAnalysis -> do
                putPrompt
                  ( "Analysis moves with depth "
                      %% colored (show level') Console.Green [Console.Bold]
                      %% ". To adjust depth, use "
                      %% colored ".level {n}" Console.White [Console.Bold]
                      %% "."
                  )
                mapM_
                  ( \m -> do
                      let score = scoreMove level' c' m
                      putStyleLn ("          " %% colored (prettyMove m) Console.White [Console.Bold] %% " => " %% show score)
                  )
                  (Game.moves c')
                doUser eng'
              Right ActionScore -> do
                putPrompt ("Current board value is " %% show (positionValue (unPosition c') current))
                doUser eng'

          -- go (Hagfish pcolor ccolor (Game.play c playerMove)) (opponent current) (n + 1)

          doEngine = do
            putPrompt "Thinking..."
            let (Just engineMove) = bestMove level c
            putPrompt ("My move is " %% colored (prettyMove engineMove) Console.Cyan [Console.Bold] %% ".")
            go (Hagfish pcolor ccolor (Game.play c engineMove) level) (opponent current) (n + 1)
