module ChessEngine
  ( Hagfish (..),
  )
where

import Chess
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

putBold :: String -> IO ()
putBold str = do
  ANSI.setSGR [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
  putStr str
  ANSI.setSGR [ANSI.Reset]

putColor :: ANSI.Color -> String -> IO ()
putColor c str = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid c]
  putStr str
  ANSI.setSGR [ANSI.Reset]

promptLn :: [Char] -> IO ()
promptLn message = putStrLn ("Hagfish> " ++ message)

prompt :: [Char] -> IO ()
prompt message = putStr ("Hagfish> " ++ message)

getUser :: IO String
getUser = do
  putStr "   User> "
  hFlush stdout
  getLine

instance Engine CLIEngine where
  initialize :: [String] -> IO (Either String CLIEngine)
  initialize _ = do
    promptLn "Initializing Hagfish CLI interface"

    -- \| prompt the color you want to play
    prompt "What color do you want to play? "
    putBold "White/white/w"
    putStr " or "
    putBold "Black/black/b"
    putStrLn "?"
    playerColor <- obtainColor

    prompt "Game started! You are playing as "
    putBold (map toLower $ show playerColor)
    putStrLn ".\n"
    return (Right $ CLIEngine playerColor (opponent playerColor) (Game.initial White))
    where
      obtainColor = do
        c <- getUser
        if c `elem` ["White", "white", "w"]
          then return White
          else
            if c `elem` ["Black", "black", "b"]
              then return Black
              else do
                prompt ("Color '" ++ c ++ "' is invalid. ")
                putBold "White/white/w"
                putStr " or "
                putBold "Black/black/b"
                putStrLn "?"
                obtainColor

  run :: CLIEngine -> IO ()
  run eng@(CLIEngine playerColor computerColor c) = go eng White 1
    where
      go eng@(CLIEngine pcolor ccolor c) current n = case Game.status c current of
        Game.Win -> declareWin current n
        Game.Loss -> declareWin (opponent current) n
        Game.Draw -> declareDraw n
        Game.Ongoing -> do
          putStrLn $ prettyChess c
          prompt ("Move " ++ show n ++ ", ")
          putBold $ map toLower (show current)
          putStrLn "'s turn."

          if pcolor == current then doUser else doEngine
        where
          declareWin color n = do
            prompt "Player "
            putBold $ map toLower (show color)
            prompt (" win after " ++ show n ++ " moves.")

          declareDraw n = do
            promptLn ("Draw after " ++ show n ++ "moves.")

          doUser = do
            promptLn "Enter your move: "
            go eng (opponent current) (n + 1)

          doEngine = do
            promptLn "Thinking..."
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