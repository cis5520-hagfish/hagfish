{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Console
  ( ConsoleColor (..),
    ConsoleStyle (..),
    PutStyle,
    ToStyled,
    (%%),
    putStyle,
    putStyleLn,
    colored,
    styleLength,
    clear,
  )
where

import Data.List (singleton)
import System.Console.ANSI qualified as ANSI
import System.IO (hFlush, stdout)

data ConsoleColor = Red | Green | Blue | Cyan | Purple | White | Yellow | Black

toANSI :: ConsoleColor -> ANSI.Color
toANSI Red = ANSI.Red
toANSI Green = ANSI.Green
toANSI Blue = ANSI.Blue
toANSI Cyan = ANSI.Cyan
toANSI Purple = ANSI.Magenta
toANSI White = ANSI.White
toANSI Black = ANSI.Black
toANSI Yellow = ANSI.Yellow

data ConsoleStyle = Bold | Italic | Underline

data StyledString = StyledString ConsoleColor [ConsoleStyle] String | PlainString String

putStyledString :: StyledString -> IO ()
putStyledString (StyledString color styles s) = do
  let ansiColor = toANSI color
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ansiColor]
  setStyles styles
  putStr s
  ANSI.setSGR [ANSI.Reset]
  where
    setStyles [] = return ()
    setStyles (Bold : rst) = do
      ANSI.setSGR [ANSI.SetConsoleIntensity ANSI.BoldIntensity]
      setStyles rst
    setStyles (Italic : rst) = do
      ANSI.setSGR [ANSI.SetItalicized True]
      setStyles rst
    setStyles (Underline : rst) = do
      ANSI.setSGR [ANSI.SetUnderlining ANSI.SingleUnderline]
      setStyles rst
putStyledString (PlainString str) = putStr str

colored :: String -> ConsoleColor -> [ConsoleStyle] -> StyledString
colored s c sty = StyledString c sty s

class ToStyled a where
  toStyled :: a -> [StyledString]

instance ToStyled String where
  toStyled :: String -> [StyledString]
  toStyled = singleton . PlainString

instance ToStyled StyledString where
  toStyled :: StyledString -> [StyledString]
  toStyled = singleton

instance ToStyled [StyledString] where
  toStyled :: [StyledString] -> [StyledString]
  toStyled = id

(%%) :: (ToStyled b, ToStyled a) => a -> b -> [StyledString]
(%%) a b = toStyled b ++ toStyled a

putStyle' :: [StyledString] -> IO ()
putStyle' [] = return ()
putStyle' (h : tl) = do
  putStyledString h
  putStyle' tl

putStyle :: (ToStyled a) => a -> IO ()
putStyle = putStyle' . reverse . toStyled

putStyleLn :: (ToStyled a) => a -> IO ()
putStyleLn s = putStyle s >> putChar '\n'

styleLength :: (ToStyled a) => a -> Int
styleLength = sum . map styleLength' . toStyled
  where
    styleLength' (PlainString s) = length s
    styleLength' (StyledString _ _ s) = length s

clear :: IO ()
clear = do
  ANSI.clearScreen
  ANSI.setCursorPosition 0 0

type PutStyle = forall a. (ToStyled a) => a -> IO ()