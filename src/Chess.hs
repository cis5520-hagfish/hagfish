{-# LANGUAGE TypeFamilies #-}

module Chess
  ( Chess,
    Color (..),
    Ply (..),
    Square (..),
    Position (..),
    parseSquare,
  )
where

import Data.Char (ord)
import Data.Ix (range)
import Game
import Game.Chess
  ( Color (..),
    Ply,
    Position (..),
    Square (..),
    legalPlies,
    startpos,
    unsafeDoPly,
  )
import Test.QuickCheck

-- | parse a string to a Square
parseSquare :: String -> Maybe Square
parseSquare [f, r] =
  let x = ord f - ord 'A'
   in let y = ord r - ord '1'
       in Just $ toEnum (x + y * 8)
parseSquare _ = Nothing

data Chess = Chess
  { unPosition :: Position,
    unHistory :: Maybe Chess
  }
  deriving (Eq)

instance Show Chess where
  show :: Chess -> String
  show = show . unPosition -- TODO: maybe a better show function?

instance Game Chess where
  type Move Chess = Ply
  type Player Chess = Color

  initial :: Player Chess -> Chess
  initial = const $ Chess startpos Nothing

  status :: Chess -> Status
  status c = undefined -- TODO: implement the status of the current chess

  player :: Chess -> Player Chess
  player = color . unPosition

  history :: Chess -> Maybe Chess
  history = unHistory

  play :: Chess -> Move Chess -> Chess
  play c@(Chess p _) m = Chess (unsafeDoPly p m) (Just c)

  moves :: Chess -> [Move Chess]
  moves = legalPlies . unPosition

-- for QuickCheck

instance Arbitrary Color where
  arbitrary :: Gen Color
  arbitrary = elements [Black, White]

  shrink :: Color -> [Color]
  shrink x = []

instance Arbitrary Square where
  arbitrary :: Gen Square
  arbitrary = elements $ range (minBound, maxBound)

  shrink :: Square -> [Square]
  shrink = enumFrom

instance Arbitrary Chess where
  arbitrary :: Gen Chess
  arbitrary = sized $ step (initial White)
    where
      step c 0 = return c
      step c n =
        let m = moves c
         in if null m -- no further moves
              then return c
              else do
                next <- elements m -- choose a random move
                step (play c next) (n - 1)

  shrink :: Chess -> [Chess]
  shrink hb = case history hb of
    Nothing -> []
    Just h -> [h]