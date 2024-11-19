module Board
  ( Color (..),
    Square (..),
    parseSquare,
    Move (..),
    Board (..),
    Position (..),
  )
where

import Data.Char (ord)
import Game.Chess

-- some wrapper type and wrapper functions

-- | parse a string to a Square
parseSquare :: String -> Maybe Square
parseSquare [f, r] =
  let x = ord f - ord 'A'
   in let y = ord r - ord '1'
       in Just $ toEnum (x + y * 8)
parseSquare _ = Nothing

data Move = Move Square Square (Maybe PieceType) deriving (Eq)

toPly :: Move -> Ply
toPly (Move a b promotion) =
  let p = move a b
   in case promotion of
        Nothing -> p
        Just promo -> promoteTo p promo

fromPly :: Ply -> Move
fromPly p = Move (plySource p) (plyTarget p) (plyPromotion p)

instance Show Move where
  show :: Move -> String
  show (Move a b p) = show a ++ show b ++ show p

class Board a where
  -- | return the initial position of a board
  initial :: a

  -- | return the player of the current board
  player :: a -> Color

  -- | return all valid moves
  moves :: a -> [Move]

  -- | number of valid moves
  nmoves :: a -> Int
  nmoves = length . moves

  -- | into stalement, no move can do
  stalemate :: a -> Bool
  stalemate = (== 0) . nmoves

  -- | move a piece, the move is forced to be valid, used for engine
  enginePlay :: Move -> a -> a

  -- | move a piece, if the move is invalid then return Nothing,
  -- check by looking in to the moves list, should be used by user
  userPlay :: Move -> a -> Maybe a
  userPlay m a = if m `elem` moves a then Just (enginePlay m a) else Nothing

instance Board Position where
  initial :: Position
  initial = startpos

  player :: Position -> Color
  player = color

  moves :: Position -> [Move]
  moves = map fromPly . legalPlies

  enginePlay :: Move -> Position -> Position
  enginePlay m b = unsafeDoPly b (toPly m)