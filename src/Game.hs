{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Game
  ( Status (..),
    Player (..),
    Move (..),
    Game (..),
  )
where

import Data.Kind (Type)

data Status = Win | Loss | Draw | Ongoing deriving (Eq, Show)

-- | abstract representation of a 2-player game
class (Eq (Move a)) => Game a where
  type Move a :: Type
  type Player a :: Type

  -- | return the initial situation with the given player
  initial :: Player a -> a

  -- | return the current status of the game scenario
  status :: a -> Status

  -- | return the current player of the game scenario
  player :: a -> Player a

  -- | return the last scenario of the current game scenario
  history :: a -> Maybe a

  -- | play the current game scenario by a move
  play :: a -> Move a -> a

  -- | generate all valid moves for the current game scenario
  moves :: a -> [Move a]

  -- | check if a move is valid for the current game scenario
  valid :: Move a -> a -> Bool
  valid m a = m `elem` moves a