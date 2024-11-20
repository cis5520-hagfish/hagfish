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

-- | abstract representation of a 2-player game
class (Eq (Move a)) => Game a where
  type Move a :: Type
  type Player a :: Type
  type Status a :: Type

  -- | return the initial situation with the given player
  initial :: a

  -- | return the current status of the game scenerio
  status :: a -> Status a

  -- | return the current player of the game scenerio
  player :: a -> Player a

  -- | return the last scenerio of the current game scenerio
  history :: a -> Maybe a

  -- | play the current game scenerio by a move
  play :: a -> Move a -> a

  -- | generate all valid moves for the current game scenerio
  moves :: a -> [Move a]

  -- | check if a move is valid for the current game scenerio
  valid :: Move a -> a -> Bool
  valid m a = m `elem` moves a