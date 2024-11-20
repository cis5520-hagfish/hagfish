{-# LANGUAGE TypeFamilies #-}

module Strategy
  ( Evaluate (..),
    Score (..),
  )
where

import Data.Kind (Type)
import Game (Game)

class (Game a, Ord (Score a)) => Evaluate a where
  type Score a :: Type

  -- | evaluate the current situation, larger score means better and smaller score means worse
  evaluate :: a -> Score a
