{-# LANGUAGE TypeFamilies #-}

module Strategy
  ( Evaluate (..),
    Score (..),
    Strategy (..),
  )
where

import Data.Foldable (maximumBy)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Ord (comparing)
import Game (Game (..))

class (Game a, Ord (Score a)) => Evaluate a where
  type Score a :: Type

  -- | evaluate the current situation, larger score means better and smaller score means worse
  evaluate :: a -> Score a

class (Evaluate a) => Strategy a where
  type Level a :: Type

  -- | return the best move if there is one
  scoreMove :: Level a -> a -> Move a -> Score a

  -- | return the best move if there is one
  bestMove :: Level a -> a -> Maybe (Move a)