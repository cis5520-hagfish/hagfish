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

  -- | evaluate a move under the current situation
  evaluateMove :: a -> Move a -> Score a
  evaluateMove c m = evaluate (play c m)

class (Evaluate a) => Strategy a where
  -- | return the best move if there is one
  bestMove :: a -> Maybe (Move a)
  bestMove s = case moves s of
    [] -> Nothing
    x -> x & map (\m -> (evaluateMove s m, m)) & maximumBy (comparing fst) & Just . snd