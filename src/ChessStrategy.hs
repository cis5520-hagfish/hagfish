{-# LANGUAGE TypeFamilies #-}

module ChessStrategy
  ( evaluate,
    bestMove,
  )
where

import Chess (Chess)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Game (Game (..))
import Strategy
  ( Evaluate (..),
    Score (..),
    Strategy (..),
  )

instance Evaluate Chess where
  type Score Chess = Double

  evaluate :: Chess -> Score Chess
  evaluate c = 1.0 -- Placeholder for evaluation function

instance Strategy Chess where
  type Level Chess = Int

  scoreMove :: Level Chess -> Chess -> Move Chess -> Score Chess
  scoreMove maxDepth pos move = minimax newPos maxDepth (-1000000) 1000000 False
    where
      newPos = play pos move

  bestMove :: Level Chess -> Chess -> Maybe (Move Chess)
  bestMove maxDepth pos = case moves pos of
    [] -> Nothing
    validMoves -> Just $ maximumBy (comparing (negate . scoreMove maxDepth pos)) validMoves

-- | Core minimax function with alpha-beta pruning
-- maximizing is True when it's our turn, False for opponent's turn
minimax :: Chess -> Int -> Double -> Double -> Bool -> Double
minimax pos depth alpha beta maximizing
  -- Base cases: leaf node or maximum depth reached
  | depth == 0 = evaluate pos
  | null (moves pos) = evaluate pos
  -- Recursive case
  | otherwise =
      if maximizing
        then maximizingPlayer pos depth alpha beta
        else minimizingPlayer pos depth alpha beta

maximizingPlayer :: Chess -> Int -> Double -> Double -> Double
maximizingPlayer pos depth alpha beta = go alpha (moves pos)
  where
    go a [] = a
    go a (move : rest) =
      let newPos = play pos move
          score = minimax newPos (depth - 1) a beta False
          newAlpha = max a score
       in if newAlpha >= beta
            then newAlpha -- Beta cutoff, won't be selected by min player
            else go newAlpha rest

minimizingPlayer :: Chess -> Int -> Double -> Double -> Double
minimizingPlayer pos depth alpha beta = go beta (moves pos)
  where
    go b [] = b
    go b (move : rest) =
      let newPos = play pos move
          score = minimax newPos (depth - 1) alpha b True
          newBeta = min b score
       in if alpha >= newBeta
            then newBeta -- Alpha cutoff, won't be selected by max player
            else go newBeta rest
