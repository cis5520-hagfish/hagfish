{-# LANGUAGE TypeFamilies #-}

module ChessStrategy 
  ( evaluate,
    bestMove
  )
where

import Strategy 
  ( Evaluate (..),
    Score (..),
    Strategy (..),
  )
import Chess ( Chess )
import Game (Game(..))
import Data.Ord (comparing)
import Data.Foldable (maximumBy)

instance Evaluate Chess where
  type Score Chess = Double

  evaluate :: Chess -> Score Chess
  evaluate c = 1.0  -- Placeholder for evaluation function

instance Strategy Chess where
  bestMove :: Chess -> Maybe (Move Chess)
  bestMove pos = case moves pos of
    [] -> Nothing
    validMoves -> Just $ maximumBy (comparing (negate . scoreMove)) validMoves
    where
      scoreMove move = 
        let newPos = play pos move
            -- Start minimax search with initial alpha-beta bounds
            score = minimax newPos maxDepth (-infinity) infinity False
        in score

      maxDepth = 4  -- Adjust based on desired search depth
      infinity = 1000000

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
    go a (move:rest) = 
      let newPos = play pos move
          score = minimax newPos (depth - 1) a beta False
          newAlpha = max a score
      in if newAlpha >= beta
           then newAlpha  -- Beta cutoff, won't be selected by min player
           else go newAlpha rest

minimizingPlayer :: Chess -> Int -> Double -> Double -> Double
minimizingPlayer pos depth alpha beta = go beta (moves pos)
  where
    go b [] = b
    go b (move:rest) = 
      let newPos = play pos move
          score = minimax newPos (depth - 1) alpha b True
          newBeta = min b score
      in if alpha >= newBeta
           then newBeta  -- Alpha cutoff, won't be selected by max player
           else go newBeta rest
