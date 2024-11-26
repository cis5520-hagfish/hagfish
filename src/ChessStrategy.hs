{-# LANGUAGE TypeFamilies #-}

module ChessStrategy 
  (

  )
where

import Strategy 
  ( Evaluate (..),
    Score (..),
    Strategy (..),
  )
import Chess
import Game (Game(..))

instance Evaluate Chess where
  type Score Chess = Double

  evaluate :: Chess -> Score Chess
  evaluate = undefined

instance Strategy Chess where
  bestMove :: Chess -> Maybe (Move Chess)
  bestMove = undefined


