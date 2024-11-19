module Evaluate
  ( Score (..),
    Status (..),
    Evaluate (..),
  )
where

class (Ord a, Bounded a) => Score a where
  -- | score when winning, should be the largest score
  win :: a
  win = maxBound

  -- | score when lossing, should be the lowest score
  loss :: a
  loss = minBound

  draw :: a

data Status = Winning | Lossing | Drawing | Ongoing

class Evaluate a where
  -- | evaluate the current situation, larger score means better and smaller score means worse
  evaluate :: (Score b) => a -> b
  evaluate a = case status a of
    Winning -> win
    Lossing -> loss
    Drawing -> draw
    Ongoing -> evaluateOngoing a

  -- | evaluate a ongoing situation
  evaluateOngoing :: (Score b) => a -> b

  -- | return the status of the current situation, winning, lossing, etc...
  status :: a -> Status