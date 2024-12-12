{-# LANGUAGE TypeFamilies #-}

module ChessStrategy
  ( evaluate,
    bestMove,
    positionValue
  )
where

import Game.Chess
import Chess (Chess (unHistory, unPosition))
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.Vector (fromList, (!), map)
import Game (Game (..))
import Strategy
  ( Evaluate (..),
    Score (..),
    Strategy (..),
  )
import Data.Ix (range)

-- | this gives the ``value`` for White, thus if evaluating black you need to
-- | take negative
pieceValue' :: PieceType -> Int -> Int
pieceValue' Pawn sq = 100 * (pawnValue ! sq)
  where pawnValue = fromList [0,   0,   0,   0,   0,   0,   0,   0, -- A1~H1
                            -31,   8,  -7, -37, -36, -14,   3, -31, -- A2~H2
                            -22,   9,   5, -11, -10,  -2,   3, -19, -- A3~H3
                            -26,   3,  10,   9,   6,   1,   0, -23, -- A4~H4
                            -17,  16,  -2,  15,  14,   0,  15, -13, -- A5~H5
                              7,  29,  21,  44,  40,  31,  44,   7, -- A6~H6
                              78,  83,  86,  73, 102,  82,  85,  90, -- A7~H7
                              0,   0,   0,   0,   0,   0,   0,   0] -- A8~H8
pieceValue' Knight sq = 280 * (knightValue ! sq)
  where knightValue = fromList [-74, -23, -26, -24, -19, -35, -22, -69,
                                -23, -15,   2,   0,   2,   0, -23, -20,
                                -18,  10,  13,  22,  18,  15,  11, -14,
                                 -1,   5,  31,  21,  22,  35,   2,   0,
                                 24,  24,  45,  37,  33,  41,  25,  17,
                                 10,  67,   1,  74,  73,  27,  62,  -2,
                                 -3,  -6, 100, -36,   4,  62,  -4, -14,
                                -66, -53, -75, -75, -10, -55, -58, -70]
pieceValue' Bishop sq = 320 * (bishopValue ! sq)
  where bishopValue = fromList [-7,   2, -15, -12, -14, -15, -10, -10,
                                19,  20,  11,   6,   7,   6,  20,  16,
                                14,  25,  24,  15,   8,  25,  20,  15,
                                13,  10,  17,  23,  17,  16,   0,   7,
                                25,  17,  20,  34,  26,  25,  15,  10,
                                -9,  39, -32,  41,  52, -10,  28, -14,
                               -11,  20,  35, -42, -39,  31,   2, -22,
                               -59, -78, -82, -76, -23,-107, -37, -50]
pieceValue' Rook sq = 479 * (rookValue ! sq)
  where rookValue = fromList [-30, -24, -18,   5,  -2, -18, -31, -32,
                              -53, -38, -31, -26, -29, -43, -44, -53,
                              -42, -28, -42, -25, -25, -35, -26, -46,
                              -28, -35, -16, -21, -13, -29, -46, -30,
                                0,   5,  16,  13,  18,  -4,  -9,  -6,
                               19,  35,  28,  33,  45,  27,  25,  15,
                               55,  29,  56,  67,  55,  62,  34,  60,
                               35,  29,  33,   4,  37,  33,  56,  50]
pieceValue' Queen sq = 929 * (queenValue ! sq)
  where queenValue = fromList [-39, -30, -31, -13, -31, -36, -34, -42,
                               -36, -18,   0, -19, -15, -15, -21, -38,
                               -30,  -6, -13, -11, -16, -11, -16, -27,
                               -14, -15,  -2,  -5,  -1, -10, -20, -22,
                                 1, -16,  22,  17,  25,  20, -13,  -6,
                                -2,  43,  32,  60,  72,  63,  43,   2,
                                14,  32,  60, -10,  20,  76,  57,  24,
                                 6,   1,  -8,-104,  69,  24,  88,  26]
pieceValue' King sq = 60000 * (kingValue ! sq)
  where kingValue = fromList [17,  30,  -3, -14,   6,  -1,  40,  18,
                              -4,   3, -14, -50, -57, -18,  13,   4,
                             -47, -42, -43, -79, -64, -32, -29, -32,
                             -55, -43, -52, -28, -51, -47,  -8, -50,
                             -55,  50,  11,  -4, -19,  13,   0, -49,
                             -62,  12, -57,  44, -67,  28,  37, -31,
                             -32,  10,  55,  56,  56,  55,  10,   3,
                               4,  54,  47, -99, -99,  60,  83, -62]

pieceValue :: Maybe (Color, PieceType) -> Int -> Int
pieceValue Nothing _ = 0
pieceValue (Just (c, p)) sq = if c == White then pieceValue' p sq else - (pieceValue' p (reflex sq))
  where reflex sq = let (x, y) = sq `divMod` 8 in (7 - x) * 8 + y

positionValue :: Position -> Color -> Int
positionValue p c = let v = sum $ Data.Vector.map (\s -> pieceValue (pieceAt p s) (fromEnum s)) squares in
    if c == White then v else -v
  where squares = fromList $ range (minBound, maxBound :: Square)


instance Evaluate Chess where
  type Score Chess = Int

  evaluate :: Chess -> Player Chess -> Score Chess
  evaluate c = positionValue (unPosition c)

instance Strategy Chess where
  type Level Chess = Int

  scoreMove :: Level Chess -> Chess -> Move Chess -> Score Chess
  scoreMove maxDepth pos move = minimax newPos (player pos) maxDepth (-1000000000) 1000000000 False
    where
      newPos = play pos move

  bestMove :: Level Chess -> Chess -> Maybe (Move Chess)
  bestMove maxDepth pos = case moves pos of
    [] -> Nothing
    validMoves -> Just $ maximumBy (comparing (negate . scoreMove maxDepth pos)) validMoves

-- | Core minimax function with alpha-beta pruning
-- maximizing is True when it's our turn, False for opponent's turn
minimax :: Chess -> Color -> Int -> Int -> Int -> Bool -> Int
minimax pos c depth alpha beta maximizing
  -- Base cases: leaf node or maximum depth reached
  | depth == 0 = evaluate pos c
  | null (moves pos) = evaluate pos c
  -- Recursive case
  | otherwise =
      if maximizing
        then maximizingPlayer pos c depth alpha beta
        else minimizingPlayer pos c depth alpha beta

maximizingPlayer :: Chess -> Color -> Int -> Int -> Int -> Int
maximizingPlayer pos c depth alpha beta = go alpha (moves pos)
  where
    go a [] = a
    go a (move : rest) =
      let newPos = play pos move
          score = minimax newPos c (depth - 1) a beta False
          newAlpha = max a score
       in if newAlpha >= beta
            then newAlpha -- Beta cutoff, won't be selected by min player
            else go newAlpha rest

minimizingPlayer :: Chess -> Color -> Int -> Int -> Int -> Int
minimizingPlayer pos c depth alpha beta = go beta (moves pos)
  where
    go b [] = b
    go b (move : rest) =
      let newPos = play pos move
          score = minimax newPos c (depth - 1) alpha b True
          newBeta = min b score
       in if alpha >= newBeta
            then newBeta -- Alpha cutoff, won't be selected by max player
            else go newBeta rest
