{-# LANGUAGE TemplateHaskell #-}

import Chess
import ChessStrategy
import Data.Maybe (isNothing, isJust)
import Game
import Game.Chess
import Strategy
import Test.HUnit
import Test.QuickCheck

finalChess :: Gen Chess
finalChess = resize 200 (arbitrary :: Gen Chess)

prop_parseSquareInvariant :: Square -> Bool
prop_parseSquareInvariant s = parseSquare (show s) == Just s

prop_chessPlayerFlip :: Chess -> Bool
prop_chessPlayerFlip c = all (\m -> player c /= player (play c m)) (moves c)

prop_chessOngoing :: Chess -> Player Chess -> Property
prop_chessOngoing c p = status c p == Ongoing ==> (not . null . moves) c

prop_chessFinal :: Player Chess -> Property
prop_chessFinal p = forAll finalChess $ \c -> status c p /= Ongoing ==> case status c p of
  Loss -> null (moves c) && inCheck p (unPosition c)
  Win -> null (moves c) && inCheck (opponent p) (unPosition c)
  Draw -> True -- TODO: check this status
  _ -> error "unreachable"

prop_chessLengthColor :: Chess -> Bool
prop_chessLengthColor c = case histLength c `mod` 2 of
  0 -> player c == White
  1 -> player c == Black
  _ -> error "unreachable"
  where
    histLength c = case history c of
      Nothing -> 0
      Just c' -> histLength c' + 1

prop_chessWhiteStart :: Color -> Bool
prop_chessWhiteStart c = player ((initial :: Color -> Chess) c) == White

-- Strategy and Evaluation Tests
prop_bestMoveExists :: Chess -> Property
prop_bestMoveExists c = 
  not (null (moves c)) ==> 
  isJust (bestMove c)

prop_bestMoveValid :: Chess -> Property
prop_bestMoveValid c = 
  not (null (moves c)) ==>
  maybe False (`elem` moves c) (bestMove c)

prop_evaluateConsistent :: Chess -> Bool
prop_evaluateConsistent c = 
  let score = evaluate c
  in score >= -1000 && score <= 1000

return []

runTests = $quickCheckAll

main :: IO ()
main = do
  putStrLn "\n=====Tests==================================="
  _ <- runTests
  putStrLn "=====Tests==================================="