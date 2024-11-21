{-# LANGUAGE TemplateHaskell #-}

import Chess
import Data.Maybe (isNothing)
import Game
import Test.HUnit
import Test.QuickCheck

prop_parseSquareInvariant :: Square -> Bool
prop_parseSquareInvariant s = parseSquare (show s) == Just s

prop_chessPlayerFlip :: Chess -> Bool
prop_chessPlayerFlip c = all (\m -> player c /= player (play c m)) (moves c)

prop_chessMoves :: Chess -> Bool
prop_chessMoves c = case status c of
  Ongoing -> not $ null (moves c)
  _ -> null (moves c)

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

-- Tests for Evaluate

return []

runTests = $quickCheckAll

main :: IO ()
main = do
  putStrLn "\n=====Tests==================================="
  _ <- runTests
  putStrLn "=====Tests==================================="