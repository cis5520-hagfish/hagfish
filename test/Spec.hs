{-# LANGUAGE TemplateHaskell #-}

import Board
import Data.Ix (range)
import Evaluate
import Test.HUnit
import Test.QuickCheck

-- Tests for Board

instance Arbitrary Color where
  arbitrary = elements [Black, White]
  shrink x = []

instance Arbitrary Square where
  arbitrary = elements $ range (minBound, maxBound)
  shrink = enumFrom

-- | Board with history
data HistoryBoard = HB {unboard :: Position, history :: Maybe HistoryBoard}

instance Show HistoryBoard where
  show = show . unboard

initialHB = HB initial Nothing

instance Arbitrary HistoryBoard where
  arbitrary :: Gen HistoryBoard
  arbitrary = sized $ aux initialHB
    where
      aux hb@(HB b _) n =
        if n == 0
          then return hb
          else
            let m = moves b
             in if null m
                  then return hb
                  else do
                    next <- elements m
                    aux (HB (enginePlay next b) (Just hb)) (n - 1)

  shrink hb = case history hb of
    Nothing -> []
    Just h -> [h]

prop_parseSquareInvariant :: Square -> Bool
prop_parseSquareInvariant s = parseSquare (show s) == Just s

prop_boardColorFlip :: HistoryBoard -> Bool
prop_boardColorFlip (HB b _) = all (\m -> player b /= player (enginePlay m b)) (moves b)

-- Tests for Evaluate

return []

runTests = $quickCheckAll

main :: IO ()
main = do
  putStrLn "\n=====Tests==================================="
  _ <- runTests
  putStrLn "=====Tests==================================="