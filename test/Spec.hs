{-# LANGUAGE TemplateHaskell #-}

import Chess
import ChessEngine
import ChessStrategy
import Data.List (intercalate)
import Data.Maybe (isJust, isNothing)
import Game
import Game.Chess
import Strategy
import Test.HUnit
import Test.QuickCheck
import Text.Parsec.Prim

finalChess :: Gen Chess
finalChess = resize 200 (arbitrary :: Gen Chess)

prop_parseSquareInvariant :: Square -> Bool
prop_parseSquareInvariant s = parseSquare (show s) == Just s

prop_chessPlayerFlip :: Chess -> Bool
prop_chessPlayerFlip c = all (\m -> player c /= player (play c m)) (moves c)

prop_chessOngoing :: Chess -> Player Chess -> Property
prop_chessOngoing c p = status c p == Ongoing ==> (not . null . moves) c

prop_chessFinal :: Player Chess -> Property
prop_chessFinal p = forAll finalChess $ \c ->
  status c p /= Ongoing ==> case status c p of
    Loss -> null (moves c) && inCheck p (unPosition c)
    Win -> null (moves c) && inCheck (opponent p) (unPosition c)
    Draw -> True
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

prop_chessUndo :: Chess -> Bool
prop_chessUndo c = all (\m -> unMove (play c m) == c) (moves c)

-- Strategy and Evaluation Tests
prop_bestMoveExists :: Chess -> Property
prop_bestMoveExists c =
  not (null (moves c)) ==>
    isJust (bestMove 3 c)

prop_bestMoveValid :: Chess -> Property
prop_bestMoveValid c =
  not (null (moves c)) ==>
    maybe False (`elem` moves c) (bestMove 3 c)

prop_evaluateConsistent :: Chess -> Bool
prop_evaluateConsistent c =
  let score = evaluate c
   in score >= -1000 && score <= 1000

parseMove :: String -> Chess -> Either String Ply
parseMove s c =
  let ply = parse pPly "(unknown)" s
   in case ply of
        Left err -> Left "Invalid syntax of move"
        Right ply -> if valid ply c then Right ply else Left "This is not a valid move"

prop_parseMove :: Property
prop_parseMove = forAllShow finalChess showMoves (\c -> all (\m -> parseMove (prettyMove m) c == Right m) (moves c))
  where
    showMoves c = intercalate "\n" (map showEach (moves c))
      where
        showEach ply = prettyMove ply ++ " <=> " ++ show (parseMove (prettyMove ply) c)

-- Tests for Evaluate

-- unit tests
test_singleFEN :: [Char] -> Status -> Test
test_singleFEN fenS expectedStatus = TestCase $ assertBool ("expected " ++ show expectedStatus) equality
  where
    equality =
      let position = fromFEN fenS
       in case position of
            Nothing -> True
            Just p -> let c = Chess p Nothing Nothing in status c (player c) == expectedStatus

collectedFEN :: [(String, Status)]
collectedFEN =
  [ ("4r3/5kQ1/7B/1b1Pp3/8/3n3P/1p4P1/4R1K1 b - - 2 37", Loss),
    ("4q3/7R/5R2/pp4p1/6kP/5PP1/7K/8 b - - 0 34", Loss),
    ("7k/7p/6pK/6P1/8/8/8/8 w - - 1 53", Draw),
    ("6k1/5p1p/6p1/1p6/1P1P4/Q7/3Pq1bP/R3r1RK w - - 0 29", Loss),
    ("3R4/5pkp/2BP2p1/2b1KP2/3r4/3n2P1/7P/8 w - - 3 48", Loss),
    ("4Q3/1p4N1/p5rp/6p1/5pk1/5PP1/PP4KP/8 b - - 0 35", Loss),
    ("3k4/3P4/3K4/8/8/8/8/8 b - - 2 112", Draw),
    ("7k/7R/4nNpp/p7/7P/P1r1P3/5PP1/6K1 b - - 20 45", Loss),
    ("5Q2/7k/8/2P3p1/1P1bBP2/6PK/1q5P/8 b - - 3 64", Loss),
    ("5r1k/6R1/1p2P2p/pP1p3P/3Pp3/6R1/1Prq4/3Kn1Q1 w - - 4 44", Loss),
    ("7k/5K1P/8/8/8/8/8/1B6 b - - 10 68", Draw),
    ("r4rk1/p5pp/1pb5/1Bpp4/6P1/1NP5/PPQ2n2/R3R1Kq w - - 5 24", Loss),
    ("8/4ppk1/3p1bp1/7p/6qP/7K/1r6/1R2Q3 w - - 0 40", Loss),
    ("1r1nQ1k1/1r5p/3P2p1/8/8/8/5PPP/B5K1 b - - 1 42", Loss),
    ("4q1k1/1p4Qp/n2rpPp1/p2p4/3P3P/5N2/5PP1/1B4K1 b - - 1 32", Loss),
    ("2k3r1/pp6/2b1p3/7p/5P1Q/P5n1/6qP/2R1R1K1 w - - 2 39", Loss),
    ("1b6/1r1rn3/1p2kR1p/pN1R2p1/2PBP3/1P6/P5KP/8 b - - 4 46", Loss),
    ("4nQ1k/p1p4p/1p1p1P2/3P4/2P1r3/1Pq1P2P/P5R1/5BK1 b - - 8 40", Loss),
    ("8/8/8/1B2B2Q/8/1K5k/5pq1/8 b - - 1 66", Loss),
    ("r4b1r/1pR5/pq2p3/1N2PpQk/5P1p/6P1/PP6/1KR5 b - - 5 35", Loss),
    ("2b1r3/7R/2p1k1q1/3pQp2/p1pP4/2N1P3/PP6/1K6 b - - 5 38", Loss),
    ("6k1/5p1p/P3p1p1/2p1b3/4P2P/8/Kq6/8 w - - 5 43", Loss),
    ("5k1Q/r2r4/3P2P1/8/p6P/1p6/P7/6K1 b - - 1 47", Loss),
    ("8/4p3/3pBk2/3Pn3/1P1KP3/3r4/1R6/8 w - - 1 53", Loss),
    ("Q7/8/1p3nk1/1P1p4/6pK/p7/4rb2/8 w - - 4 54", Loss),
    ("6k1/5p2/r2pp1pp/5P2/1pPR3Q/1P2N1PP/r4n2/1R4Kq w - - 2 40", Loss),
    ("Q7/4kp2/p2p2p1/1p1Pp1b1/2q5/2K5/PP6/7R w - - 9 38", Loss),
    ("r1r3k1/pp2ppbp/4b1p1/8/7P/1P1BBP2/PqP3P1/1K1R3R w - - 0 19", Loss),
    ("2r3k1/p5pp/8/6R1/3P4/P6b/1Q3P1P/3q2K1 w - - 7 31", Loss),
    ("1k1r1b1r/1Pp5/2P2p2/6p1/4p2p/2P1QbP1/PP3PqP/R4RK1 w - - 1 23", Loss),
    ("7k/1p4Q1/7p/p3B1b1/8/2N3P1/PP3PKP/8 b - - 0 32", Loss),
    ("8/5R1p/5kp1/8/5P2/1Bpn2PP/Pb6/3K4 b - - 2 41", Loss),
    ("1Q2r1k1/2Q2ppp/8/8/3b4/6PP/1p6/1R3BqK w - - 2 38", Loss),
    ("1k2r3/ppp3Q1/2p5/8/8/2P1P1P1/PP2r1qP/2RR2K1 w - - 1 32", Loss),
    ("4k2r/pp1b1p2/4p2p/b2pP1p1/3n3P/Q1P3B1/P2NqPP1/2R1K2R w Kk - 1 21", Loss),
    ("6rk/p1p4R/1p4B1/4n2p/2qP3P/2P5/P7/4R1K1 b - - 0 36", Loss),
    ("7r/p5k1/1p2P3/4Rpp1/P1P2n2/1B3P2/5P2/3R2Kr w - - 1 36", Loss),
    ("r4k2/1Q3pp1/3p2r1/7p/P3Pp1b/1N5q/1PP5/5RK1 w - - 5 33", Loss),
    ("1r5k/5N1P/5RK1/6P1/5P2/8/8/8 b - - 2 73", Loss),
    ("7K/5kP1/8/8/7r/8/8/8 w - - 7 68", Loss),
    ("r5rk/pp3N1p/2p2p2/q7/6Q1/1PN5/1PP4P/2K1R3 b - - 0 26", Loss),
    ("r3r3/pb6/1np4p/1p6/3R4/2P2RkP/P5P1/7K b - - 3 32", Loss),
    ("R1k5/8/2K5/2P5/8/8/8/8 b - - 2 85", Loss),
    ("3r4/5p1k/3p1Ppq/2pP2K1/1pN1P1P1/1P2R3/5r2/4R3 w - - 9 55", Loss),
    ("k1Q5/r5q1/4p3/2Np4/P2P1B1b/2P1p1P1/1P5P/5RK1 b - - 5 42", Loss),
    ("7k/3R4/5B2/pp4R1/8/7P/P1r3PK/8 b - - 0 37", Loss),
    ("7k/6Q1/5K2/8/1P6/8/8/8 b - - 20 68", Loss),
    ("B7/p2k2p1/1p1p3p/2pKr3/b1P5/2PP3P/PR4P1/8 w - - 2 36", Loss),
    ("2b4k/6bp/6p1/8/1Q2qP2/1P3NPn/3B2KP/6r1 w - - 5 46", Loss),
    ("4Nr1k/r4PQp/p7/1p6/3P4/8/1P4P1/5RK1 b - - 0 35", Loss),
    ("r5k1/r2bR1Q1/3p1p2/2pP1Pp1/1pP5/1P5P/1q1N2P1/1B4K1 b - - 2 38", Loss),
    ("k7/1Q5b/2PK1B2/8/p7/8/1P6/8 b - - 4 72", Loss),
    ("4R3/1p3pkQ/1n3b2/p5q1/7R/8/1Pr2PP1/6K1 b - - 3 34", Loss),
    ("8/2p5/5R2/6R1/2bk1p2/bpn1N3/r7/2K5 w - - 5 56", Loss),
    ("8/8/8/4b2p/7P/8/5k2/7K w - - 5 64", Draw),
    ("2q4k/pp5Q/5N2/3B4/4p3/2P3P1/P4P1P/6K1 b - - 0 30", Loss),
    ("8/5kPK/8/8/7r/3p4/8/8 w - - 2 53", Loss),
    ("r6r/p6B/bpnQ4/4P3/2ppkP2/P1n3N1/6P1/R1B1K2R b KQ - 6 27", Loss),
    ("4R1k1/pr3pp1/8/3p3Q/3P4/1P1r4/q4PP1/3R2K1 b - - 1 28", Loss),
    ("4r1k1/p1q2p1p/5p1Q/5N2/1n1P4/2pK4/5PP1/R7 w - - 1 28", Loss),
    ("4rk2/npq1pP2/p1p3pQ/2P5/PPB5/8/1B3P2/3b2K1 b - - 2 31", Loss),
    ("6k1/p4p2/1p1p4/K1p3PP/2qp1P2/P7/8/8 w - - 0 39", Loss),
    ("5r1k/p3R2Q/2pP1q2/8/8/6P1/PP3PK1/8 b - - 0 32", Loss),
    ("Q7/3N1ppk/4p2p/8/8/5PP1/5PKP/1r3q2 w - - 6 38", Loss),
    ("2r2bQk/2r5/pp1p1p1p/2q1p1pP/P3P3/1B3R2/1PP2PP1/3R2K1 b - - 3 30", Loss),
    ("2rk4/1R2Q3/4p2r/3p3p/2nP1P2/8/6PP/6K1 b - - 4 38", Loss),
    ("1Q2k3/8/2K2PP1/8/8/8/8/7N b - - 0 65", Loss),
    ("3q3k/p5Q1/8/4p3/3p3r/1Pb3R1/P4P1P/5RK1 b - - 0 32", Loss),
    ("6k1/6p1/8/5p1K/6q1/8/8/8 w - - 2 49", Loss),
    ("8/8/8/8/6k1/4n3/5p1K/6q1 w - - 0 70", Loss),
    ("8/1Q6/p2kp3/1pp5/1q6/1K1P4/PPP1rr1P/2R5 w - - 17 36", Loss),
    ("k6r/p4pp1/2B1p3/2p4p/2q2B2/P7/1P3P2/K7 b - - 1 34", Loss),
    ("8/1kp5/3b4/7p/1p4p1/1P3p1P/5P2/R2rrK2 w - - 0 46", Loss),
    ("5rk1/p4p2/4p2p/5p2/2P2Q2/4P1P1/3r2qP/2R3K1 w - - 2 44", Loss),
    ("q7/4k3/4p3/KP6/3q4/8/8/8 w - - 6 60", Loss),
    ("8/8/kQ6/8/8/2P2K2/5B2/8 b - - 4 52", Loss),
    ("8/8/2p2Q2/p1P4p/P3pk1p/1P4qP/4K1P1/8 b - - 17 58", Loss),
    ("6rk/7R/5KPP/8/8/8/8/8 b - - 2 70", Loss),
    ("8/5k2/5p2/5K2/2r3P1/5r2/8/3R4 w - - 10 72", Loss),
    ("8/8/8/8/8/7r/5q2/2k4K w - - 12 76", Loss),
    ("8/6P1/8/7N/p7/8/Kqk5/8 w - - 10 84", Loss),
    ("8/8/8/5b2/nk6/8/K7/1q6 w - - 0 86", Loss),
    ("6K1/6q1/5k2/8/8/8/8/8 w - - 14 86", Loss),
    ("r1bq1k1r/ppp2Qpp/5b2/4p2B/N2nP3/5N2/PP5P/R4RK1 b - - 6 17", Loss),
    ("4r2k/pp6/6p1/1q2bbBp/r3p2P/K6N/P2Q1PP1/3RR3 w - - 4 28", Loss),
    ("5bk1/p4p2/8/6p1/3N1p2/1P3P1P/P5rr/5RK1 w - - 6 32", Loss),
    ("2r3R1/5p2/p3pN1k/3bP3/P7/2b1B3/3r1PP1/R5K1 b - - 2 41", Loss),
    ("r2q3k/p4N2/2p3Q1/np1p3P/1R1Pr3/2P3P1/P4PK1/5n2 b - - 1 29", Loss),
    ("r4r1k/2p1R2b/3p4/pP6/7N/2P2P1P/3Q4/1qK3R1 w - - 1 33", Loss),
    ("8/1p5R/p2pQ3/3P2pk/2P5/1P5P/1q3Pp1/6K1 b - - 0 35", Loss),
    ("1br2r1k/p1q1NR2/1p3B1p/8/2p4P/2P5/P4PP1/4R1K1 b - - 0 30", Loss),
    ("8/p2r1p2/k2Pr3/b1N1p2p/p1N2p2/2P5/P4PPP/1R4K1 b - - 1 36", Loss),
    ("6k1/6p1/r3ppP1/7p/4PP2/K4N2/2p4P/1q4R1 w - - 0 38", Loss),
    ("5R2/7p/8/2q1p3/p3PbQk/5P2/K1P5/8 b - - 2 44", Loss),
    ("5rk1/6Qp/1p1p2pP/3Pp1P1/3b4/1q3P2/2R3K1/2R5 b - - 7 43", Loss),
    ("8/8/8/p3K2p/k1Q3PP/1p6/1P6/8 b - - 1 51", Loss),
    ("4r2k/7p/2Pp1Q1P/ppq5/6p1/1B3pK1/P2p1P2/3R4 b - - 0 43", Loss),
    ("8/4Q3/3P1kp1/2P5/1P2K3/5P2/8/6q1 b - - 14 68", Loss),
    ("8/8/4n3/3p2P1/1p3k2/8/1r3K2/7q w - - 1 58", Loss),
    ("8/6Qk/4q2p/5p2/4p3/2B5/8/2K5 b - - 0 56", Loss),
    ("8/p1R5/6k1/5BBp/5Kp1/pb6/1r4PP/8 b - - 2 54", Loss),
    ("8/8/6pk/8/2B2PKP/4r3/5n2/8 w - - 6 54", Loss),
    ("8/8/8/8/8/2K5/1Q6/1k6 b - - 6 72", Loss),
    ("8/8/8/8/8/6kp/8/5r1K w - - 0 79", Loss),
    ("7b/8/6p1/5p1k/2P4Q/6PK/4qqB1/8 b - - 1 81", Loss),
    ("r3k2r/pp3ppp/2n5/3p1b2/1b2N3/1P2P3/PBQ1N1PP/R2KqB1R w kq - 1 14", Loss),
    ("8/3Q1p2/p3kq1b/1pp1N3/5P2/8/PPP5/2K5 b - - 6 28", Loss),
    ("1r4k1/5pp1/4p1p1/3p1q2/Q2P1K2/R1P1R1P1/5P2/1r6 w - - 3 36", Loss),
    ("4R1k1/1p3ppp/b3p3/3pB3/1Pr5/2P1P1PP/5P2/2R3K1 b - - 0 29", Loss),
    ("2bq3k/p7/1pn4Q/3pP2p/3P3b/PPN2N2/2B5/6RK b - - 3 32", Loss),
    ("kr6/ppN5/2nr1pp1/8/3P2P1/4P1Pp/5P1K/RR6 b - - 5 35", Loss),
    ("r3qbnr/pP4pp/2N1p3/3p1p2/n2B1k1P/2PB1PP1/PP1N1K2/R6R b - - 0 22", Loss),
    ("4QnkR/pp3p2/5B2/2p2q2/8/1P4K1/P5PP/8 b - - 4 33", Loss),
    ("3Q1b1r/p2k1ppp/2N1p1b1/4P3/1r6/q3B3/P4PPP/2R3K1 b - - 2 23", Loss),
    ("3Q3k/1R6/7p/8/p5P1/6P1/5P2/6K1 b - - 0 43", Loss),
    ("6Qk/1b5p/5pp1/p7/1p4N1/1BrP4/4q1PP/1R3R1K b - - 0 30", Loss),
    ("r4r2/7R/p4pk1/1qb1pQ2/b3P3/2BP4/pP1RN3/K7 b - - 1 30", Loss),
    ("8/8/8/3r2k1/7p/6pK/6P1/8 w - - 0 56", Draw),
    ("8/5R2/bp5k/6PP/P2r1P1K/2p1N3/8/8 b - - 0 43", Loss),
    ("1q5R/2r2k2/b2pB3/p2Pp1Q1/1pn5/8/P1P2P2/R3K3 b Q - 4 33", Loss),
    ("1r5k/6Q1/p5R1/1p5p/5P1P/1P4P1/PK6/4q3 b - - 2 45", Loss),
    ("5Q2/p4b1k/1p5p/5q2/7K/5PP1/PP5r/8 w - - 6 41", Loss),
    ("8/4R3/2p5/ppk4p/3r3P/2Kq4/PP6/5N2 w - - 2 44", Loss),
    ("r7/2pN3p/2PkR3/5P2/8/p1P2B2/r3PK2/8 b - - 1 49", Loss),
    ("8/8/8/6Qk/5K2/7p/8/8 b - - 9 59", Loss),
    ("8/8/1K6/1P6/8/8/1Q6/3Q2k1 b - - 9 72", Loss),
    ("8/8/6p1/3K1k1p/P4R1P/6P1/r7/8 b - - 11 55", Loss),
    ("4k3/1p2P3/4KN2/2P5/1p6/1P6/6P1/8 b - - 1 59", Loss),
    ("8/8/8/8/8/8/p1K5/k7 b - - 1 67", Draw),
    ("6b1/8/7p/7P/8/6k1/8/4r1K1 w - - 2 75", Loss),
    ("8/8/8/3kn3/3b4/2p5/3q4/3K4 w - - 4 86", Loss),
    ("r1b2k1Q/pp4p1/6P1/q1p1p1B1/P1npN2p/7P/1PP3P1/R5K1 b - - 1 24", Loss),
    ("5k1Q/4rp2/3R2pp/4p3/4P2P/6P1/1q3P2/2b2RK1 b - - 4 31", Loss),
    ("1r4k1/p4p2/8/3p1b2/2p1nQ2/P4N2/5qPP/4RK2 w - - 0 25", Loss),
    ("3r1kR1/p6p/4Q3/4P3/8/1P3P1P/P1qr2P1/4RK2 b - - 7 36", Loss),
    ("2rbk1R1/5R2/6P1/3pP3/7r/2PK4/3B3p/8 b - - 4 49", Loss),
    ("1nkr3r/1Q3ppp/1p1bp3/1P3q2/3P4/4nB1P/P4PP1/R4RK1 b - - 0 20", Loss),
    ("4R1Q1/3r1kb1/2pr2p1/p4qB1/8/3p3P/1P3PP1/4R1K1 b - - 1 37", Loss),
    ("3r2k1/4ppbp/6p1/Q1P5/8/4B2P/Pr4q1/4RK1n w - - 0 33", Loss),
    ("7Q/8/6pk/7p/P6P/1q6/5PP1/3R2K1 b - - 2 41", Loss),
    ("4r3/p1p5/7k/5p1p/5b2/3Q2P1/PP2r1q1/5RK1 w - - 2 35", Loss),
    ("q7/p5Q1/1p6/5pkp/1P1p4/P5P1/5P1P/4R1K1 b - - 0 38", Loss),
    ("6R1/5p1p/4p1k1/p5P1/4N1P1/8/rr3PK1/8 b - - 6 41", Loss),
    ("8/8/1p6/3p4/3kn3/8/3q4/3K4 w - - 8 68", Loss),
    ("8/p4k1p/1n4p1/P4K2/3R1P2/6r1/4r3/1R6 w - - 0 44", Loss),
    ("6k1/8/p2p3p/2pP4/PpP5/1P1Bq1n1/5KQR/4r3 w - - 6 45", Loss),
    ("5rk1/8/3Q3b/2pPp3/2P1Pn2/6p1/3BqpB1/1R3K2 w - - 4 37", Loss),
    ("8/7k/1B6/p1P1b3/P3PpQ1/1P3Kp1/5r2/7r w - - 6 47", Loss),
    ("2bk4/2p5/3b2p1/Q7/8/1P3P2/P5Pq/3R3K w - - 0 44", Loss),
    ("1Q6/R7/5p2/k1p3p1/1p2P1P1/1P6/PKP1R3/3r2q1 b - - 0 40", Loss),
    ("7R/pp5k/2br2pp/4Q3/1KP2PP1/5q2/P7/8 b - - 1 41", Loss),
    ("5Q1k/6R1/8/p3B3/P5p1/1P3P2/K7/8 b - - 0 50", Loss),
    ("2R5/8/8/4p2q/3bbP2/6PK/3k4/8 w - - 5 49", Loss),
    ("2Q5/1k6/1P2B3/p1P5/P5p1/4Kp2/8/8 b - - 0 49", Loss),
    ("7k/p4Q2/1p4p1/6P1/8/4P1K1/2qp4/7R b - - 2 50", Loss),
    ("5k2/p2N1P1p/5K2/8/P1pp4/7P/6n1/2q5 b - - 1 56", Loss),
    ("2Kq4/1P2k3/8/8/8/8/8/8 w - - 6 82", Loss),
    ("8/4b3/8/8/8/7k/7p/7K w - - 2 113", Draw),
    ("8/8/8/2N5/8/2K5/kQ6/8 b - - 13 200", Loss),
    ("5R1k/1p1br1pp/p1n1p2P/4q3/3pN3/3B4/PPP3P1/1K1R4 b - - 0 29", Loss),
    ("3q1rk1/6Qp/1p3n2/3PpN2/1PP2bB1/8/6PP/5R1K b - - 0 31", Loss),
    ("5Q1k/3R3p/p3B3/8/4bp2/7P/P4PPK/q7 b - - 0 38", Loss),
    ("4r1k1/ppp2pp1/7p/7P/8/6qP/PP6/5RKR w - - 6 32", Loss),
    ("8/2R5/1p1p2p1/3P2k1/1pP5/1P3RK1/6rr/8 w - - 14 40", Loss),
    ("4r1k1/1p3p2/2pRn3/p3rN2/P1P1B1P1/8/3Q3R/5q1K w - - 2 38", Loss),
    ("3r1k1Q/3P1p1p/2N3pb/8/8/5K1P/1p6/8 b - - 1 44", Loss),
    ("4Q3/4q3/p2rk2R/1p2pp2/2n5/P2R4/1PP5/2K5 b - - 4 43", Loss),
    ("8/7p/6p1/2B2p2/7P/P2kP1P1/5P2/1r2K3 w - - 8 50", Loss),
    ("5r1k/6Q1/p1pq3p/3p4/1P6/P7/1B2nR1P/5R1K b - - 0 32", Loss),
    ("3R2k1/p4p1p/6pB/2p5/6P1/5Q2/Pq6/4R1K1 b - - 4 39", Loss),
    ("1R1Q4/5p1k/4p1pP/3n4/3P1n2/8/4q1NP/4K3 w - - 2 40", Loss),
    ("8/1p5k/p3Q2P/3p1p2/1P5q/P7/5PPK/2r5 w - - 5 42", Loss),
    ("1r6/5pk1/2bN2p1/2P1P2p/p2p3P/1p4P1/1Q1R1P2/6Kq w - - 4 47", Loss),
    ("r7/3k4/1p4Q1/2p1pP2/p1Pp4/PP6/3r3q/1RR4K w - - 15 41", Loss),
    ("6k1/5p2/p3b3/1p6/3B1PqK/8/3Q4/8 w - - 6 57", Loss),
    ("8/6R1/5pk1/5N2/5KP1/7P/8/8 b - - 5 52", Loss),
    ("6qK/8/8/3b1pk1/8/8/8/8 w - - 14 63", Loss),
    ("5Q2/1p5p/8/3P2kQ/r5P1/8/7P/4n2K b - - 2 46", Loss),
    ("6k1/8/1q6/3bK2p/4q3/8/8/8 w - - 4 54", Loss),
    ("8/8/8/8/1p6/5k2/6qK/8 w - - 10 58", Loss),
    ("8/5p2/Pb6/7p/4kP2/7P/1q2K3/r7 w - - 0 58", Loss),
    ("7k/7P/7K/8/8/8/8/8 b - - 2 68", Draw),
    ("R1k3r1/8/2P5/3KB3/4P3/8/8/8 b - - 4 68", Loss),
    ("3k1R2/8/3K3p/3B3P/5P2/8/8/8 b - - 10 76", Loss),
    ("8/8/8/6p1/5p2/6k1/7n/r6K w - - 12 83", Loss),
    ("8/8/B5Qk/5K2/8/8/8/8 b - - 20 84", Loss),
    ("3k4/3Q4/3K2n1/8/8/8/8/8 b - - 44 101", Loss),
    ("5b1r/1p1Qkp1p/pq3p2/3p3P/2rP1B2/3N3B/PP3P2/4K2R b K - 1 20", Loss),
    ("1kbq3r/Q4R1p/p1ppp3/4b1r1/N3P3/1P6/PP5P/1K1R4 b - - 1 27", Loss),
    ("5Q1k/q6p/6p1/2p3N1/3b4/6P1/PP3P1P/3R2K1 b - - 0 31", Loss),
    ("8/pp3k1p/2p4B/3p1r2/6Q1/3P3P/P1P1n1BK/6q1 w - - 10 31", Loss),
    ("1r6/3R2p1/4p3/6k1/8/2p5/2P5/1rK5 w - - 2 38", Loss),
    ("b2r3k/7p/5rp1/2B1R3/P1Bp2P1/6QP/2n2P1K/7q w - - 1 43", Loss),
    ("5rk1/pp3p1p/6P1/8/8/2bQP3/4BPP1/4qK1R w - - 1 27", Loss),
    ("8/8/pp3k2/2p1p2Q/2P1Pp2/P5q1/5RPK/3r4 w - - 4 44", Loss),
    ("6k1/5p1p/4p1pb/p3P3/2PPN3/8/P3R2P/1q1K4 w - - 4 48", Loss),
    ("2Rb2Qk/8/6pp/8/1qB5/6P1/7P/7K b - - 3 49", Loss),
    ("6k1/5p2/1p4p1/1p5p/1P5P/2nN1N2/2r1rPP1/R2K4 w - - 2 39", Loss),
    ("3r4/8/pp6/8/2PN4/1PK3P1/P7/3k1R2 b - - 4 50", Loss),
    ("8/pp6/2p5/Q7/1Q1K4/8/k7/8 b - - 3 48", Loss),
    ("4r1k1/2R3Q1/4q3/8/2Np4/P6p/KP6/8 b - - 0 41", Loss),
    ("7k/7R/5Np1/6P1/2n4P/4P1K1/8/5r2 b - - 0 45", Loss),
    ("8/8/1p4p1/1P4P1/8/4k3/4p3/4K2q w - - 0 55", Loss),
    ("4R3/p6k/6p1/1p5p/3B1b1K/2P5/P7/1r5q w - - 6 56", Loss),
    ("6k1/5p2/4p3/3nB1p1/6P1/5P2/1r6/1r5K w - - 0 59", Loss),
    ("2k5/1pQ5/3K4/7P/2P5/8/P7/8 b - - 2 54", Loss),
    ("8/8/2P5/1B4R1/7P/5k2/8/1r3K2 w - - 5 59", Loss),
    ("8/5R2/4P1p1/5p2/5P2/3r2PK/3k4/7q w - - 3 61", Loss),
    ("k7/2K5/8/P7/8/8/5B2/8 b - - 2 73", Draw),
    ("8/1Q6/8/8/5PK1/3r4/5P1k/7Q b - - 6 64", Loss),
    ("8/5K2/6Qk/8/8/8/8/8 b - - 4 141", Loss),
    ("r1bqk1Q1/1pppr3/p5pB/3P2b1/8/1P6/1PP2PPP/R3R1K1 b - - 2 20", Loss),
    ("8/5N1k/2p5/6pp/8/1P3p2/5R2/q4K2 w - - 1 49", Loss),
    ("8/6kp/4p1p1/1p2P1P1/1PrKp3/4P3/8/R7 w - - 2 43", Loss),
    ("3r2k1/1Q3pbp/6p1/8/2B5/1N2PP2/r4P2/3qKR2 w - - 2 35", Loss),
    ("4Qb2/3R4/pRk4p/P1p3r1/2P1Pq1N/3r1P2/6P1/6K1 b - - 2 34", Loss),
    ("3R4/5p2/p6P/P4k2/1P4p1/2P2bKr/5P2/8 w - - 5 46", Loss),
    ("8/p7/1kQ1p1p1/3r4/1P2q3/P6P/2R3P1/2R4K b - - 2 34", Loss),
    ("8/p2k3p/1pp1b3/2K1B3/1PP1B1r1/P5P1/3r4/4R3 w - - 0 36", Loss),
    ("k6r/2Q2p2/R3p3/1PPq4/8/6Pr/5P1P/2R3K1 b - - 0 30", Loss),
    ("4r1k1/1R4pp/p1p5/Q3p3/2P5/3NP1q1/PP2P3/5r1K w - - 0 31", Loss),
    ("8/2k5/2p2p2/KqPb4/7b/8/N7/8 w - - 6 70", Loss),
    ("8/6p1/4p3/3pPk1p/2pP3P/2PN4/r3qP2/2R1K3 w - - 4 44", Loss),
    ("k3r2r/N3bpp1/8/1R6/P2BB3/7b/2P5/R5K1 b - - 0 31", Loss),
    ("r7/1p4Bp/3p3k/1P1P1R2/p5RP/Pb6/2r2NK1/8 b - - 8 41", Loss),
    ("1Qb1k3/7p/8/5pp1/4p1n1/4P1PP/PB6/5q1K w - - 0 39", Loss),
    ("8/6R1/1p4kp/1bpnB3/6P1/5P1P/7K/8 b - - 2 40", Loss),
    ("3k1Q2/p2r4/1p6/2p1B3/2P2P2/4n3/P7/2K5 b - - 2 43", Loss),
    ("8/1p5p/1p4pk/1P2Q3/2nPp3/8/1q5P/1K3R2 w - - 7 46", Loss),
    ("2n4Q/4qp2/3nNkp1/1p1p3p/1PpP3P/2P1P1PB/5P2/6K1 b - - 4 49", Loss),
    ("2r5/8/3P2pk/1p3p2/1P5p/8/2r4q/6RK w - - 0 49", Loss),
    ("3r3k/8/3P1p1Q/5P2/2B5/8/5PK1/4q3 b - - 0 51", Loss),
    ("8/1B2Q3/P2P1kp1/5p2/3q3p/7P/6P1/7K b - - 3 57", Loss),
    ("7Q/8/6Pk/4BK2/8/8/8/8 b - - 0 52", Loss),
    ("7q/8/5P1K/5kPP/8/8/8/8 w - - 1 66", Loss),
    ("8/6r1/p3p3/P3P3/8/3kp3/1rpn4/R3RK2 w - - 2 58", Loss),
    ("4k3/6R1/1p2N3/1P1pPP2/Kq1P4/b7/3r4/8 w - - 4 60", Loss),
    ("8/6qK/3k3p/8/8/8/6r1/8 w - - 2 67", Loss),
    ("8/6q1/8/2p4R/P5pk/5pN1/7P/5BK1 b - - 0 59", Loss),
    ("8/5p1p/6p1/8/4P2P/1pp2k2/6q1/7K w - - 4 63", Loss),
    ("8/8/8/2q5/2K5/4q3/6k1/8 w - - 4 92", Loss),
    ("5Q2/8/8/2p5/bk6/4n3/6q1/6K1 w - - 6 108", Loss),
    ("r2B2k1/pp3pb1/6pp/1P2p3/2P3n1/1QNP1PP1/5P1q/1R3RK1 w - - 0 21", Loss),
    ("8/1R5p/1p3kp1/2b5/2p2p2/2Kb1P1P/P1r3P1/3R4 w - - 9 36", Loss),
    ("5r1k/1pp5/p2p3p/3P4/P2b3R/7P/1PQ2q2/2B3rK w - - 0 29", Loss),
    ("4r1k1/1b3ppp/4P3/1p3R2/3Q4/1PBP3P/1P2q2r/R3K3 w - - 3 34", Loss),
    ("4R3/4kp2/4pNp1/pp2PnPp/5P2/1Pr4P/5K2/8 b - - 5 37", Loss),
    ("r3b1r1/5pBk/3p1n1P/qp3PN1/p7/P2P4/8/1K1R2R1 b - - 1 33", Loss),
    ("8/p4pkp/1p6/4nRPB/1Krn3P/P7/6P1/5R2 w - - 0 34", Loss),
    ("2kr4/1Q3p2/2P1p3/4Pp2/3P1q1p/7P/1P2p1R1/6RK b - - 1 39", Loss),
    ("4r3/8/7R/5p1k/3R1PNp/1r1p3P/4bKP1/8 b - - 2 44", Loss),
    ("5Q2/5p1k/p4Npp/1p6/1P6/P3qP2/6PP/1B3K2 b - - 5 38", Loss),
    ("8/8/8/4R3/P2pk3/2bN4/2P1K3/8 b - - 2 65", Loss),
    ("8/P7/7p/6p1/1R6/4ppk1/1P6/3r1K2 w - - 1 48", Loss),
    ("8/2R3P1/5k1p/5p2/1P3p2/1bP3R1/r5P1/2K1r3 w - - 5 40", Loss),
    ("8/2p5/PpNpk2p/1P2p1b1/2P1Pn1B/4KP2/4r2P/3R4 w - - 1 43", Loss),
    ("6k1/5pqn/6pK/8/8/5r2/8/8 w - - 2 54", Loss),
    ("6k1/6P1/5Q2/6p1/6q1/8/5P2/3r1K2 w - - 5 53", Loss),
    ("5N2/7R/p6k/3r2p1/P7/1P2RKP1/5P2/1r6 b - - 0 46", Loss),
    ("8/5p1k/6p1/4Q1P1/4P3/8/5r1p/1r5K w - - 2 50", Loss),
    ("8/4bk2/2p5/p3BnrK/N2P3p/8/5R2/8 w - - 2 51", Loss),
    ("7R/8/1p2BK1k/2r5/5np1/8/P7/8 b - - 1 53", Loss),
    ("2q5/8/q7/K7/2k5/8/8/8 w - - 14 80", Loss),
    ("8/8/8/8/8/6K1/8/5R1k b - - 0 69", Loss),
    ("5k1R/8/5K2/8/8/8/8/2B5 b - - 40 87", Loss),
    ("5r1k/1bpQ2pp/pp4n1/5pN1/1Pr2P2/P3P3/6qP/3R1RK1 w - - 0 27", Loss),
    ("3k1b1r/3QR1pp/p2p4/3Bqp2/8/6P1/P4P1P/6K1 b - - 8 32", Loss),
    ("1r4kr/6Qp/p3N3/q3pP1P/2p1P3/P7/1PP5/2K4R b - - 4 30", Loss),
    ("5qk1/7Q/2p5/1b1n2Np/3P2N1/6P1/r4P2/6K1 b - - 0 40", Loss),
    ("7r/1p3R2/p2p4/3Ppk2/5p2/2N5/PP5r/5KR1 b - - 1 39", Loss),
    ("7k/1p3p1p/6p1/4q2P/8/5PqK/6P1/8 w - - 1 47", Loss),
    ("1r1NQ2k/8/4B1PP/8/3p2K1/p7/PP6/8 b - - 0 54", Loss),
    ("bRk1r3/3r2p1/2p2p1p/8/2p2B1P/5P2/2P2KP1/8 b - - 1 34", Loss),
    ("Q5k1/8/6PK/8/4p3/8/8/8 b - - 0 52", Loss),
    ("8/8/8/5K1p/5N2/5k2/4Q2P/8 b - - 1 71", Loss),
    ("7R/7k/2p2Q2/8/5P2/8/2q5/6K1 b - - 6 44", Loss),
    ("8/8/3p4/R2Ppp2/5k2/P3bP1r/7P/6rK w - - 0 59", Loss),
    ("8/8/8/8/R7/8/k1K5/8 b - - 22 63", Loss),
    ("8/8/8/2N4p/2B4k/6Q1/5P2/6K1 b - - 0 53", Loss),
    ("5Q1R/4r1k1/p5pb/1p3p2/6P1/P7/1P2qNK1/8 b - - 1 54", Loss),
    ("2Q5/1R6/1P6/k3p3/p7/2K2p2/P2r4/2q5 w - - 3 52", Loss),
    ("3R3k/1Q6/8/6pp/7P/8/6P1/7K b - - 5 54", Loss),
    ("4k3/2R1Q3/4p3/4p3/1r2P1Pp/1q3P1P/1p6/1K6 b - - 4 60", Loss),
    ("8/5pQk/5P2/8/6K1/6P1/5r2/8 b - - 4 58", Loss),
    ("8/8/5kp1/2p5/5p2/5q2/7K/7r w - - 20 70", Loss),
    ("8/8/2B5/8/Q7/k1p4P/1pK5/8 b - - 0 72", Loss),
    ("8/8/8/R7/3k4/3p4/3Kn3/2q5 w - - 0 67", Loss),
    ("6k1/5p1p/8/2pPb1p1/2N1Pn1K/5P1P/6r1/3R4 w - - 0 32", Loss),
    ("6k1/1N3pp1/7p/p4r1K/P3r3/2Pn2PP/1P6/R7 w - - 4 35", Loss),
    ("r2q1r1k/1pp3Q1/3ppn1p/3P4/pPP2P2/P4n1B/1B5P/2K3R1 b - - 2 27", Loss),
    ("8/3Q1p2/p1r1kp2/1p2p3/2q5/P5P1/1P5P/K2R4 b - - 5 33", Loss),
    ("8/1bPB1p2/p3pk2/6Q1/1P1qP3/2N3RP/6PK/8 b - - 2 49", Loss),
    ("4r2r/1pq1P1pp/1p2Bpk1/5P2/1P5Q/8/P1P3PP/3RR1K1 b - - 0 30", Loss)
  ]

test_allFEN :: Test
test_allFEN = TestList $ map (uncurry test_singleFEN) collectedFEN

test_pPly :: Test
test_pPly =
  TestList $
    map
      testParseMove
      [ ("e2e3", Right (move E2 E3)),
        ("j8j100", Left ""),
        ("e4e5q", Right (promoteTo (move E4 E5) Queen)),
        ("h4h5", Right (move H4 H5)),
        ("what's up boy", Left "")
      ]
  where
    testParseMove (str, mov) =
      let parsed = case parse pPly "(unknown)" str of
            Left _ -> Left ""
            Right mov -> Right mov
       in TestCase $ assertEqual "move should be matched" parsed mov

test_pCommand :: Test
test_pCommand =
  TestList $
    map
      testParseCommand
      [ (".help", Right ActionHelp),
        (".level", Right (ActionLevel Nothing)),
        (".level oh", Left ""),
        (".level 3", Right (ActionLevel (Just 3))),
        ("e2e4", Right (ActionMove (move E2 E4))),
        (".analysis", Right ActionAnalysis),
        (".level .help", Left ""),
        ("e2e100", Left "")
      ]
  where
    testParseCommand (str, act) =
      let parsed = case parse pCommand "(unknown)" str of
            Left _ -> Left ""
            Right mov -> Right mov
       in TestCase $ assertEqual "command should be matched" parsed act

return []

runTests = $quickCheckAll

main :: IO ()
main = do
  putStrLn "\n=====Property Tests==========================="
  _ <- runTests

  putStrLn "\n=====Unit Tests==============================="
  runTestTT test_allFEN
  runTestTT test_pPly
  runTestTT test_pCommand
  putStrLn "==============================================="
