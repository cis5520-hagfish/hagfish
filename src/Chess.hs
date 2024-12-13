{-# LANGUAGE TypeFamilies #-}

module Chess
  ( Chess (..),
    Color (..),
    Ply (..),
    Square (..),
    Position (..),
    PieceType (..),
    parseSquare,
    opponent,
    prettyChess,
    prettyMove,
    pPly,
    unMove,
  )
where

import Data.Char (ord, toUpper)
import Data.Ix (range)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Game
import Game.Chess
import Test.QuickCheck
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Control.Monad.Identity (Identity)

-- | parse a string to a Square
parseSquare :: String -> Maybe Square
parseSquare [f, r] =
  let x = ord f - ord 'A'
   in let y = ord r - ord '1'
       in Just $ toEnum (x + y * 8)
parseSquare _ = Nothing

-- parser combinators

-- parses a chess square
pSquare = do
  f <- pRank
  r <- pIndex
  return
    ( let x = ord f - ord 'A'
       in let y = ord r - ord '1'
           in toEnum (x + y * 8)
    )
  where
    pRank = toUpper <$> satisfy (`elem` "ABCDEFGHabcdefgh")
    pIndex = satisfy (`elem` "12345678")

-- parses a chess piece
pPiece = optionMaybe (pKnight <|> pBishop <|> pRook <|> pQueen)
  where
    pKnight = Knight <$ (char 'N' <|> char 'n')
    pBishop = Bishop <$ (char 'B' <|> char 'b')
    pRook = Rook <$ (char 'R' <|> char 'r')
    pQueen = Queen <$ (char 'Q' <|> char 'q')

-- parses a chess move
pPly :: ParsecT String u Identity Ply
pPly = do
  source <- pSquare
  target <- pSquare
  promo <- pPiece
  eof

  let ply = move source target
  return
    ( case promo of
        Nothing -> ply
        Just p -> promoteTo ply p
    )

data Chess = Chess
  { unPosition :: Position,
    unHistory :: Maybe Chess,
    unPly :: Maybe Ply
  }
  deriving (Eq)

instance Show Chess where
  show :: Chess -> String
  show = show . unPosition

instance Game Chess where
  type Move Chess = Ply
  type Player Chess = Color

  initial :: Player Chess -> Chess
  initial = const $ Chess startpos Nothing Nothing

  status :: Chess -> Player Chess -> Status
  status c p
    | inDraw c = Draw
    | inCheckmate c = if p == player c then Loss else Win
    | otherwise = Ongoing
    where
      inChecked :: Chess -> Bool
      inChecked c = inCheck (player c) (unPosition c)

      inCheckmate :: Chess -> Bool
      inCheckmate c = null (moves c) && inChecked c

      inDraw :: Chess -> Bool
      inDraw c =
        let inStalemate c = null (moves c) && not (inChecked c)
         in let inThreeRep c = case repetitions (fullHistory c []) of
                  Just (n, _) -> n >= 3
                  Nothing -> False
             in inStalemate c || inThreeRep c || insufficientMaterial (unPosition c)
        where
          fullHistory c acc =
            unPosition c
              : ( case history c of
                    Nothing -> acc
                    Just h -> fullHistory h acc
                )

  player :: Chess -> Player Chess
  player = color . unPosition

  history :: Chess -> Maybe Chess
  history = unHistory

  play :: Chess -> Move Chess -> Chess
  play c@(Chess p _ _) m = Chess (unsafeDoPly p m) (Just c) (Just m)

  moves :: Chess -> [Move Chess]
  moves = legalPlies . unPosition

-- for QuickCheck

instance Arbitrary Color where
  arbitrary :: Gen Color
  arbitrary = elements [Black, White]

  shrink :: Color -> [Color]
  shrink x = []

instance Arbitrary Square where
  arbitrary :: Gen Square
  arbitrary = elements $ range (minBound, maxBound)

  shrink :: Square -> [Square]
  shrink = enumFrom

instance Arbitrary Chess where
  arbitrary :: Gen Chess
  arbitrary = sized $ step (initial White)
    where
      step c 0 = return c
      step c n =
        let m = moves c
         in if null m -- no further moves
              then return c
              else do
                next <- elements m -- choose a random move
                step (play c next) (n - 1)

  shrink :: Chess -> [Chess]
  shrink hb = case history hb of
    Nothing -> []
    Just h -> [h]

prettyChess :: Chess -> String
prettyChess = showPos . unPosition
  where
    squareLine :: Int -> [Square]
    squareLine i = map toEnum (enumFromTo (8 * (i - 1)) (8 * i - 1))

    showLine :: Position -> Int -> String
    showLine pos i = show i ++ concatMap (showSquare . pieceAt pos) (squareLine i)

    showPos :: Position -> String
    showPos pos = "  A B C D E F G H\n" ++ intercalate "\n" (map (showLine pos) [8, 7 .. 1])

    showSquare :: Maybe (Color, PieceType) -> String
    showSquare Nothing = " ."
    showSquare (Just (Black, Pawn)) = " ♟"
    showSquare (Just (Black, Knight)) = " ♞"
    showSquare (Just (Black, Bishop)) = " ♝"
    showSquare (Just (Black, Rook)) = " ♜"
    showSquare (Just (Black, Queen)) = " ♛"
    showSquare (Just (Black, King)) = " ♚"
    showSquare (Just (White, Pawn)) = " ♙"
    showSquare (Just (White, Knight)) = " ♘"
    showSquare (Just (White, Bishop)) = " ♗"
    showSquare (Just (White, Rook)) = " ♖"
    showSquare (Just (White, Queen)) = " ♕"
    showSquare (Just (White, King)) = " ♔"

-- | pretty print ply
prettyMove :: Ply -> String
prettyMove m = show (plySource m) ++ show (plyTarget m) ++ showPromote (plyPromotion m)
  where
    showPromote Nothing = ""
    showPromote (Just Pawn) = "P"
    showPromote (Just Knight) = "N"
    showPromote (Just Bishop) = "B"
    showPromote (Just Rook) = "R"
    showPromote (Just Queen) = "Q"
    showPromote (Just King) = "K"

unMove :: Chess -> Chess
unMove c = fromMaybe c (unHistory c)