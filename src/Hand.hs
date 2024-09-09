module Hand where

import Data.List

import Card

type Hand = [Card]

data HandValue = Soft Int
               | Hard Int

instance Eq HandValue where
  Soft v1 == Soft v2 = v1 == v2
  Hard v1 == Soft v2 = v1 == v2
  Soft v1 == Hard v2 = v1 == v2
  Hard v1 == Hard v2 = v1 == v2

instance Ord HandValue where
  Soft v1 <= Soft v2 = v1 <= v2
  Hard v1 <= Soft v2 = v1 <= v2
  Soft v1 <= Hard v2 = v1 <= v2
  Hard v1 <= Hard v2 = v1 <= v2

instance Show HandValue where
  show (Soft val) = show (val - 10) ++ "/" ++ show val
  show (Hard val) = show val

handValue :: Hand -> HandValue
handValue cards
  | hasAce cards = let oneVal = foldl (\acc card -> acc + cardValue card) 0 cards
                    in if oneVal <= 11
                         then Soft $ oneVal + 10
                         else Hard $ oneVal
  | otherwise    = Hard $ foldl (\acc card -> acc + cardValue card) 0 cards

hasAce :: Hand -> Bool
hasAce = any (\card -> value card == Ace)

isBust :: Hand -> Bool
isBust hand = case handValue hand of
                Soft val -> val > 21
                Hard val -> val > 21

is21 :: Hand -> Bool
is21 hand = case handValue hand of
              Soft val -> val == 21
              Hard val -> val == 21

largeHand :: Hand -> String
largeHand cards =
  intercalate "\n"
    $ map (intercalate " ") (transpose $ map largeCard cards)

largeHand' :: Hand -> String
largeHand' (card:cards) =
  intercalate "\n"
    $ map (intercalate " ") (transpose $ cardBack : map largeCard cards)
