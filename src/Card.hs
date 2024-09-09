module Card where

import Data.List

data Suit = Spade | Club | Heart | Diamond
          deriving Show

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
           | Jack | Queen | King | Ace
           deriving (Show, Eq)

data Card = Card { value :: Value
                 , suit  :: Suit
                 }

instance Show Card where
  show (Card value suit) = valueString value ++ [suitChar suit]
--  show (Card value suit) = show value ++ " of " ++ show suit ++ "s"

cardValue :: Card -> Int
cardValue (Card Two _)   =  2
cardValue (Card Three _) =  3
cardValue (Card Four _)  =  4
cardValue (Card Five _)  =  5
cardValue (Card Six _)   =  6
cardValue (Card Seven _) =  7
cardValue (Card Eight _) =  8
cardValue (Card Nine _)  =  9
cardValue (Card Ace _)   =  1
cardValue (Card _ _)     = 10

-- Large card printing
singleLargeCard :: Card -> String
singleLargeCard card = intercalate "\n" (largeCard card)

largeCard :: Card -> [String]
largeCard (Card value suit) =
  let [line1, line2, line3, line4, line5] = fiveByFive value (suitChar suit)
   in
      [ "┌─────────┐"
      , "│" ++ valueStringL value ++ "       │"
      , "│" ++ [suitChar suit] ++ " " ++ line1 ++ "  │"
      , "│  " ++ line2 ++ "  │"
      , "│  " ++ line3 ++ "  │"
      , "│  " ++ line4 ++ "  │"
      , "│  " ++ line5 ++ " " ++ [suitChar suit] ++ "│"
      , "│       " ++ valueString' value ++ "│"
      , "└─────────┘"
      ]

suitChar :: Suit -> Char
suitChar Spade   = '♠'
suitChar Club    = '♣'
suitChar Heart   = '♥'
suitChar Diamond = '♦'

valueString :: Value -> String
valueString Two   = "2"
valueString Three = "3"
valueString Four  = "4"
valueString Five  = "5"
valueString Six   = "6"
valueString Seven = "7"
valueString Eight = "8"
valueString Nine  = "9"
valueString Ten   = "10"
valueString Jack  = "J"
valueString Queen = "Q"
valueString King  = "K"
valueString Ace   = "A"

valueStringL :: Value -> String
valueStringL Two   = "2 "
valueStringL Three = "3 "
valueStringL Four  = "4 "
valueStringL Five  = "5 "
valueStringL Six   = "6 "
valueStringL Seven = "7 "
valueStringL Eight = "8 "
valueStringL Nine  = "9 "
valueStringL Ten   = "10"
valueStringL Jack  = "J "
valueStringL Queen = "Q "
valueStringL King  = "K "
valueStringL Ace   = "A "

valueString' :: Value -> String
valueString' Two   = " 2"
valueString' Three = " 3"
valueString' Four  = " 4"
valueString' Five  = " 5"
valueString' Six   = " 6"
valueString' Seven = " 7"
valueString' Eight = " 8"
valueString' Nine  = " 9"
valueString' Ten   = "10"
valueString' Jack  = " J"
valueString' Queen = " Q"
valueString' King  = " K"
valueString' Ace   = " A"

fiveByFive :: Value -> (Char -> [String])
fiveByFive King = \chr ->
  [ [chr, ' ', ' ', ' ', chr]
  , [chr, ' ', ' ', chr, ' ']
  , [chr, chr, chr, ' ', ' ']
  , [chr, ' ', ' ', chr, ' ']
  , [chr, ' ', ' ', ' ', chr]
  ]
fiveByFive Queen = \chr ->
  [ [' ', chr, chr, chr, ' ']
  , [chr, ' ', ' ', ' ', chr]
  , [chr, ' ', ' ', ' ', chr]
  , [chr, ' ', ' ', chr, ' ']
  , [' ', chr, chr, ' ', chr]
  ]
fiveByFive Jack = \chr ->
  [ [' ', ' ', chr, chr, chr]
  , [' ', ' ', ' ', ' ', chr]
  , [' ', ' ', ' ', ' ', chr]
  , [chr, ' ', ' ', ' ', chr]
  , [' ', chr, chr, chr, ' ']
  ]
fiveByFive Ten = \chr ->
  [ [' ', chr, ' ', chr, ' ']
  , [' ', chr, ' ', chr, ' ']
  , [' ', chr, ' ', chr, ' ']
  , [' ', chr, ' ', chr, ' ']
  , [' ', chr, ' ', chr, ' ']
  ]
fiveByFive Nine = \chr ->
  [ [' ', chr, ' ', chr, ' ']
  , [' ', chr, ' ', chr, ' ']
  , [' ', ' ', chr, ' ', ' ']
  , [' ', chr, ' ', chr, ' ']
  , [' ', chr, ' ', chr, ' ']
  ]
fiveByFive Eight = \chr ->
  [ [' ', chr, ' ', chr, ' ']
  , [' ', ' ', chr, ' ', ' ']
  , [' ', chr, ' ', chr, ' ']
  , [' ', ' ', chr, ' ', ' ']
  , [' ', chr, ' ', chr, ' ']
  ]
fiveByFive Seven = \chr ->
  [ [' ', chr, ' ', chr, ' ']
  , [' ', ' ', chr, ' ', ' ']
  , [' ', chr, ' ', chr, ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', chr, ' ', chr, ' ']
  ]
fiveByFive Six = \chr ->
  [ [' ', chr, ' ', chr, ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', chr, ' ', chr, ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', chr, ' ', chr, ' ']
  ]
fiveByFive Five = \chr ->
  [ [' ', chr, ' ', chr, ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', chr, ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', chr, ' ', chr, ' ']
  ]
fiveByFive Four = \chr ->
  [ [' ', chr, ' ', chr, ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', chr, ' ', chr, ' ']
  ]
fiveByFive Three = \chr ->
  [ [' ', ' ', chr, ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', chr, ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', chr, ' ', ' ']
  ]
fiveByFive Two = \chr ->
  [ [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', chr, ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', chr, ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  ]
fiveByFive Ace = \chr ->
  [ [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', chr, ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  , [' ', ' ', ' ', ' ', ' ']
  ]

cardBack :: [String]
cardBack =
  [ "┌─────────┐"
  , "│█ █ █ █ █│"
  , "│ █ █ █ █ │"
  , "│█ █ █ █ █│"
  , "│ █ █ █ █ │"
  , "│█ █ █ █ █│"
  , "│ █ █ █ █ │"
  , "│█ █ █ █ █│"
  , "└─────────┘"
  ]
