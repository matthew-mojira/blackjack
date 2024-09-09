module Deck where

import Card

type Deck = [Card]

newDeck :: Deck
newDeck = [Card value suit | suit  <- [Spade, Club, Heart, Diamond],
                             value <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]]

