module Outcome where

import Hand

data Outcome = NaturalBlackjack
             | Push
             | Win
             | Lose
             | Surrender
             deriving Show

-- player -> dealer -> winner
computeOutcome :: HandValue -> HandValue -> Outcome
computeOutcome (Hard 0)  _         = Surrender
computeOutcome (Soft 21) (Soft 21) = Push
computeOutcome (Soft 21) _         = NaturalBlackjack -- TODO if split, not natural
computeOutcome (Hard x) _ | x > 21 = Lose
computeOutcome _ (Hard x) | x > 21 = Win
computeOutcome player    dealer    = case player `compare` dealer of
                                       LT -> Lose
                                       EQ -> Push
                                       GT -> Win
