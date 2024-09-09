module Rules where

data Rules = Rules
  { numberOfDecks      :: Int
  , dealerHitsOnSoft17 :: Bool
  , strictSplitting    :: Bool
  , hittingSplitAces   :: Bool
  , resplittingAces    :: Bool
  , resplitMaximum     :: Maybe Int
  , doubleDownRule     :: DoubleDownRule
  , doubleAfterSplit   :: Bool
  , doubleDownSoft21   :: Bool
  , dealerChecks21Rule :: DealerChecks21Rule
  , surrenderRule      :: SurrenderRule
  , fiveCardCharlie    :: Bool
  }
  deriving Show

data DoubleDownRule = DoubleAnyCards
                    | Double9Thru11Only
                    | Double10Or11Only
                    deriving Show

data DealerChecks21Rule = DealerChecks21
                        | NoHoleCard
                        | OriginalBetsOnly
                        deriving Show

data SurrenderRule = NoSurrender
                   | EarlySurrender
                   | LateSurrender
                   deriving Show

defaultRules :: Rules
defaultRules = Rules { numberOfDecks      = 1               -- implemented
                     , dealerHitsOnSoft17 = True            -- implemented
                     , strictSplitting    = False           -- not implemented
                     , hittingSplitAces   = False           -- implemented
                     , resplittingAces    = False           -- not implemented
                     , resplitMaximum     = Just 4          -- not implemented
                     , doubleDownRule     = DoubleAnyCards  -- not implemented
                     , doubleAfterSplit   = True            -- not implemented
                     , doubleDownSoft21   = False           -- not implemented
                     , dealerChecks21Rule = DealerChecks21  -- not implemented
                     , surrenderRule      = NoSurrender     -- not implemented
                     , fiveCardCharlie    = False           -- not implemented
                     }


