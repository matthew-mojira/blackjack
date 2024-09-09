module Main where

import Card
import Deck
import Hand
import Outcome
import Rules
import TextUtil

import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Data.List
import List.Shuffle

data Table = Table { deck          :: Deck
                   , dealerHand    :: [Card]
                   , playerHands   :: [[Card]]
                   , currentPlayer :: Maybe Int
                   , rules         :: Rules
                   , chips         :: Word
                   }
           deriving Show

newTable :: Int -> Rules -> IO Table
newTable numPlayers rules = do
  deck <- shuffleIO (concat $ replicate (numberOfDecks rules) newDeck)
  return Table { deck          = deck
               , dealerHand    = []
               , playerHands   = replicate numPlayers []
               , currentPlayer = Nothing
               , rules         = rules
               , chips         = 100
               }

type Blackjack m a = StateT Table m a

main :: IO ()
main = forever $ do
  numPlayers <- promptInt "How many players?"
  betSize    <- promptInt "Bet size for each player?"

  table      <- liftIO $ newTable numPlayers defaultRules

  -- run the game
  outcome <- evalStateT evalBlackjack table

  replay <- promptChar "Play again? (Y)es or (N)o" "YN"
  case replay of
    'Y' -> return ()
    'N' -> error "Thanks for playing!"

evalBlackjack :: Blackjack IO [Outcome]
evalBlackjack = do
  -- Start of game procedure
  drawDealerCard
  drawPlayerCards
  drawDealerCard
  drawPlayerCards

  displayDealerHand'
  displayPlayerHands

  -- check last card to offer insurance
  hand <- getDealerHand
  case last hand of
    Card Ace _ -> do
      char <- promptChar "Take insurance? (Y)es or (N)o" "YN"
      case char of
        'Y' -> return () -- TODO
        'N' -> return ()
    _          -> return ()

  -- check for 21 for real
  displayText "Checking for 21... "
  if handValue hand == Soft 21
    then do
      displayTextLn "Dealer has blackjack!"
      hands <- getPlayerHands
      return $ map (\hand -> if handValue hand == Soft 21 then Push else Lose) hands
    else do
      displayTextLn "No blackjack!"

      -- players' turn
      hands <- getPlayerHands
      nextPlayer
      whileM_ (getPlayer >>= (\v -> return (v /= Nothing))) $ do
        takePlayerTurn
        nextPlayer

      -- dealers' turn
      displayDealerHand
      hand <- getDealerHand

      whileM_ dealerShouldDraw $ do
        _ <- liftIO $ getLine
        drawDealerCard

        displayPlayerHands
        displayDealerHand

      -- compute winners and losers
      playerHands <- getPlayerHands
      dealerHand  <- getDealerHand
      let dealerValue = handValue dealerHand

      let outcomes = map (\hand -> computeOutcome (handValue hand) dealerValue) playerHands
      displayTextLn "Results:"
      displayPlayerHands' outcomes

      return outcomes
  where
    dealerShouldDraw :: Blackjack IO Bool
    dealerShouldDraw = do
      hand  <- getDealerHand
      rules <- getRules
      return $ case handValue hand of
                 Soft val -> val < if dealerHitsOnSoft17 rules then 18 else 17
                 Hard val -> val < 17

data Action = Action { hit        :: Bool
                     , stand      :: Bool
                     , doubleDown :: Bool
                     , split      :: Bool
                     , surrender  :: Bool
                     }

instance Show Action where
  show action =
    intercalate ", " $ filter (not . null)
      [ if hit action then "(H)it" else ""
      , if stand action then "(S)tand" else ""
      , if split action then "S(P)lit" else ""
      , if doubleDown action then "(D)ouble Down" else ""
      , if surrender action then "S(U)rrender" else ""
      ]

noAllowedAction :: Action
noAllowedAction = Action { hit        = True
                         , stand      = False
                         , split      = False
                         , doubleDown = False
                         , surrender  = True
                         }

takePlayerTurn :: Blackjack IO ()
takePlayerTurn = do
  hand <- getPlayerHand

  displayPlayerHand

  let action = noAllowedAction
  displayTextLn $ show action

  -- first turn
  if (handValue hand == Soft 21)
    then displayTextLn "Blackjack!"
  else if cardValue (hand!!0) == cardValue (hand!!1)
    then do
      choice <- promptChar "What would you like to do? (H)it, (S)tand, S(P)lit, S(U)rrender" "HSPU"
      case choice of
        'H' -> do
          drawPlayerCard
          simpleTurn
        'S' -> return ()
        'P' -> do
          splitCards hand
          rules <- getRules
          if value (hand!!0) == Ace && not (hittingSplitAces rules)
            then nextPlayer
            else takePlayerTurn
        'U' -> surrender
  else do
      choice <- promptChar "What would you like to do? (H)it, (S)tand, S(U)rrender" "HSU"
      case choice of
        'H' -> do
          drawPlayerCard
          simpleTurn
        'S' -> return ()
        'U' -> surrender
  where
    -- subsequent turns
    simpleTurn :: Blackjack IO ()
    simpleTurn = do
      displayPlayerHand

      hand <- getPlayerHand
      if isBust hand
        then displayTextLn "BUST!"
      else if is21 hand
        then displayTextLn "Blackjack!"
      else do
        choice <- promptChar "What would you like to do? (H)it or (S)tand" "HS"
        case choice of
          'H' -> do
            drawPlayerCard
            simpleTurn
          'S' -> return ()

    splitCards :: Hand -> Blackjack IO ()
    splitCards [card1a, card1b] = do
      Table{ playerHands = hands } <- get
      card2a <- drawCard
      card2b <- drawCard
      Just i <- getPlayer
      let hands' = insertAt i hands [[card1a, card2a], [card1b, card2b]]
      table <- get
      put $ table{ playerHands = hands' }

    surrender :: Blackjack IO ()
    surrender = do
      Table{ playerHands = hands } <- get
      Just i <- getPlayer
      let hands' = deleteAt i hands
      table <- get
      put $ table{ playerHands = hands' }

    deleteAt :: Int -> [[a]] -> [[a]]
    deleteAt i xss = let (h, _:xss') = splitAt i xss in h ++ [[]] ++ xss'

    insertAt :: Int -> [[a]] -> [[a]] -> [[a]]
    insertAt i xss yss = let (h, _:xss') = splitAt i xss in h ++ yss ++ xss'

nextPlayer :: Blackjack IO ()
nextPlayer = do
  player <- getPlayer
  table  <- get
  case player of
    Just i  -> if i < length (playerHands table) - 1
                 then put $ table{ currentPlayer = Just (i + 1) }
                 else put $ table{ currentPlayer = Nothing }
    Nothing -> put $ table{ currentPlayer = Just 0 }


getRules :: Blackjack IO Rules
getRules = do
  Table{ rules = rules } <- get
  return rules

getPlayer :: Blackjack IO (Maybe Int)
getPlayer = do
  Table{ currentPlayer = player } <- get
  return player

getDealerHand :: Blackjack IO Hand
getDealerHand = do
  Table{ dealerHand = hand } <- get
  return hand

getPlayerHand :: Blackjack IO Hand
getPlayerHand = do
  Just player <- getPlayer
  hand        <- getPlayerHands
  return $ hand!!player

getPlayerHands :: Blackjack IO [Hand]
getPlayerHands = do
  Table { playerHands = hands } <- get
  return hands

drawCard :: Blackjack IO Card
drawCard = do
  table@Table{ deck = cards } <- get
  let card = head cards
  put $ table{ deck = tail cards } -- assume not empty
  return card

drawDealerCard :: Blackjack IO ()
drawDealerCard = do
  card <- drawCard
  table@Table{ dealerHand = cards } <- get
  put $ table{ dealerHand = card:cards }

drawPlayerCard :: Blackjack IO ()
drawPlayerCard = do
  Table{ playerHands = hands } <- get
  card   <- drawCard
  Just i <- getPlayer
  let hands' = consAt i hands card
  table <- get
  put $ table{ playerHands = hands' }
  where
    consAt :: Int -> [[a]] -> a -> [[a]]
    consAt i xss x = let (h, xs:xss') = splitAt i xss in h ++ (x:xs):xss'

drawPlayerCards :: Blackjack IO ()
drawPlayerCards = do
  Table{ playerHands = hands } <- get
  hands' <- mapM (\hand -> do
    card <- drawCard
    return $ card : hand) hands
  table <- get
  put $ table{ playerHands = hands' }

{-
 - Display functions
 -}

displayDealerHand :: Blackjack IO ()
displayDealerHand = do
  hand <- getDealerHand
  displayTextLn "Dealer's cards:"
  displayTextLn $ largeHand hand
  displayTextLn $ "Value: " ++ show (handValue hand)

displayDealerHand' :: Blackjack IO ()
displayDealerHand' = do
  hand <- getDealerHand
  displayTextLn "Dealer's cards:"
  displayTextLn $ largeHand' hand

displayPlayerHand :: Blackjack IO ()
displayPlayerHand = do
  Just player <- getPlayer
  hand        <- getPlayerHand
  dealerHand  <- getDealerHand
  displayTextLn $ "Player " ++ show (player + 1) ++ "'s cards:"
  displayTextLn $ largeHand hand
  displayTextLn $ "Value: " ++ show (handValue hand)
  displayTextLn $ "Dealer: ?? " ++ show (last hand)

displayPlayerHands :: Blackjack IO ()
displayPlayerHands = do
  playerHands <- getPlayerHands
  forM playerHands $ \hand -> do
    displayTextLn $ intercalate " " (map show hand) ++ " | " ++ show (handValue hand)
  return ()

displayPlayerHands' :: [Outcome] -> Blackjack IO ()
displayPlayerHands' outcomes = do
  playerHands <- getPlayerHands
  forM (zip outcomes playerHands) $ \(outcome, hand) -> do
    displayTextLn $ intercalate " " (map show hand) ++ " | " ++ show (handValue hand) ++ " - " ++ show outcome
  return ()


