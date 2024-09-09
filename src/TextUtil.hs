module TextUtil where

import Control.Monad.State
import System.IO
import Text.Read (readMaybe)

display :: (MonadIO m, Show a) => a -> m ()
display = liftIO . print

displayText :: (MonadIO m) => String -> m ()
displayText text = liftIO $ do
  putStr text
  hFlush stdout

displayTextLn :: (MonadIO m) => String -> m ()
displayTextLn = liftIO . putStrLn

prompt :: (MonadIO m) => String -> m String
prompt text = do
  displayText (text ++ " ")
  liftIO $ getLine

promptInt :: (MonadIO m) => String -> m Int
promptInt text = do
  displayTextLn text
  displayText "> "
  line <- liftIO $ getLine
  case readMaybe line of
    Just val -> return val
    Nothing  -> promptInt text

promptChar :: (MonadIO m) => String -> [Char] -> m Char
promptChar text chars = do
  displayTextLn text
  displayText "> "
  line <- liftIO $ getLine
  case line of
    char:[] -> if elem char chars
                 then return char
                 else promptChar text chars
    _       -> promptChar text chars
