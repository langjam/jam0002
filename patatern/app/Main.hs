module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Interpret
import SyntaxTree
import System.Console.Haskeline

main :: IO ()
main = do
  ref <- newIORef []
  runInputT defaultSettings (loop ref)
  where
    loop :: IORef [Rule Text] -> InputT IO ()
    loop ref = do
      minput <- getInputLine "patatern> "
      case minput of
        Nothing -> pure ()
        Just "quit" -> pure ()
        Just "exit" -> pure ()
        Just ('l':'o':'a':'d':' ':files) ->
          catchAll (liftIO (handleLoadCmd ref files) >> loop ref) $ \e ->
            liftIO (print e) >> loop ref
        Just query ->
          finally (liftIO (runQuery ref (T.pack query))) (loop ref)

handleLoadCmd :: IORef [Rule Text] -> String -> IO ()
handleLoadCmd ref args = loadFiles ref (words args)
