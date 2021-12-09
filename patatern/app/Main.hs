{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import NeatInterpolation
import Interpret
import SyntaxTree
import System.Console.Haskeline

main :: IO ()
main = do
  TIO.putStrLn patatern
  TIO.putStrLn help
  putStrLn ""
  ref <- newIORef []
  runInputT defaultSettings (loop ref)
  where
    loop :: IORef [Rule Text] -> InputT IO ()
    loop ref = do
      minput <- getInputLine "?> "
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

patatern :: Text
patatern = [untrimming|
 _______  _______ _________ _______ _________ _______  _______  _       
(  ____ )(  ___  )\__   __/(  ___  )\__   __/(  ____ \(  ____ )( (    /|
| (    )|| (   ) |   ) (   | (   ) |   ) (   | (    \/| (    )||  \  ( |
| (____)|| (___) |   | |   | (___) |   | |   | (__    | (____)||   \ | |
|  _____)|  ___  |   | |   |  ___  |   | |   |  __)   |     __)| (\ \) |
| (      | (   ) |   | |   | (   ) |   | |   | (      | (\ (   | | \   |
| )      | )   ( |   | |   | )   ( |   | |   | (____/\| ) \ \__| )  \  |
|/       |/     \|   )_(   |/     \|   )_(   (_______/|/   \__/|/    )_)
|]

help :: Text
help = [trimming|
Welcome to heaven!

This is the interactive interpreter of patatern 0.1.0.0

You can set the active rules with the following command:
  load FILE1 FILE2...

You can run a query by typing a list of comma-separated terms to match against
the rules.
|]
