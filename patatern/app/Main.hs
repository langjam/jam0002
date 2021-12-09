{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Interpret
import NeatInterpolation
import SyntaxTree
import System.Console.Haskeline
import System.Directory (XdgDirectory (..), getXdgDirectory)
import System.FilePath ((</>))
import System.Process (spawnCommand,waitForProcess)

main :: IO ()
main = do
  TIO.putStrLn patatern
  TIO.putStrLn help
  putStrLn ""
  ref <- newIORef []
  historyPath <- getHistoryFilePath
  let settings = defaultSettings {historyFile = Just historyPath}
  runInputT settings (loop ref)
  where
    loop :: IORef [Rule Text] -> InputT IO ()
    loop ref = do
      minput <- getInputLine "?> "
      case minput of
        Nothing -> pure ()
        Just "quit" -> pure ()
        Just "exit" -> pure ()
        Just (':' : cmd) -> try $
          case cmd of
            ('!':rest)                -> handleShellCmd rest
            (words -> ("load":files)) -> handleLoadCmd ref files
        Just query ->
          finally (withInterrupt $ liftIO (runQuery ref (T.pack query))) (loop ref)
      where
        try handler =
          catchAll (liftIO handler) (liftIO . print) >> loop ref

getHistoryFilePath :: IO FilePath
getHistoryFilePath = do
  dir <- getXdgDirectory XdgData "patatern"
  pure (dir </> "history")

handleShellCmd :: String -> IO ()
handleShellCmd xs = spawnCommand xs >>= waitForProcess >> pure ()

handleLoadCmd :: IORef [Rule Text] -> [String] -> IO ()
handleLoadCmd = loadFiles

patatern :: Text
patatern =
  [untrimming|
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
help =
  [trimming|
Welcome to heaven!

This is the interactive interpreter of patatern 0.1.0.0

You can set the active rules with the following command:
  load FILE1 FILE2...

Or you can run a query directly.
|]
