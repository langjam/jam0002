{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import Interpret
import NeatInterpolation
import SyntaxTree
import System.Console.Haskeline
import System.Directory (XdgDirectory (..), getXdgDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Process (spawnCommand,waitForProcess)

main :: IO ()
main = do
  TIO.putStrLn patatern
  TIO.putStrLn help
  putStrLn ""
  ref <- newIORef []
  historyPath <- getHistoryFilePath
  let settings = setComplete completeKeyword $
                   defaultSettings { historyFile    = Just historyPath
                                   , autoAddHistory = True }
  runInputT settings (loop ref)
  where
    loop :: IORef [Rule Text] -> InputT IO ()
    loop ref = do
      minput <- getInputLine "?> "
      case minput of
        Nothing -> pure ()
        Just "quit" -> pure ()
        Just "exit" -> pure ()
        Just (':' : '!' : cmd) -> try (handleShellCmd cmd)
        Just (':' : cmd) -> try $
          case words cmd of
            ("load":files) -> handleLoadCmd ref files
            ("rules":_   ) -> handleRulesCmd ref
        Just query ->
          finally (withInterrupt $ liftIO (runQuery ref (T.pack query))) (loop ref)
      where
        try handler =
          catchAll (liftIO handler) (liftIO . print) >> loop ref

getHistoryFilePath :: IO FilePath
getHistoryFilePath = do
  dir <- getXdgDirectory XdgData "patatern"
  createDirectoryIfMissing True dir
  pure (dir </> "history")

handleShellCmd :: String -> IO ()
handleShellCmd xs = spawnCommand xs >>= waitForProcess >> pure ()

handleLoadCmd :: IORef [Rule Text] -> [String] -> IO ()
handleLoadCmd = loadFiles

handleRulesCmd :: IORef [Rule Text] -> IO ()
handleRulesCmd ref = readIORef ref >>= putStrLn . L.intercalate "\n\n" . map show

completeKeyword = completeWord Nothing " \t:" action
  where
    action :: String -> IO [Completion]
    action xs =
      do files <- listFiles xs
         pure (map simpleCompletion (find xs keywords) ++ files)

    find xs = filter (xs `L.isPrefixOf`)
    keywords = ["load","rules"]

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

This is the interactive interpreter for patatern version 0.1.0.0

Commands available from the prompt:

  <query>                 run a query
  :load <file> ...        set the active rules
  :rules                  show the active rules
  :!<cmd> [<args>]        launch a shell command

|]
