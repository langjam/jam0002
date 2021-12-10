{-# LANGUAGE OverloadedStrings #-}

module Interpret where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Either (lefts, rights)
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Evaluate
import Instantiate
import Logic.Unify
import Parser
import SyntaxTree

loadFiles :: IORef [Rule Text] -> [FilePath] -> IO ()
loadFiles ref files = do
  contents <- traverse TIO.readFile files
  let filesAndContents = zip files contents
      programs = map (uncurry parseProgram) filesAndContents
  unless (null (lefts programs)) $
    traverse_ putStrLn (lefts programs)
  writeIORef ref (concat (rights programs))

runQuery :: IORef [Rule Text] -> Text -> IO ()
runQuery ref input = do
  let eitherTerms = parseQueryTerms input
  case eitherTerms of
    Left err -> putStrLn err
    Right terms -> evalUnifyT $ do
      rules <- liftIO $ readIORef ref
      (termsI, varMap) <- runInstantiateTerms terms
      evalMany rules termsI
      responseTerms <- traverse applyBindingsOrDie termsI
      for_ (Map.toList varMap) $ \(name, var) -> do
        val <- applyBindingsOrDie (Var var)
        liftIO $ TIO.putStrLn (name <> " = " <> T.pack (show val) <> ".")
