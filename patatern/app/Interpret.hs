module Interpret where

import Control.Monad.IO.Class
import Control.Monad (unless)
import Data.Either (lefts, rights)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text.IO as T
import Evaluate
import SyntaxTree
import Parser
import Logic.Unify
import Instantiate

loadFiles :: IORef [Rule Text] -> [FilePath] -> IO ()
loadFiles ref files = do
  contents <- traverse T.readFile files
  let filesAndContents = zip files contents
      programs = map (uncurry parseProgram) filesAndContents
  unless (null (lefts programs)) $
    traverse_ putStrLn (lefts programs)
  writeIORef ref (concat (rights programs))

runQuery :: IORef [Rule Text] -> Text -> IO ()
runQuery ref input = do
  let eitherTerms = parseQueryTerms input
  case eitherTerms of
    Left err -> print err
    Right terms -> evalUnifyT $ do
      rules <- liftIO $ readIORef ref
      termsI <- runInstantiateTerms terms
      evalMany rules termsI
