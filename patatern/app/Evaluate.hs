{-# LANGUAGE PatternSynonyms #-}

module Evaluate where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.IORef
import Data.Sequence (Seq, pattern Empty, pattern (:<|))
import Data.Text (Text)
import Instantiate
import Logic.Unify
import SyntaxTree
import Utils

eval :: IORef (Seq (Rule Text)) -> Term Text -> UnifyT (Term UVar) IO ()
eval rulesRef query = do
  rules <- liftIO $ readIORef rulesRef
  let maybeRHS = firstMatchingRuleRHS rules query
  -- TODO Continue
  pure ()

firstMatchingRuleRHS ::
  Seq (Rule Text) ->
  Term Text ->
  UnifyT (Term UVar) IO (Maybe [Term Text])
firstMatchingRuleRHS Empty _ = pure Nothing
firstMatchingRuleRHS (rule :<| rules) query = do
  res <- tryRule
  if res
    then pure $ Just (rhs rule)
    else firstMatchingRuleRHS rules query
  where
    tryRule = flip evalStateT mempty $ do
      queryI <- instantiateTerm query
      lhsI <- instantiateTerm (lhs rule)
      lift $ unifyOrUndo_ queryI lhsI
