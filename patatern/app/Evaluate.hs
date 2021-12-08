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

eval :: Seq (Rule Text) -> Term UVar -> UnifyT (Term UVar) IO ()
eval rules query = do
  maybeRule <- firstMatchingRule rules query
  case maybeRule of
    Just rule -> traverse_ (eval rules) (rhs rule)
    Nothing -> undefined -- evalBuiltin query
  pure ()

firstMatchingRule ::
  Seq (Rule Text) ->
  Term UVar ->
  UnifyT (Term UVar) IO (Maybe (Rule UVar))
firstMatchingRule Empty _ = pure Nothing
firstMatchingRule (rule :<| rules) query = do
  ruleI <- runInstantiateRule rule
  res <- unifyOrUndo_ query (lhs ruleI)
  if res
    then pure $ Just ruleI
    else firstMatchingRule rules query
