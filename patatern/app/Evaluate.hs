module Evaluate where

import Builtin
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Foldable
import Data.IORef
import Data.Text (Text)
import Instantiate
import Logic.Unify
import SyntaxTree

evalMany :: [Rule Text] -> [Term UVar] -> UnifyT (Term UVar) IO ()
evalMany rules = traverse_ (eval rules)

eval :: [Rule Text] -> Term UVar -> UnifyT (Term UVar) IO ()
eval rules query = do
  query' <- applyBindingsOrDie query
  maybeRule <- firstMatchingRule rules query'
  case maybeRule of
    Just rule -> evalMany rules (rhs rule)
    Nothing -> evalBuiltin =<< applyBindingsOrDie query'

firstMatchingRule ::
  [Rule Text] ->
  Term UVar ->
  UnifyT (Term UVar) IO (Maybe (Rule UVar))
firstMatchingRule [] _ = pure Nothing
firstMatchingRule (rule : rules) query = do
  ruleI <- runInstantiateRule rule
  unifies <- unifyOrUndo_ (lhs ruleI) query
  if unifies
    then pure $ Just ruleI
    else firstMatchingRule rules query

applyBindingsOrDie ::
  (Monad m, Unifiable t, Show t) =>
  t ->
  UnifyT t m t
applyBindingsOrDie t = do
  maybeT' <- applyBindings t
  case maybeT' of
    Just t' -> pure t'
    Nothing -> error ("Cyclic term: " <> show t)
