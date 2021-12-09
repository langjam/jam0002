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

evalMany :: [Term UVar] -> [Rule Text] -> [Term UVar] -> UnifyT (Term UVar) IO ()
evalMany query1 rules = traverse_ (eval query1 rules)

eval :: [Term UVar] -> [Rule Text] -> Term UVar -> UnifyT (Term UVar) IO ()
eval query1 rules query = do
  query' <- applyBindingsOrDie query
  maybeRule <- firstMatchingRule rules query'
  case maybeRule of
    Just rule -> evalMany query1 rules (rhs rule)
    Nothing -> evalBuiltin =<< applyBindingsOrDie query'

firstMatchingRule ::
  [Rule Text] ->
  Term UVar ->
  UnifyT (Term UVar) IO (Maybe (Rule UVar))
firstMatchingRule [] _ = pure Nothing
firstMatchingRule (rule : rules) query = do
  ruleI <- runInstantiateRule rule
  valid <- subsumes_ (lhs ruleI) query
  if valid
    then do
      unifies <- unifyOrUndo_ (lhs ruleI) query
      if unifies
        then pure $ Just ruleI
        else firstMatchingRule rules query
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
