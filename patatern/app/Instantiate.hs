{-# LANGUAGE FlexibleContexts #-}

module Instantiate where

import Control.Monad.State
import Data.Functor.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Logic.Unify
import SyntaxTree

-- | Instantiate the LHS and the RHS of a rule
instantiateRule ::
  Monad m =>
  Rule Text ->
  StateT (Map Text UVar) (UnifyT (Term UVar) m) (Rule UVar)
instantiateRule (Rule l r) = Rule <$> instantiateTerm l <*> traverse instantiateTerm r

-- | Instantiate a term,
--   ie. convert all the symbolic variables to unification variables
instantiateTerm ::
  Monad m =>
  Term Text ->
  StateT (Map Text UVar) (UnifyT (Term UVar) m) (Term UVar)
instantiateTerm (Var name) = do
  vars <- get
  case Map.lookup name vars of
    Just v -> pure (Var v)
    Nothing -> do
      v <- lift newVar
      modify' (Map.insert name v)
      pure $ Var v
instantiateTerm (Symbol s) = pure $ Symbol s
instantiateTerm (Int i) = pure $ Int i
instantiateTerm (t1 :< t2) = (:<) <$> instantiateTerm t1 <*> instantiateTerm t2
instantiateTerm (Lazy t) = Lazy <$> instantiateTerm t
