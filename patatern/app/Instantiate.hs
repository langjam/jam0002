{-# LANGUAGE FlexibleContexts #-}

module Instantiate where

import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Logic.Unify
import SyntaxTree

-- | Instantiate the LHS and the RHS of a rule
instantiateRule ::
  UState (Term UVar) ->
  Rule Text ->
  (Rule UVar, UState (Term UVar))
instantiateRule s (Rule l r) =
  flip evalState mempty $
    runUnifyT
      s
      (Rule <$> instantiateTerm l <*> instantiateTerm r)

-- | Instantiate a term,
--   ie. convert all the symbolic variables to unification variables
instantiateTerm ::
  MonadState (Map Text UVar) m =>
  Term Text ->
  UnifyT (Term UVar) m (Term UVar)
instantiateTerm (Var name) = do
  vars <- get
  case Map.lookup name vars of
    Just v -> pure (Var v)
    Nothing -> do
      v <- newVar
      modify' (Map.insert name v)
      pure $ Var v
instantiateTerm (Symbol s) = pure $ Symbol s
instantiateTerm (Int i) = pure $ Int i
instantiateTerm (t1 :< t2) = (:<) <$> instantiateTerm t1 <*> instantiateTerm t2
instantiateTerm (Lazy t) = Lazy <$> instantiateTerm t
