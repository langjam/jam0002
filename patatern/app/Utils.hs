{-# LANGUAGE FlexibleContexts #-}

module Utils where

import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Text (Text)
import Logic.Unify
import SyntaxTree

evalUnifyTState ::
  Monoid s =>
  UnifyT t (State s) a ->
  a
evalUnifyTState = flip evalState mempty . evalUnifyT

runUnifyTState ::
  Monoid s =>
  UState t ->
  UnifyT t (State s) a ->
  (a, UState t)
runUnifyTState s = flip evalState mempty . runUnifyT s
