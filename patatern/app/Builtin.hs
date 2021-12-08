{-# LANGUAGE OverloadedStrings #-}

module Builtin where

import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import Logic.Unify
import SyntaxTree

evalBuiltin :: Term UVar -> UnifyT (Term UVar) IO ()
evalBuiltin (Symbol "write" :< t) = liftIO $ putStr (show t)
evalBuiltin (Symbol "writeLn" :< t) = liftIO $ print t
evalBuiltin (Int x :< Symbol "+" :< Int y :< Symbol "=" :< Var r) =
  void $ unify (Var r) (Int (x + y))
evalBuiltin (Int x :< Symbol "+" :< Var y :< Symbol "=" :< Int r) =
  void $ unify (Var y) (Int (r - x))
evalBuiltin (Var x :< Symbol "+" :< Int y :< Symbol "=" :< Int r) =
  void $ unify (Var x) (Int (r - y))
evalBuiltin t = liftIO $ putStrLn ("Unmatched pattern: " <> show t)
