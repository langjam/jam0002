{-# LANGUAGE OverloadedStrings #-}

module Builtin where

import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Logic.Unify
import SyntaxTree
import Text.Read (readMaybe)

evalBuiltin :: Term UVar -> UnifyT (Term UVar) IO ()
evalBuiltin (Symbol "write" :< Symbol s) = liftIO $ TIO.putStr s
evalBuiltin (Symbol "print" :< ts) = liftIO $ putStrLn (unwords (map show (termList ts)))
evalBuiltin (Symbol "getLine" :< Var r) = do
  line <- liftIO TIO.getLine
  void $ unify (Var r) (Symbol line)
evalBuiltin (Symbol "getInt" :< Var r) = do
  maybeI <- liftIO $ readMaybe . T.unpack <$> TIO.getLine
  case maybeI of
    Just i -> void $ unify (Var r) (Int i)
    Nothing -> liftIO $ putStrLn "Not an integer"
evalBuiltin (Int x :< Symbol "+" :< Int y :< Symbol "=" :< Var r) =
  void $ unify (Var r) (Int (x + y))
evalBuiltin (Int x :< Symbol "+" :< Var y :< Symbol "=" :< Int r) =
  void $ unify (Var y) (Int (r - x))
evalBuiltin (Var x :< Symbol "+" :< Int y :< Symbol "=" :< Int r) =
  void $ unify (Var x) (Int (r - y))
evalBuiltin t = liftIO $ putStrLn ("Unmatched pattern: " <> show t)

termList :: Term v -> [Term v]
termList (x :< y) = x : termList y
termList x = [x]
