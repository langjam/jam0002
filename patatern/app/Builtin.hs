{-# LANGUAGE OverloadedStrings #-}

module Builtin where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Logic.Unify
import SyntaxTree
import Text.Read (readMaybe)

evalBuiltin :: Term UVar -> UnifyT (Term UVar) IO ()
evalBuiltin ("write" :> Symbol s) = liftIO $ TIO.putStr s
evalBuiltin ("print" :> t) = liftIO $ print t
evalBuiltin ("getLine" :> Var r) = do
  line <- liftIO TIO.getLine
  void $ unify (Var r) (Symbol line)
evalBuiltin ("getInt" :> Var r) = do
  maybeI <- liftIO $ readMaybe . T.unpack <$> TIO.getLine
  case maybeI of
    Just i -> void $ unify (Var r) (Int i)
    Nothing -> do
      liftIO $ putStrLn "Not an integer"
      error "Not an integer"
evalBuiltin t@(Int x :> "+" :> Int y :> "=" :> Int r) =
  if x + y == r
    then pure ()
    else do
      liftIO $ putStrLn ("Wrong addition: " <> show t)
      error "Wrong addition"
evalBuiltin (Int x :> "+" :> Int y :> "=" :> Var r) =
  void $ unify (Var r) (Int (x + y))
evalBuiltin (Int x :> "+" :> Var y :> "=" :> Int r) =
  void $ unify (Var y) (Int (r - x))
evalBuiltin (Var x :> "+" :> Int y :> "=" :> Int r) =
  evalBuiltin (Int y :> "+" :> Var x :> Symbol "=" :> Int r)
evalBuiltin (x :> "+" :> y :> "=" :> r) = pure ()
evalBuiltin (r :> "=" :> x :> "+" :> y) =
  evalBuiltin (x :> "+" :> y :> "=" :> r)
evalBuiltin t = liftIO $ putStrLn ("Unmatched pattern: " <> show t)

evalAdditionTerms :: [Term UVar] -> Term UVar -> UnifyT (Term UVar) IO ()
evalAdditionTerms addends result = do
  let isVar Var{} = True
      isVar _ = False
      getInt (Int i) = Just i
      getInt _ = Nothing
      (vars, others) = partition isVar addends
      maybeInts = traverse getInt others
  case (vars, maybeInts, result) of
        ([var], Just ints, Int r) ->
          void $ unify var (Int (sum (r : map negate ints)))
        ([], Just ints, Var r) ->
          void $ unify (Var r) (Int (sum ints))
        _ -> pure ()
