{-# LANGUAGE OverloadedStrings #-}

module Builtin where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.List (partition)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Logic.Unify
import SyntaxTree hiding (lhs, rhs)
import Text.Read (readMaybe)

evalBuiltin :: Term UVar -> UnifyT (Term UVar) IO ()
evalBuiltin ("write" :> t) = liftIO $ putStr (show t)
evalBuiltin ("print" :> t) = liftIO $ print t
evalBuiltin ("getSymbol" :> Var r) = do
  line <- liftIO TIO.getLine
  void $ unify (Var r) (Symbol line)
evalBuiltin ("getInt" :> Var r) = do
  maybeI <- liftIO $ readMaybe . T.unpack <$> TIO.getLine
  case maybeI of
    Just i -> void $ unify (Var r) (Int i)
    Nothing -> do
      liftIO $ putStrLn "Not an integer"
      error "Not an integer"
evalBuiltin t@(Int n1 :> ">" :> Int n2 :> "is" :> Var b)
  | n1 > n2   = void $ unify (Var b) (Symbol "true")
  | otherwise = void $ unify (Var b) (Symbol "false")
evalBuiltin t@(Int n1 :> "<" :> Int n2 :> "is" :> Var b)
  | n1 < n2   = void $ unify (Var b) (Symbol "true")
  | otherwise = void $ unify (Var b) (Symbol "false")
evalBuiltin t@(_ :> "+" :> _ :> "=" :> _) = evalAddition t
evalBuiltin t@(_ :> "-" :> _ :> "=" :> _) = evalAddition t
evalBuiltin t@(_ :> "+" :> _) = evalAddition t
evalBuiltin t@(_ :> "-" :> _) = evalAddition t
evalBuiltin t = liftIO $ putStrLn ("Unmatched pattern: " <> show t)

evalAddition :: Term UVar -> UnifyT (Term UVar) IO ()
evalAddition t = case getAdditionTerms [] [] (Right t) of
  (x : xs, y : ys) -> evalAdditionTerms (x : xs) (y : ys)
  (_, _) -> do
    liftIO $ putStrLn ("Invalid operation: " <> show t)
    error "Invalid operation"

getAdditionTerms :: [Term UVar]
                 -> [Term UVar]
                 -> Either (Term UVar) (Term UVar)
                 -> ([Term UVar], [Term UVar])
getAdditionTerms lhs rhs (Left (Int i :> "+" :> x)) = (Int i : x : lhs, rhs)
getAdditionTerms lhs rhs (Left (Int i :> "-" :> x)) = (Int i : lhs, x : rhs)
getAdditionTerms lhs rhs (Right (Int i :> "=" :> x)) = (Int i : lhs, x : rhs)
getAdditionTerms lhs rhs (Left (Var v :> "+" :> x)) = (Var v : x : lhs, rhs)
getAdditionTerms lhs rhs (Left (Var v :> "-" :> x)) = (Var v : lhs, x : rhs)
getAdditionTerms lhs rhs (Right (Var v :> "=" :> x)) = (Var v : lhs, x : rhs)
getAdditionTerms lhs rhs (Right (xs :> "+" :> x)) =
  getAdditionTerms lhs (x : rhs) (Right xs)
getAdditionTerms lhs rhs (Right (xs :> "-" :> x)) =
  getAdditionTerms (x : lhs) rhs (Right xs)
getAdditionTerms lhs rhs (Right (xs :> "=" :> x)) =
  getAdditionTerms lhs (x : rhs) (Left xs)
getAdditionTerms lhs rhs (Left (xs :> "+" :> x)) =
  getAdditionTerms (x : lhs) rhs (Left xs)
getAdditionTerms lhs rhs (Left (xs :> "-" :> x)) =
  getAdditionTerms lhs (x : rhs) (Left xs)
getAdditionTerms _ _ _ = ([], [])

evalAdditionTerms :: [Term UVar] -> [Term UVar] -> UnifyT (Term UVar) IO ()
evalAdditionTerms lhs rhs = do
  let isVar Var{} = True
      isVar _ = False
      getInt (Int i) = Just i
      getInt _ = Nothing
      (varsL, othersL) = partition isVar lhs
      (varsR, othersR) = partition isVar rhs
      maybeIntsL = traverse getInt othersL
      maybeIntsR = traverse getInt othersR
  case (varsL, maybeIntsL, varsR, maybeIntsR) of
        ([var], Just intsL, [], Just intsR) ->
          void $ unify var (Int (negate (sum intsL) + sum intsR))
        ([], Just intsL, [var], Just intsR) ->
          void $ unify var (Int (sum intsL + negate (sum intsR)))
        ([], Just intsL, [], Just intsR) ->
          if sum (intsL) == sum intsR
            then pure ()
            else do
              liftIO $ putStrLn "Go back to school!"
              error "Go back to school!"
        _ -> pure ()
