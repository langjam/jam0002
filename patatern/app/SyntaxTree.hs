{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module SyntaxTree where

import Control.Lens (Plated, children, transformM)
import Data.Data (Data)
import Data.Text (Text)
import Logic.Unify

-- | A term.
--   Terms are first parsed as (Term Text),
--   then instantiated to (Term UVar) before attempting unification.
data Term v
  = Symbol Text
  | Int Integer
  | Var v
  | (:<) (Term v) (Term v)
  | Lazy (Term v)
  deriving (Eq, Show, Data)

infixr 5 :<

instance Plated (Term UVar)

instance Unifiable (Term UVar) where
  getVar (Var v) = Just v
  getVar _ = Nothing
  transformTermM = transformM
  termChildren = children

-- | A rewrite-rule
data Rule v = Rule
  { lhs :: Term v,
    rhs :: Term v
  }
  deriving (Eq, Show)
