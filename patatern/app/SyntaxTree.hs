{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module SyntaxTree where

import Control.Lens (Plated, children, transformM)
import Data.Data (Data)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Logic.Unify

-- | A term.
--   Terms are first parsed as (Term Text),
--   then instantiated to (Term UVar) before attempting unification.
data Term v
  = Symbol Text
  | Int Integer
  | Var v
  | (:<) (Term v) (Term v)
  deriving (Eq, Data)

infixr 5 :<

deriving instance Show (Term Text)

instance Show (Term UVar) where
  showsPrec _ (Symbol t) = showString (T.unpack t)
  showsPrec _ (Int n) = shows n
  showsPrec _ (Var (UVar n)) = showString "#" . shows n
  -- Pairs are right associative, therefore if the left
  -- child if a pair is itself a pair we must insert parentheses.
  -- we use the prec parameter to gracefully handle this.
  -- showParen adds a pair of additional parentheses when
  -- the condition is true
  showsPrec prec ((:<) l r) =
    showParen (pair_prec < prec) $
      showsPrec (pair_prec + 1) l
        . showChar ' '
        . showsPrec pair_prec r
    where
      pair_prec = 1

instance Plated (Term UVar)

instance Unifiable (Term UVar) where
  getVar (Var v) = Just v
  getVar _ = Nothing
  transformTermM = transformM
  termChildren = children

-- | A logic rule
data Rule v = Rule
  { lhs :: Term v,
    rhs :: [Term v]
  }
  deriving (Eq)

deriving instance Show (Rule Text)

instance Show (Rule UVar) where
  show (Rule lhs rhs) = show lhs ++ ":\n" ++ rhs' ++ "."
    where
      rhs' = intercalate ",\n" $ map (\t -> "  " ++ show t) rhs
