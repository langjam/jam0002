{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module SyntaxTree where

import Control.Lens (Plated, children, transformM)
import Data.Data (Data)
import Data.List (intercalate)
import Data.String (IsString(..))
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
  | (:>) (Term v) (Term v)
  deriving (Eq, Data)

infixl 5 :>

emptyList :: Text
emptyList = T.pack "fin"

-- A list is defined as right-nested pairs ending with a (Symbol "fin")
-- If the term given is a list, then we convert it to an haskell list
-- of its elements, otherwise returns Nothing
convertToList :: Term v -> Maybe [Term v]
convertToList (Symbol s)
  | s == emptyList = Just []
  | otherwise      = Nothing

convertToList (l :> r) = (l :) <$> convertToList r
convertToList _ = Nothing

instance IsString (Term v) where
  fromString = Symbol . T.pack

instance ShowVar v => Show (Term v) where
  showsPrec _ (Symbol t) = showString (T.unpack t)
  showsPrec _ (Int n) = shows n
  showsPrec _ (Var v) = showString "#" . showVarS v
  -- Pairs are left associative, therefore if the left
  -- child if a pair is itself a pair we must insert parentheses.
  -- we use the prec parameter to gracefully handle this.
  -- showParen adds a pair of additional parentheses when
  -- the condition is true.
  -- We want ad hoc rules for handling list pretty printing
  showsPrec prec t@((:>) l r) =
    case convertToList t of
      Just list -> showChar '[' . showsList list . showChar ']'
      Nothing -> showsPair l r
    where
      pair_prec = 1
      list_prec = 5

      showsList []     = showString ""
      showsList [x]    = showsPrec list_prec x
      showsList (x:xs) = showsPrec list_prec x
                       . showChar ' '
                       . showsList xs

      showsPair l r = showParen (pair_prec < prec)
                    $ showsPrec pair_prec l
                    . showChar ' '
                    . showsPrec (pair_prec + 1) r

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

instance ShowVar v => Show (Rule v) where
  show (Rule lhs rhs) = show lhs ++ ":\n" ++ rhs' ++ "."
    where
      rhs' = intercalate ",\n" $ map (\t -> "  " ++ show t) rhs

class ShowVar v where
  showVarS :: v -> ShowS

instance ShowVar Text where
  showVarS = shows

instance ShowVar UVar where
  showVarS (UVar i) = shows i
