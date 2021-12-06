module SyntaxTree where

import Data.Text (Text)

data Term v
  = Symbol Text
  | Int Integer
  | Var v
  | (:<) (Term v) (Term v)
  | Lazy (Term v)
  deriving (Eq, Show)
 
infixr 5 :<
 
data Rule v = Rule
  { lhs :: Term v,
    rhs :: Term v
  }
  deriving (Eq, Show)
