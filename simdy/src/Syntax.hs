module Syntax (Expr(..), Op(..)) where
import Data.Semigroup (Min(Min))

type Name = String
-- type Type = String 

data Expr 
    = Variable Name Expr
    | Int Integer
    | Decimal Double
    | Type Name
    | BinOp Op Expr Expr
    | Function Name [Expr] [Expr]
    | Call Name [Expr]
    deriving (Eq, Ord, Show)


data Op
  = Plus
  | Minus
  | Times
  | Divide
  | Assignment
  deriving (Eq, Ord, Show) 