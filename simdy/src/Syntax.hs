module Syntax (Expr(..), Op(..), ExpType(..)) where
import Data.Semigroup (Min(Min))
import GHC.Generics (Associativity(LeftAssociative))

type Name = String

data Expr 
    = Variable Name
    | DefVar Name ExpType
    | Int Integer
    | Decimal Double
    | BinOp Op Expr Expr
    | Function Name [Expr] [Expr]
    | Call Name [Expr] 
    | If Expr [Expr] [Expr]
    deriving (Eq, Ord, Show)

data ExpType = I32 | U32 | I16 | U16 | DOUBLE | FLOAT deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  | Assignment
  | Less
  | Greater
  deriving (Eq, Ord, Show) 