module Syntax (Expr(..), Op(..), ExpType(..), Name, UnaryOp(..)) where
import Data.Semigroup (Min(Min))
import GHC.Generics (Associativity(LeftAssociative))

type Name = String

data Expr 
    = Variable Name
    | DefVar Name ExpType
    | Int Integer
    | Decimal Double
    | UnOp UnaryOp Expr
    | BinOp Op Expr Expr
    | Function Name [Expr] [Expr]
    | Call Name [Expr] 
    | If Expr [Expr] [Expr]
    | While Expr [Expr]
    | IndexOp Expr Expr
    | CastOp ExpType Expr
    deriving (Eq, Ord, Show)

data ExpType = I32 | U32 | I16 | U16 | DOUBLE | FLOAT | Ptr ExpType deriving (Eq, Ord, Show)

data Op 
    = Plus | Minus | Times | Divide | Assign | 
    Less | Greater | Equal | NotEqual | OR | 
    AND deriving (Eq, Ord, Show) 

data UnaryOp = Increment | Decrement | UnMinus deriving(Eq, Ord, Show)
