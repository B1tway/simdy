module Syntax (Expr(..), Op(..), Type(..), PrimitiveType(..), Name, UnaryOp(..)) where
import Data.Semigroup (Min(Min))
import GHC.Generics (Associativity(LeftAssociative))

type Name = String

data Expr 
    = Variable Name
    | DefVar Name Type
    | Int Integer
    | Decimal Double
    | UnOp UnaryOp Expr
    | BinOp Op Expr Expr
    | Function Name [Expr] [Expr]
    | Call Name [Expr] 
    | If Expr [Expr] [Expr]
    | While Expr [Expr]
    | IndexOp Expr Expr
    | CastOp Type Expr
    | MemOp Name Type Expr Expr 
    deriving (Eq, Ord, Show)

data Type = Primitive  PrimitiveType | Ptr Type | VectType Expr PrimitiveType  deriving (Eq, Ord, Show)

data PrimitiveType  = I32 | U32 | I16 | U16 | DOUBLE | FLOAT deriving (Eq, Ord, Show)

data Op 
    = Plus | Minus | Times | Divide | Assign | 
    Less | Greater | Equal | NotEqual | OR | 
    AND deriving (Eq, Ord, Show) 

data UnaryOp = Increment | Decrement | UnMinus deriving(Eq, Ord, Show)
