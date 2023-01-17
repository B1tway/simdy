--{-# LANGUAGE BlockArguments #-}
module Parser (parseToplevel) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenLanguageDef(identLetter))
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Data.Functor.Identity

import Lexer ( lexer, parens, identifier, reserved, reservedOp, int', commaSep, braces, decimal', angles, brackets )
import Syntax ( Op(..), Expr(..), Type(..), PrimitiveType(..),Name, UnaryOp(..), MemoryOp(..) )
import Data.Maybe (fromMaybe)
import Control.Exception (bracket)
import Control.Applicative ()


binary :: String -> Op -> Ex.Assoc -> Ex.Operator String () Data.Functor.Identity.Identity Expr
binary s f  = Ex.Infix (reservedOp s >> return (BinOp f))


prefix :: String -> UnaryOp -> Ex.Operator String () Data.Functor.Identity.Identity Expr
prefix s f = Ex.Prefix (reservedOp s >> return (UnOp f))

table :: [[Ex.Operator String () Data.Functor.Identity.Identity Expr]]
table = [
    [prefix "-" UnMinus, prefix "++" Increment, prefix "--" Decrement],
    [binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft],
    [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft],
    [binary "<" Less Ex.AssocNone, binary ">" Greater Ex.AssocNone],
    [binary "==" Equal Ex.AssocLeft, binary "!=" NotEqual Ex.AssocLeft],
    [binary "||" OR Ex.AssocLeft, binary "&&" AND Ex.AssocLeft],
    [binary "=" Assign Ex.AssocRight]
    ]

factor :: Parser Expr
factor =
    try idx
    <|> function
    <|> try variableDef
    <|> try cast
    <|> try load
    <|> try store
    <|> try decimal
    <|> try int
    <|> try call
    <|> try variable
    <|> try ifelse
    <|> try while
    <|> parens expr

int :: Parser Expr
int = do Int <$> int'

decimal :: Parser Expr
decimal = do Decimal <$> decimal'

variableI32 :: Parser PrimitiveType
variableI32 = do
    varType <- reserved "i32"
    return I32

variableU32 :: Parser PrimitiveType
variableU32 = do
    varType <- reserved "u32"
    return U32

variableI16 :: Parser PrimitiveType
variableI16 = do
    varType <- reserved "i16"
    return I16

variableU16 :: Parser PrimitiveType
variableU16 = do
    varType <- reserved "u16"
    return U16

variableDouble :: Parser PrimitiveType
variableDouble = do
    varType <- reserved "double"
    return DOUBLE

variableFloat :: Parser PrimitiveType
variableFloat = do
    varType <- reserved "float"
    return FLOAT

parsePrimitive :: Parser PrimitiveType
parsePrimitive = do
    try variableI32 <|> try variableU32 <|> try variableI16 <|> try variableU16 <|> try variableDouble <|> variableFloat

variablePtr :: Parser Type
variablePtr = do
    p <- reserved "ptr"
    ptrType <- angles $ try variablePtr <|> (Primitive <$> parsePrimitive)
    return $ Ptr ptrType

-- awful
variableVec :: Parser Type
variableVec = do
    reserved "vec"
    reservedOp "<"
    sz <- int
    reservedOp ","
    ty <- parsePrimitive
    reservedOp ">"
    return $ VectType sz ty

variableDef :: Parser Expr
variableDef = do
    varName <- identifier
    reservedOp ":"
    varType <- try variablePtr <|> try variableVec <|> (Primitive <$> parsePrimitive)
    return $ DefVar varName varType

variable :: Parser Expr
variable = do
    Variable <$> identifier

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

function :: Parser Expr
function = do
  reserved "func"
  name <- identifier
  args <- parens $ commaSep variableDef
  body <- braces $ many expr
  return $ Function name args body

call :: Parser Expr
call = do
    name <- identifier
    args <- parens $ commaSep variableDef
    return $ Call name args

ifelse :: Parser Expr
ifelse = do
    reserved "if"
    cond <- parens expr
    tr <- braces $ many expr
    fl <- optionMaybe $ do
        reserved "else"
        braces $ many expr
    return $ If cond tr (fromMaybe [] fl)

while :: Parser Expr
while = do
    reserved "while"
    cond <- parens expr
    body <- braces $ many expr
    return $ While cond body

cast :: Parser Expr
cast = do
    reserved "bitcast"
    typeId <- angles $ try variablePtr <|> try variableVec <|> (Primitive <$> parsePrimitive)
    CastOp typeId <$> (try idx <|> variable)

idx :: Parser Expr
idx = do
    var <- variable
    pos <- brackets expr
    return $ IndexOp var pos

memOp' :: Parser ((Type, Expr), Expr)
memOp' = do
    ty <- angles $ try variablePtr <|> try variableVec <|> (Primitive <$> parsePrimitive)
    reservedOp "("
    addr <- variable
    reservedOp ","
    off <- try variable <|> int
    reservedOp ")"
    return ((ty, addr), off)

load :: Parser Expr
load = do
    reserved "load"
    MemOp . uncurry (uncurry Load) <$> memOp'

store :: Parser Expr
store = do
    reserved "store"
    MemOp . uncurry (uncurry Store) <$> memOp'


defn :: Parser Expr
defn =
    try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do defn

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel = parse (contents toplevel) "<stdin>"
