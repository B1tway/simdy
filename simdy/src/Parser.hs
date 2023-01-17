module Parser (parseToplevel) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenLanguageDef(identLetter))
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Data.Functor.Identity

import Lexer ( lexer, parens, identifier, reserved, reservedOp, int', commaSep, braces, decimal', angles, brackets )
import Syntax ( Op(..), Expr(..), Type(..), PrimitiveType(..),Name, UnaryOp(..) )
import Data.Maybe (fromMaybe)
import Control.Exception (bracket)


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
    <|> try cast
    <|> try decimal
    <|> try int
    <|> try call
    <|> try variableDef
    <|> try variable
    <|> try ifelse
    <|> try while
    <|> parens expr

int :: Parser Expr
int = do Int <$> int'

decimal :: Parser Expr
decimal = do Decimal <$> decimal'

variableI32 :: Parser Type
variableI32 = do
    varType <- reserved "i32"
    return $ Primitive I32

variableU32 :: Parser Type
variableU32 = do
    varType <- reserved "u32"
    return $ Primitive U32

variableI16 :: Parser Type
variableI16 = do
    varType <- reserved "i16"
    return $ Primitive I16

variableU16 :: Parser Type
variableU16 = do
    varType <- reserved "u16"
    return $ Primitive U16

variableDouble :: Parser Type
variableDouble = do
    varType <- reserved "double"
    return $ Primitive DOUBLE

variableFloat :: Parser Type
variableFloat = do
    varType <- reserved "float"
    return $ Primitive FLOAT

variablePtr :: Parser Type
variablePtr = do
    p <- reserved "ptr"
    ptrType <- angles $ try variablePtr <|> try variableI32 <|> try variableU32 <|> try variableI16 <|> try variableU16 <|> try variableDouble <|> try variableFloat
    return $ Ptr ptrType

variableDef :: Parser Expr
variableDef = do
    varName <- identifier
    reservedOp ":"
    varType <- try variablePtr <|> try variableI32 <|> try variableU32 <|> try variableI16 <|> try variableU16 <|> try variableDouble <|> variableFloat 
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
    typeId <- angles $ try variableI32 <|> try variableU32 <|> try variableI16 <|> try variableU16 <|> try variableDouble <|> variableFloat
    CastOp typeId <$> (try idx <|> variable)

idx :: Parser Expr
idx = do 
    var <- variable
    pos <- brackets expr
    return $ IndexOp var pos

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
