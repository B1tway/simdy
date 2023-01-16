module Parser (parseToplevel) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenLanguageDef(identLetter))
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Data.Functor.Identity

import Lexer ( lexer, parens, identifier, reserved, reservedOp, int', commaSep, braces, decimal' )
import Syntax ( Op(..), Expr(..) )




binary :: String -> Op -> Ex.Assoc -> Ex.Operator String () Data.Functor.Identity.Identity Expr
binary s f  = Ex.Infix (reservedOp s >> return (BinOp f))

table :: [[Ex.Operator String () Data.Functor.Identity.Identity Expr]]
table = [
    [binary "*" Times Ex.AssocLeft, binary "/" Divide Ex.AssocLeft],
    [binary "+" Plus Ex.AssocLeft, binary "-" Minus Ex.AssocLeft],
    [binary "=" Assignment Ex.AssocRight]
    ]

factor :: Parser Expr
factor = 
    try decimal 
    <|> try int
    <|> try variable
    <|> try function
    <|> parens expr

int :: Parser Expr
int = do Int <$> int'

decimal :: Parser Expr
decimal = do Decimal <$> decimal'

variableI32 :: Parser Expr
variableI32 = do
    varType <- reserved "i32"
    return $ Type "i32"

variableU32 :: Parser Expr
variableU32 = do
    varType <- reserved "u32"
    return $ Type "u32"

variableI16 :: Parser Expr
variableI16 = do
    varType <- reserved "i16"
    return $ Type "i16"

variableU16 :: Parser Expr
variableU16 = do
    varType <- reserved "u16"
    return $ Type "u16"

variableDouble :: Parser Expr
variableDouble = do
    varType <- reserved "double"
    return $ Type "double"

variableFloat :: Parser Expr
variableFloat = do
    varType <- reserved "float"
    return $ Type "float"

variable :: Parser Expr
variable = do
    varName <- identifier
    reservedOp ":"
    varType <- try variableI32 <|> try variableU32 <|> try variableI16 <|> try variableU16 <|> try variableDouble <|> variableFloat
    return $ Variable varName varType

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

function :: Parser Expr
function = do
  reserved "func"
  name <- identifier
  args <- parens $ commaSep variable
  body <- braces $ many expr
  return $ Function name args body

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
