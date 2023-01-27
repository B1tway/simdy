module Lexer
       (lexer, parens, identifier, reserved, reservedOp, int', commaSep, braces,
        decimal', angles, brackets)
       where
import Syntax ()
import Text.Parsec.Language (emptyDef)

import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops
          = ["+", "++", "*", "-", "--", "/", "%", "~", "||", "&&", "<", ">",
             "==", "!=", "=", ":", "(", ")", "{", "}", "[", "]", ","]

        names
          = ["func", "load", "store", "sizeof", "bitcast", "i32", "u32", "i16",
             "u16", "float", "double", "if", "else", "for", "while", "ptr",
             "vec"]

        style
          = emptyDef{Tok.commentLine = "//", Tok.reservedOpNames = ops,
                     Tok.reservedNames = names}

int' :: Parser Integer
int' = Tok.integer lexer

decimal' :: Parser Double
decimal' = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
