module Lexer (lexer, parens, identifier, reserved, reservedOp, int', commaSep, braces, decimal', angles, brackets) where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Char (oneOf, char, digit, satisfy)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)

import qualified Text.Parsec.Token as Tok

import Syntax ()

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style where
    ops = [
        "+", "++", "*", "-", "--",
        "/", "%", "~",  "||","&&", 
        "<", ">", "==", "!=", "=", ":" 
        ]
    names = [
        "func", "load", "store","sizeof", "bitcast", "i32", "u32", 
        "i16", "u16", "float", "double", "if",
        "else", "for", "while"
        ]
    style = emptyDef {
        Tok.commentLine = "//",
        Tok.reservedOpNames = ops,
        Tok.reservedNames = names
    }


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

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
