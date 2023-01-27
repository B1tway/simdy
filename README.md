Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования

«Национальный исследовательский университет ИТМО»

---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__

__Лабораторная работа №4__

по Функциональному программированию

Выполнили: Лавлинский М. С. Шукшов А. И.

Группа: P34112, P34102

Преподаватель: Пенской Александр Владимирович

###### Санкт-Петербург
###### 2023 г.

---

## Требования к разработанному ПО

* Трансляция иcходного когда в LLVM IR
* Поддержка векторных типов данных 

## Ключевые элементы реализации с минимальными комментариями


```

-- AST для ЯП

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
    | MemOp MemoryOp
    deriving (Eq, Ord, Show)

data Type = Primitive  PrimitiveType | Ptr Type | VectType Expr PrimitiveType  deriving (Eq, Ord, Show)

data PrimitiveType  = I32 | U32 | I16 | U16 | DOUBLE | FLOAT deriving (Eq, Ord, Show)

data Op 
    = Plus | Minus | Times | Divide | Assign | 
    Less | Greater | Equal | NotEqual | OR | 
    AND deriving (Eq, Ord, Show) 

data UnaryOp = Increment | Decrement | UnMinus deriving(Eq, Ord, Show)

data MemoryOp = Load Type Expr Expr | Store Type Expr Expr deriving (Eq, Ord, Show)

```

```

-- лексер, релизованный с помощью Parsec, модуль содержит все базовые парсеры, ключевые слова и операторы языка

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
        "<", ">", "==", "!=", "=", 
        ":", "(", ")", "{", "}",
        "[", "]", "," 
        ]
    names = [
        "func", "load", "store","sizeof", "bitcast", "i32", "u32", 
        "i16", "u16", "float", "double", "if",
        "else", "for", "while", "ptr", "vec"
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

```

```

-- Парсер, который разбирает программу, поданную на вход и строит AST

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


```
## Ввод/вывод программы
### Simdy input
```c
func foo(a:vec<8, i32>, b:vec<8, i32>, c : ptr<i32>) {
    sum : vec<8, i32> = a + b
    store<i32>(c, sum)
}

```
### LLVM IR
```llvm
define external ccc  void @foo(<8 x i32>  %arg_a_0, <8 x i32>  %arg_b_0, i32*  %arg_c_0)    {
Body_0:
  %a_0 = alloca <8 x i32>, align 4 
  store  <8 x i32> %arg_a_0, <8 x i32>* %a_0, align 4 
  %b_0 = alloca <8 x i32>, align 4 
  store  <8 x i32> %arg_b_0, <8 x i32>* %b_0, align 4 
  %c_0 = alloca i32*, align 4 
  store  i32* %arg_c_0, i32** %c_0, align 4 
  %sum_0 = alloca <8 x i32>, align 4 
  %0 = load  <8 x i32>, <8 x i32>* %a_0, align 4 
  %1 = load  <8 x i32>, <8 x i32>* %b_0, align 4 
  %2 = add   <8 x i32> %0, %1 
  store  <8 x i32> %2, <8 x i32>* %sum_0, align 4 
  %3 = load  i32*, i32** %c_0, align 4 
  %4 = load  <8 x i32>, <8 x i32>* %sum_0, align 4 
  store  <8 x i32> %4, i32* %3, align 4 
  ret void 
}

```
### x86-64
```nasm
foo:                                    # @foo
        movdqa  xmmword ptr [rsp - 24], xmm1
        movdqa  xmmword ptr [rsp - 40], xmm0
        movdqa  xmmword ptr [rsp - 56], xmm3
        movdqa  xmmword ptr [rsp - 72], xmm2
        mov     qword ptr [rsp - 112], rdi
        paddd   xmm0, xmm2
        paddd   xmm1, xmm3
        movdqa  xmmword ptr [rsp - 88], xmm1
        movdqa  xmmword ptr [rsp - 104], xmm0
        movdqu  xmmword ptr [rdi + 16], xmm1
        movdqu  xmmword ptr [rdi], xmm0
        ret
}
```
## Выводы

В ходе этой работы мы поработали с Parsec, изучили как можно относительно легко и быстро писать парсеры с помощью языка Haskell, используя парсер-комбинаторы. Также... 
