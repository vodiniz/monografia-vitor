module PyParser where

import Control.Applicative hiding (many, some)
import Prelude

import PySyntax

import Control.Monad (void)
import Control.Monad.Combinators.Expr

import Data.Void (Void)
import Data.Word (Word32)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Pos (Pos, pos1)

-- definition of a type for parsers

type Parser = Parsec Void String

-- definition of a type for parser errors

type ParserError = ParseErrorBundle String Void

-- Espaços horizontais.
-- Usado dentro de uma linha.
-- Importante: não consome quebra de linha.
slexer :: Parser ()
slexer =
    L.space
        hspace1
        (L.skipLineComment "#")
        empty

-- Espaços com quebra de linha.
-- Usado para controlar indentação.
slexerNewline :: Parser ()
slexerNewline =
    L.space
        space1
        (L.skipLineComment "#")
        empty

-- parsing a symbol

symbol :: String -> Parser String
symbol = L.symbol slexer

-- dealing with parenthesis

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- producing a lexeme

lexeme :: Parser a -> Parser a
lexeme = L.lexeme slexer

-- parsing a number

integer :: Parser Word32
integer = lexeme L.decimal

-- parsing keywords

pKeyword :: String -> Parser String
pKeyword keyword =
    lexeme (string keyword <* notFollowedBy (alphaNumChar <|> char '_'))

-- reserved words

reservedWords :: [String]
reservedWords =
    [ "if"
    , "else"
    , "input"
    , "print"
    , "and"
    , "or"
    , "not"
    ]

{- | Parse an identifier.
Variáveis seguem estilo Python: x, y, soma, valor1, etc.
-}
pName :: Parser Name
pName = lexeme $ do
    name <-
        (:)
            <$> (letterChar <|> char '_')
            <*> many (alphaNumChar <|> char '_')

    if name `elem` reservedWords
        then empty
        else return name

-- expressions

pExp :: Parser PyExp
pExp = makeExprParser pFactor optable

pFactor :: Parser PyExp
pFactor =
    PyVal <$> integer
        <|> PyVar <$> pName
        <|> parens pExp
        <?> "expression"

binary :: String -> (PyExp -> PyExp -> PyExp) -> Operator Parser PyExp
binary name f = InfixL (f <$ symbol name)

binaryNonAssoc :: String -> (PyExp -> PyExp -> PyExp) -> Operator Parser PyExp
binaryNonAssoc name f = InfixN (f <$ symbol name)

prefix :: String -> (PyExp -> PyExp) -> Operator Parser PyExp
prefix name f = Prefix (f <$ pKeyword name)

optable :: [[Operator Parser PyExp]]
optable =
    [ [prefix "not" PyNot]
    , [binary "+" PyAdd]
    ,
        [ binaryNonAssoc "==" PyEq
        , binaryNonAssoc "<" PyLt
        , binaryNonAssoc ">" PyGt
        ]
    , [binary "and" PyAnd]
    , [binary "or" PyOr]
    ]

-- statements

pStatement :: Pos -> Parser PyStmt
pStatement indent =
    try (pIf indent)
        <|> try pInput
        <|> try pPrint
        <|> pAssign
        <?> "statement"

-- var = input()

pInput :: Parser PyStmt
pInput = do
    v <- pName
    _ <- symbol "="
    _ <- pKeyword "input"
    _ <- symbol "("
    _ <- symbol ")"
    return (PyInput v)

-- print(expr)

pPrint :: Parser PyStmt
pPrint = do
    _ <- pKeyword "print"
    _ <- symbol "("
    e <- pExp
    _ <- symbol ")"
    return (PyPrint e)

-- var = expr

pAssign :: Parser PyStmt
pAssign = do
    v <- pName
    _ <- symbol "="
    e <- pExp
    return (PyAssign v e)

-- if expr:
--     statements
-- else:
--     statements

pIf :: Pos -> Parser PyStmt
pIf currentIndent = do
    _ <- pKeyword "if"
    cond <- pExp
    _ <- symbol ":"

    thenBlock <- pIndentedBlock currentIndent

    elseBlock <- optional $ try $ do
        _ <- L.indentGuard slexerNewline EQ currentIndent
        _ <- pKeyword "else"
        _ <- symbol ":"
        pIndentedBlock currentIndent

    return (PyIf cond thenBlock elseBlock)

-- Parseia um bloco indentado depois de ':'

pIndentedBlock :: Pos -> Parser [PyStmt]
pIndentedBlock parentIndent = do
    void eol

    blockIndent <- L.indentGuard slexerNewline GT parentIndent

    firstStmt <- pStatement blockIndent

    restStmts <- many $ try $ do
        _ <- L.indentGuard slexerNewline EQ blockIndent
        pStatement blockIndent

    return (firstStmt : restStmts)

-- Parseia um statement exatamente em uma coluna esperada

pStatementAt :: Pos -> Parser PyStmt
pStatementAt indent = do
    _ <- L.indentGuard slexerNewline EQ indent
    pStatement indent

-- Top-level parser.
-- Todo comando global deve começar na coluna 1.

pPyProgram :: Parser PyProgram
pPyProgram = do
    slexerNewline
    ss <- many (try (pStatementAt pos1))
    slexerNewline
    return (PyProgram ss)

-- Run parser

pyParser :: String -> Either String PyProgram
pyParser input =
    case runParser (pPyProgram <* eof) "<input>" input of
        Left err -> Left (errorBundlePretty err)
        Right ast -> Right ast
