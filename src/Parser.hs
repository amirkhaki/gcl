module Parser (pProgram) where
import AST
import Text.Megaparsec (
  Parsec
  , choice
  , between
  , takeWhileP
  , try
  , sepBy
  , sepBy1
  , eof
  )
import Data.Void (Void)
import Text.Megaparsec.Char (space1, letterChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Char (isAlphaNum)

type Parser = Parsec Void String
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

binary :: String -> (a -> a -> a) -> Operator Parser a
binary  name f = InfixL  (f <$ symbol name)

prefix :: String -> (a -> a) -> Operator Parser a
prefix  name f = Prefix  (f <$ symbol name)

pBoolExpr :: Parser BoolExpr
pBoolExpr = makeExprParser pTerm operatorTable
  where operatorTable = [ [ prefix "!" Not]
                        , [ binary "||" (:||:), binary "&&" (:&&:)]
                        ]
        pTerm = choice [pTrue, pFalse, try pRelational, try (parens pBoolExpr)]
        pTrue = BTrue <$ symbol "true"
        pFalse = BFalse <$ symbol "false"
        pRelational = do
          e1 <- pExpr
          op <- choice [ (:=:) <$ symbol "=", (:>:) <$ symbol ">" ]
          op e1 <$> pExpr

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable
  where operatorTable = [ [ prefix "-" Negation, prefix "+" id]
                        , [ binary "*" (:*:), binary "/" (:/:)]
                        , [ binary "+" (:+:), binary "-" (:-:)]
                        ]
        pTerm = choice [ pVariable, pNumber, parens pExpr]
        pVariable = Var <$>
          lexeme ((:) <$> letterChar <*> takeWhileP Nothing isAlphaNum)
        pNumber = Number <$> lexeme signedInteger

pGuardedCommand :: Parser GuardedCommand
pGuardedCommand = GuardedCommand <$>
  lexeme pBoolExpr
  <*> (symbol "->" *> pStatement)

pGCS :: Parser GCS
pGCS = sepBy pGuardedCommand (symbol "[]")

pStatement :: Parser Statement
pStatement = folder <$> sepBy f (symbol ";")
  where pAssign = (::=:) <$> lexeme ((:) <$> letterChar <*> takeWhileP Nothing isAlphaNum) <*> (symbol ":=" *> pExpr)
        pRepetitive = Repetitive <$> between (symbol "do") (symbol "od") pGCS
        pAlternative = Alternative <$> between (symbol "if") (symbol "fi") pGCS
        f = choice [pAlternative, pRepetitive, pAssign]
        folder [] = Repetitive []
        folder z@(_:_) = foldr1 (:/\:) z

pProgram :: Parser Statement
pProgram = pStatement <* eof
