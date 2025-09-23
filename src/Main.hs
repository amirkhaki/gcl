module Main (main) where
import System.Environment (getArgs)
import Text.Megaparsec (
  Parsec
  , choice
  , between
  , takeWhileP
  , try
  , parseTest
  , sepBy
  , sepBy1
  , eof
  )
import Data.Void (Void)
import Text.Megaparsec.Char (space1, letterChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Char (isAlphaNum)



data BoolExpr
  = BTrue
  | BFalse
  | Not BoolExpr
  | BoolExpr :&&: BoolExpr
  | BoolExpr :||: BoolExpr
  | Expr :=: Expr
  | Expr :>: Expr
  deriving (Eq, Ord, Show)

data Expr = Var String
  | Number Integer
  | Negation Expr
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  deriving (Eq, Ord, Show)

data Statement = Alternative GCS | Repetitive GCS
  | String ::=: Expr
  deriving (Eq, Ord, Show)
type SL = [Statement]
data GuardedCommand = GuardedCommand BoolExpr SL
  deriving (Eq, Ord, Show)
type GCS = [GuardedCommand]

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
  <*> (symbol "->" *> pStatementList)

pGCS :: Parser GCS
pGCS = sepBy pGuardedCommand (symbol "[]")

pStatement :: Parser Statement
pStatement = choice [pAlternative, pRepetitive, pAssign]
  where pAssign = (::=:) <$> lexeme ((:) <$> letterChar <*> takeWhileP Nothing isAlphaNum) <*> (symbol ":=" *> pExpr)
        pRepetitive = Repetitive <$> between (symbol "do") (symbol "od") pGCS
        pAlternative = Alternative <$> between (symbol "if") (symbol "fi") pGCS

pStatementList :: Parser [Statement]
pStatementList = sepBy pStatement (symbol ";")

pProgram :: Parser [Statement]
pProgram = sepBy1 pStatement (symbol ";") <* eof

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "❌ Please provide a file path as an argument."

    (filePath : _) -> do
      input <- readFile filePath
      parseTest pProgram input
