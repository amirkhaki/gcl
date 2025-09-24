module AST where

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
  | String ::=: Expr | Statement :/\: Statement
  deriving (Eq, Ord, Show)
data GuardedCommand = GuardedCommand BoolExpr Statement
  deriving (Eq, Ord, Show)
type GCS = [GuardedCommand]
