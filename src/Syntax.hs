module Syntax (
  Error (..),
  Token (..),
  TokenValue (..),
  Expression (..),
  Statement (..),
  displayError,
  replaceLineNumber,
  exprLineNumber,
  stmtLineNumber,
) where

data Error = Error Int String
  deriving (Eq, Show)

displayError (Error lineNumber msg) = "Error on line " ++ show lineNumber ++ ": " ++ msg

replaceLineNumber lineNumber (Error _ msg) = Error lineNumber msg

-- All tokens must have line numbers for error locating
data Token = Token {tLine :: Int, tType :: TokenValue}
  deriving (Show)

-- Line numbers not considered in token equality
instance Eq Token where
  (Token _ tType1) == (Token _ tType2) = (tType1 == tType2)

data TokenValue
  = TNumber Integer
  | TString String -- should not include quotes
  | TIdentifier String
  | {- keywords -}
    TLet
  | TDef
  | TIf
  | TElse
  | TWhile
  | TReturn
  | TTrue
  | TFalse
  | {- Operators -}
    TSingleEqual
  | TAnd
  | TOr
  | TGreater
  | TLess
  | TGreaterEqual
  | TLessEqual
  | TDoubleEqual
  | TNotEqual
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TNot
  | {- Other -}
    TComma
  | TSemicolon
  | TOpenParen
  | TCloseParen
  | TOpenBrace
  | TCloseBrace
  | TEOF
  deriving (Eq, Show)

-- Each expression that can fail must have a token for error locating
-- This is usually the operator
data Expression
  = EAssignment {eTarget :: Token, eValue :: Expression}
  | EComparison {eLeft :: Expression, eOperator :: Token, eRight :: Expression}
  | ESum {eLeft :: Expression, eOperator :: Token, eRight :: Expression}
  | EProduct {eLeft :: Expression, eOperator :: Token, eRight :: Expression}
  | EUnary {eOperator :: Token, eObject :: Expression}
  | ECall {eObject :: Expression, eOpenParen :: Token, eArgs :: [Expression]}
  | ENumber Token
  | EString Token
  | EBoolean Token
  | EVariable Token
  deriving (Eq, Show)

-- All statements that can err must have a token, see above
-- Note whether each statement 'needs' the signifying token
data Statement
  = SVariableDeclaration {sNewName :: Token, sValue :: Expression}
  | SFunctionDeclaration {sNewVariable :: Token, sParams :: [Token], sBody :: [Statement]}
  | SIfStatement {sKeyword :: Token, sCondition :: Expression, sTrueBranch :: Statement, sFalseBranch :: Maybe Statement}
  | SWhileStatement {sKeyword :: Token, sCondition :: Expression, sLoopBody :: Statement}
  | SReturnStatement {sKeyword :: Token, sValue :: Expression}
  | SBlockStatement {sOpenBrace :: Token, sBody :: [Statement]}
  | SExpressionStatement Expression
  deriving (Eq, Show)

exprLineNumber :: Expression -> Int
exprLineNumber (EAssignment t _) = tLine t
exprLineNumber (EComparison _ t _) = tLine t
exprLineNumber (ESum _ t _) = tLine t
exprLineNumber (EProduct _ t _) = tLine t
exprLineNumber (EUnary t _) = tLine t
exprLineNumber (ECall _ t _) = tLine t
exprLineNumber (ENumber t) = tLine t
exprLineNumber (EString t) = tLine t
exprLineNumber (EBoolean t) = tLine t
exprLineNumber (EVariable t) = tLine t

stmtLineNumber :: Statement -> Int
stmtLineNumber (SVariableDeclaration t _) = tLine t
stmtLineNumber (SFunctionDeclaration t _ _) = tLine t
stmtLineNumber (SIfStatement t _ _ _) = tLine t
stmtLineNumber (SWhileStatement t _ _) = tLine t
stmtLineNumber (SReturnStatement t _) = tLine t
stmtLineNumber (SBlockStatement t _) = tLine t
stmtLineNumber (SExpressionStatement e) = exprLineNumber e
