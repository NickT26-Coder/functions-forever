module Parsing (
  parse,
) where

import Syntax

type ParseFn a = [Token] -> Either Error (a, [Token])

parse :: [Token] -> Either Error [Statement]
parse = undefined

parseDeclarationOrStatement :: ParseFn Statement
parseDeclarationOrStatement (Token n TLet : after) = parseVariableDeclaration after
parseDeclarationOrStatement ts = parseStatement ts

parseVariableDeclaration :: ParseFn Statement
parseVariableDeclaration = undefined

parseFunctionDeclaration :: ParseFn Statement
parseFunctionDeclaration = undefined

parseStatement :: ParseFn Statement
parseStatement all@(Token _ TIf : after) = parseIfStatement all
-- Other cases go here
parseStatement ts = parseExpressionStatement ts

parseIfStatement :: ParseFn Statement
parseIfStatement = undefined

parseWhileStatement :: ParseFn Statement
parseWhileStatement = undefined

parseReturnStatement :: ParseFn Statement
parseReturnStatement = undefined

parseBlock :: ParseFn Statement
parseBlock = undefined

parseExpressionStatement :: ParseFn Statement
parseExpressionStatement ts = do
  (expr, after) <- parseExpression ts
  Right (SExpressionStatement expr, after)

parseExpression :: ParseFn Expression
parseExpression [] = Left $ Error 0 "Expected an expression."
parseExpression ts = undefined

parseAssignment :: ParseFn Expression
parseAssignment (target@(Token _ (TIdentifier s)) : Token _ TSingleEqual : after) = do
  (value, after) <- parseAssignment after
  Right (EAssignment target value, after)
parseAssignment ts = parseComparison ts

parseComparison :: ParseFn Expression
parseComparison = parseLeftAssociativeBinaryExpression
  parseSum
  [TGreater, TLess, TGreaterEqual, TLessEqual, TDoubleEqual, TNotEqual]
  EComparison

parseSum :: ParseFn Expression
parseSum = undefined

parseProd :: ParseFn Expression
parseProd = undefined

parseLeftAssociativeBinaryExpression
  :: (ParseFn Expression)
  -> [TokenValue]
  -> (Expression -> Token -> Expression -> Expression)
  -> ParseFn Expression
parseLeftAssociativeBinaryExpression operandFn possibleOperators constructor ts = do
  (leftOperand, after) <- operandFn ts
  if null after then -- no more tokens
    Right (leftOperand, after)
  else
    helper leftOperand after
 where
  helper :: Expression -> ParseFn Expression
  helper = undefined

parseUnary :: ParseFn Expression
parseUnary = undefined

parseCall :: ParseFn Expression
parseCall = undefined

parseArgumentList :: ParseFn Expression
parseArgumentList = undefined

parsePrimary :: ParseFn Expression
parsePrimary = undefined
