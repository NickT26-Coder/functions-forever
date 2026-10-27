module Parsing (
  parse,
) where

import Syntax

parse :: [Token] -> Either Error [Statement]
parse ts = do
  (program, _) <- loop ts
  Right program
 where
  loop :: ParseFn [Statement]
  loop [Token _ TEOF] = Right ([], [])
  loop ts@(_ : _) = do
    (statement, after) <- parseDeclarationOrStatement ts
    (statementsAfter, _) <- loop after
    Right (statement : statementsAfter, [])

type ParseFn a = [Token] -> Either Error (a, [Token])

parseDeclarationOrStatement :: ParseFn Statement
-- these two don't use the `let` or `def` tokens, so we exclude them
parseDeclarationOrStatement (Token _ TLet : after) = parseVariableDeclaration after
parseDeclarationOrStatement (Token _ TDef : after) = parseFunctionDeclaration after
parseDeclarationOrStatement ts = parseStatement ts

parseVariableDeclaration :: ParseFn Statement
parseVariableDeclaration = undefined

parseFunctionDeclaration :: ParseFn Statement
parseFunctionDeclaration = undefined

parseStatement :: ParseFn Statement
parseStatement all@(Token _ TIf : after) = parseIfStatement all
-- TODO: Other cases go here
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
  (expr, afterExpr) <- parseExpression ts
  (_, afterStatement) <- expect TSemicolon afterExpr
  Right (SExpressionStatement expr, afterStatement)

parseExpression :: ParseFn Expression
parseExpression = parseAssignment -- the lowest precedence

parseAssignment :: ParseFn Expression
parseAssignment (target@(Token _ (TIdentifier _)) : Token _ TSingleEqual : after) = do
  (value, afterValue) <- parseAssignment after -- assignment is right-associative
  Right (EAssignment target value, afterValue)
parseAssignment ts = parseLogical ts

parseLogical :: ParseFn Expression
parseLogical = undefined

parseComparison :: ParseFn Expression
parseComparison =
  parseLeftAssociativeBinaryExpression
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
  loop leftOperand after
 where
  loop :: Expression -> ParseFn Expression
  loop curLeftOperand (operatorToken@(Token _ operatorTokenValue) : rightTokens)
    -- the next token is an operator we're looking for!
    | operatorTokenValue `elem` possibleOperators = do
        (rightOperand, afterRight) <- operandFn rightTokens
        loop (constructor curLeftOperand operatorToken rightOperand) afterRight -- next iteration of the loop
  loop curLeftOperand rightTokens = Right (curLeftOperand, rightTokens) -- the next token is not an operator; return the current left operand

parseUnary :: ParseFn Expression
parseUnary = undefined

parseCall :: ParseFn Expression
parseCall = undefined

parsePrimary :: ParseFn Expression
-- TODO: Other primary-level expressions go here
parsePrimary (Token _ TOpenParen : after) = do
  (grouped, afterGroup) <- parseExpression after
  (_, afterParen) <- expect TCloseParen afterGroup
  Right (grouped, afterParen)
parsePrimary (Token n _ : _) = Left $ Error n "Expected an expression."

-- WARNING: Do not use with Number _, String _, or Identifier _.
-- Those tokens should be caught with pattern matching.
expect :: TokenValue -> ParseFn Token
expect expectedValue (token@(Token n value) : after) =
  if expectedValue == value then
    Right (token, after)
  else
    -- remove the T prefix from token (drop 1)
    Left $ Error n ("Expected a " ++ (drop 1 $ show expectedValue) ++ " token.")
