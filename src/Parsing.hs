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
parseVariableDeclaration (name@(Token _ (TIdentifier _)) : (Token _ TSingleEqual) : after) = do
  (value, afterValue) <- parseExpression after
  (_, afterStatement) <- expect TSemicolon afterValue
  Right (SVariableDeclaration name value, afterStatement)
parseVariableDeclaration (Token n _ : _) = Left $ Error n "Invalid variable declaration."

parseFunctionDeclaration :: ParseFn Statement
parseFunctionDeclaration(name@(Token _ (TIdentifier _)) : afterName) = do
  (_, afterOpenParen) <- expect TOpenParen afterName 
  (params, afterParams) <- parseParameters afterOpenParen
  (_, afterCloseParen) <- expect TCloseParen afterParams
  (body, afterBody) <- parseBlock afterCloseParen 
  Right (SFunctionDeclaration name params (sBodyFromBlock body), afterBody)
  where
    sBodyFromBlock (SBlockStatement _ statements) = statements
    sBodyFromBlock _ = []
parseFunctionDeclaration (Token n _ : _ ) = 
  Left $ Error n "Expects function name"

parseParameters :: ParseFn [Token]
parseParameters tokens@(Token _ TCloseParen : _ ) = Right ([], tokens)
parseParameters (name@(Token _ (TIdentifier _)) : afterName) = 
  case afterName of 
    Token _ TComma : afterComma -> do 
      (params, remaining) <- parseParameters afterComma
      Right (name : params, remaining)
    _->Right ([name], afterName)
parseParameters (Token n _ : _ ) =
  Left $ Error n "Expects parameter name"

parseStatement :: ParseFn Statement
parseStatement all@(Token _ TIf : after) = parseIfStatement all
parseStatement all@(Token _ TReturn : _) = parseReturnStatement all
parseStatement all@(Token _ TWhile : _) = parseWhileStatement all
parseStatement all@(Token _ TOpenBrace : _) = parseBlock all
-- TODO: Other cases go here
parseStatement ts = parseExpressionStatement ts

parseIfStatement :: ParseFn Statement
parseIfStatement (keyword@(Token _ TIf) : afterIf) = do
  (_, afterOpenParen) <- expect TOpenParen afterIf
  (condition, afterCondition) <- parseExpression afterOpenParen
  (_, afterCloseParen) <- expect TCloseParen afterCondition
  (trueBranch, afterTrueBranch) <- parseBlock afterCloseParen
  case afterTrueBranch of 
    Token _ TElse : afterElse -> do
      (falseBranch, afterFalseBranch) <- parseBlock afterElse
      Right (SIfStatement keyword condition trueBranch (Just falseBranch), afterFalseBranch)
    _ -> Right (SIfStatement keyword condition trueBranch Nothing, afterTrueBranch)

parseWhileStatement :: ParseFn Statement
parseWhileStatement(keyword@(Token _ TWhile) : afterWhile) = do
  (_, afterOpenParen) <- expect TOpenParen afterWhile
  (condition, afterCondition) <- parseExpression afterOpenParen
  (_, afterCloseParen) <- expect TCloseParen afterCondition
  (body, afterBody) <- parseBlock afterCloseParen
  Right (SWhileStatement keyword condition body, afterBody)

parseReturnStatement :: ParseFn Statement
parseReturnStatement(keyword@(Token _ TReturn) : afterReturn) = do
  (value, afterValue) <- parseExpression afterReturn
  (_, afterStatement) <- expect TSemicolon afterValue 
  Right (SReturnStatement keyword value, afterStatement)

parseBlock :: ParseFn Statement
parseBlock(openBrace@(Token _ TOpenBrace): afterOpenBrace) = do
  (body, afterBody) <- loop afterOpenBrace 
  (_, afterCloseBrace) <- expect TCloseBrace afterBody
  Right (SBlockStatement openBrace body, afterCloseBrace)
  where
    loop :: ParseFn [Statement]
    loop tokens@(Token _ TCloseBrace : _ ) = Right ([], tokens)
    loop [Token n TEOF] = Left $ Error n "Expects close bracket"
    loop tokens = do
      (statement,afterStatement) <- parseDeclarationOrStatement tokens
      (statementsAfter, remaining) <- loop afterStatement
      Right (statement : statementsAfter, remaining)

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
parseLogical = parseLeftAssociativeBinaryExpression
  parseComparison
  [TAnd, TOr]
  ELogical

parseComparison :: ParseFn Expression
parseComparison =
  parseLeftAssociativeBinaryExpression
    parseSum
    [TGreater, TLess, TGreaterEqual, TLessEqual, TDoubleEqual, TNotEqual]
    EComparison

parseSum :: ParseFn Expression
parseSum = 
  parseLeftAssociativeBinaryExpression
    parseProd 
    [TPlus, TMinus]
    ESum

parseProd :: ParseFn Expression
parseProd = 
  parseLeftAssociativeBinaryExpression
    parseUnary
    [TTimes, TDivide]
    EProduct

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
parseUnary(operator@(Token _ operatorValue) : afterOperator) | operatorValue == TMinus || operatorValue == TNot = do
  (object, afterObject) <- parseUnary afterOperator
  Right (EUnary operator object, afterObject)
parseUnary ts = parseCall ts

parseCall :: ParseFn Expression
parseCall ts = do
  (expr, afterExpr) <- parsePrimary ts
  loop expr afterExpr 
  where 
    loop :: Expression -> ParseFn Expression 
    loop expr (openParen@(Token _ TOpenParen) : afterOpenParen) = do
      (args, afterArgs) <- parseArguments afterOpenParen 
      (_, afterCloseParen) <- expect TCloseParen afterArgs
      loop (ECall expr openParen args) afterCloseParen
    loop expr remaining = Right (expr, remaining)

parseArguments :: ParseFn [Expression]
parseArguments tokens@(Token _ TCloseParen : _) = Right ([], tokens)
parseArguments ts = do 
  (arg, afterArg) <- parseExpression ts 
  case afterArg of 
    Token _ TComma : afterComma -> do 
      (args, afterArgs) <- parseArguments afterComma 
      Right (arg : args, afterArgs)
    _ -> Right ([arg], afterArg)

parsePrimary :: ParseFn Expression
-- TODO: Other primary-level expressions go here
parsePrimary (token@(Token _ (TNumber _ )) : after) =
  Right (ENumber token, after)
parsePrimary (token@(Token _ (TString _ )) : after) =
  Right (EString token, after)
parsePrimary (token@(Token _ TTrue) : after) =
  Right (EBoolean token, after)
parsePrimary (token@(Token _ TFalse) : after) =
  Right (EBoolean token, after)
parsePrimary (token@(Token _ (TIdentifier _ )) : after) =
    Right (EVariable token, after)
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
