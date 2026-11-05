module Runtime (
  Environment (..),
  Value (..),
  newEnvironment,
  interpret,
  displayValue,
) where

import Control.Applicative (asum)
import qualified Data.Map as Map
import Parsing (parse)
import Syntax
import Tokens (tokenize)

data Environment = Environment
  { globals :: Map.Map String Value,
    scopes :: [Map.Map String Value],
    totalPrinted :: [String]
  }

environmentGet :: Environment -> String -> Maybe Value
environmentGet (Environment globals scopes _) name = asum $ map (Map.lookup name) (scopes ++ [globals])

environmentPut :: Environment -> String -> Value -> Environment
environmentPut (Environment globals [] printed) name value =
  Environment (Map.insert name value globals) [] printed
environmentPut (Environment globals (scope : outerScopes) printed) name value =
  Environment globals ((Map.insert name value scope) : outerScopes) printed

type RuntimeFn a b = Environment -> a -> Either Error (Environment, b)

data Value
  = VNumber Integer
  | VString String
  | VBoolean Bool
  | VFunction [String] [Statement]
  | VBuiltinFunction (RuntimeFn (Int, [Value]) Value)
  | VNothing

instance Eq Value where
  VNumber a == VNumber b = (a == b)
  VString a == VString b = (a == b)
  VBoolean a == VBoolean b = (a == b)
  VFunction a1 a2 == VFunction b1 b2 = (a1 == b1 && a2 == b2)
  VBuiltinFunction _ == VBuiltinFunction _ = False
  VNothing == VNothing = True

displayValue :: Value -> String
displayValue (VNumber n) = show n
displayValue (VString s) = s
displayValue (VBoolean True) = "true"
displayValue (VBoolean False) = "false"
displayValue (VFunction _ _) = "<function>"
displayValue (VBuiltinFunction _) = "<built-in function>"
displayValue (VNothing) = "<no value>"

standardLibrary :: Map.Map String Value
standardLibrary =
  Map.fromList
    [ ("print", VBuiltinFunction printImplementation)
    ]

newEnvironment :: Environment
newEnvironment = Environment standardLibrary [] []

printImplementation :: RuntimeFn (Int, [Value]) Value
-- just concatenate it to the list of outputs
printImplementation (Environment g s outputs) (_, [arg]) = Right $ (Environment g s ((displayValue arg ++ "\n") : outputs), VNothing)
printImplementation env (n, args) = Left $ Error n ("Expected " ++ show n ++ " argument(s), got " ++ show (length args) ++ ".")

interpret :: Environment -> [Char] -> IO Environment
interpret env str =
  case programResult of
    (Right (envAfter@(Environment _ _ outputs), result)) -> do
      mapM_ putStr outputs
      putStrLn $ displayValue result
      return envAfter
    (Left (err)) -> do
      putStrLn $ displayError err
      return env
 where
  programResult :: Either Error (Environment, Value)
  programResult = do
    tokens <- tokenize str
    stmts <- parse tokens
    runProgram env stmts

runProgram :: RuntimeFn [Statement] Value
-- no last expression, print nothing
runProgram env [] = Right (env, VString "")
-- if the last line is an expression, we return that value
runProgram env [SExpressionStatement expr] = evaluate env expr
runProgram env (stmt : after) = do
  (envAfter, flow) <- execute env stmt
  case flow of
    Returned value -> Left $ Error (stmtLineNumber stmt) "Cannot return from outside a function."
    Continue -> runFunction envAfter after

data ExecutionFlow = Continue | Returned Value

execute :: RuntimeFn Statement ExecutionFlow
execute env (SVariableDeclaration newName value) = executeVariableDeclaration env (newName, value)
execute env (SFunctionDeclaration newVariable params body) = executeFunctionDeclaration env (newVariable, params, body)
execute env (SIfStatement keyword condition trueBranch falseBranch) = executeIfStatement env (keyword, condition, trueBranch, falseBranch)
execute env (SWhileStatement keyword condition loopBody) = executeWhileStatement env (keyword, condition, loopBody)
execute env (SReturnStatement keyword value) = executeReturnStatement env (keyword, value)
execute env (SBlockStatement openBrace body) = executeBlockStatement env (openBrace, body)
execute env (SExpressionStatement expression) = executeExpressionStatement env expression

executeVariableDeclaration :: RuntimeFn (Token, Expression) ExecutionFlow
executeVariableDeclaration env (Token _ (TIdentifier name), value) = do
  (envAfterValue, evaluated) <- evaluate env value
  Right (environmentPut envAfterValue name evaluated, Continue)

executeFunctionDeclaration :: RuntimeFn (Token, [Token], [Statement]) ExecutionFlow
executeFunctionDeclaration env ((Token _ (TIdentifier name), paramTokens, body)) = do
  paramStrings <- mapM getStringOfIdent paramTokens
  Right (environmentPut env name (VFunction paramStrings body), Continue)
 where
  getStringOfIdent :: Token -> Either Error String
  getStringOfIdent (Token _ (TIdentifier str)) = Right str
  getStringOfIdent (Token n _) = Left $ Error n "Expected identifier token"

executeIfStatement :: RuntimeFn (Token, Expression, Statement, Maybe Statement) ExecutionFlow
executeIfStatement env ((Token n _), condition, trueBranch, falseBranch) = do
  (envAfterCondition, result) <- evaluate env condition
  case (result, falseBranch) of
    (VBoolean True, _) -> execute envAfterCondition trueBranch
    (VBoolean False, Nothing) -> Right (envAfterCondition, Continue)
    (VBoolean False, Just stmt) -> execute envAfterCondition stmt
    _ -> Left $ Error n ("Expected condition to be a boolean value, instead got value of " ++ displayValue result)

executeWhileStatement :: RuntimeFn (Token, Expression, Statement) ExecutionFlow
executeWhileStatement env (keyword, condition, loopBody) = undefined

executeReturnStatement :: RuntimeFn (Token, Expression) ExecutionFlow
executeReturnStatement env (keyword, value) = do
  (envAfter, evaluated) <- evaluate env value
  Right (envAfter, Returned evaluated)

executeBlockStatement :: RuntimeFn (Token, [Statement]) ExecutionFlow
executeBlockStatement (Environment globals scopes printed) (openBrace, body) = do
  -- create a new scope for the block
  let newEnv = Environment globals (Map.empty : scopes) printed
  (Environment globalsAfter scopesAfter printedAfter, result) <- executeBlockBody newEnv body
  -- exclude the innermost scope just created
  Right $ (Environment globalsAfter (drop 1 scopesAfter) printedAfter, result)

executeBlockBody :: RuntimeFn ([Statement]) ExecutionFlow
executeBlockBody env [] = Right (env, Continue)
executeBlockBody env (stmt : after) = do
  (envAfter, flowAfter) <- execute env stmt
  case flowAfter of
    Continue -> executeBlockBody envAfter after
    flow@(Returned _) -> Right (envAfter, flow)

executeExpressionStatement :: RuntimeFn Expression ExecutionFlow
executeExpressionStatement env expression = undefined

evaluate :: RuntimeFn Expression Value
evaluate env (EAssignment target value) = evaluateAssignment env (target, value)
evaluate env (EComparison left operator right) = evaluateComparison env (left, operator, right)
evaluate env (ELogical left operator right) = evaluateLogical env (left, operator, right)
evaluate env (ESum left operator right) = evaluateSum env (left, operator, right)
evaluate env (EProduct left operator right) = evaluateProduct env (left, operator, right)
evaluate env (EUnary operator object) = evaluateUnary env (operator, object)
evaluate env (ECall object openParen args) = evaluateCall env (object, openParen, args)
evaluate env (ENumber value) = evaluateNumber env value
evaluate env (EString value) = evaluateString env value
evaluate env (EBoolean value) = evaluateBoolean env value
evaluate env (EVariable value) = evaluateVariable env value

evaluateAssignment :: RuntimeFn (Token, Expression) Value
evaluateAssignment env (Token n (TIdentifier target), value) = do
  _ <- case environmentGet env target of
    Nothing -> Left $ Error n ("Undefined variable '" ++ target ++ "'")
  (envAfterValue, evaluated) <- evaluate env value
  Right (environmentPut envAfterValue target evaluated, evaluated)

evaluateLogical :: RuntimeFn (Expression, Token, Expression) Value
evaluateLogical env (left, operator, right) = undefined

evaluateComparison :: RuntimeFn (Expression, Token, Expression) Value
evaluateComparison env (left, operator, right) = undefined

evaluateSum :: RuntimeFn (Expression, Token, Expression) Value
evaluateSum env (left, operator, right) = undefined

evaluateProduct :: RuntimeFn (Expression, Token, Expression) Value
evaluateProduct env (left, operator, right) = undefined

evaluateUnary :: RuntimeFn (Token, Expression) Value
evaluateUnary env (Token n operator, object) = do
  (envAfterObject, objectValue) <- evaluate env object
  case (operator, objectValue) of
    (TMinus, VNumber num) -> Right (envAfterObject, VNumber $ -num)
    (TNot, VBoolean bool) -> Right (envAfterObject, VBoolean $ not bool)
    _ -> Left $ Error n ("Operation '" ++ getTokenName operator ++ "' not supported on value of " ++ displayValue objectValue)

evaluateCall :: RuntimeFn (Expression, Token, [Expression]) Value
evaluateCall env (object, (Token n _), args) = do
  (envAfterObject, objectValue) <- evaluate env object
  (Environment globals scopes printed, argValues) <- evaluateArguments env args
  case objectValue of
    (VFunction params body) -> do
      _ <-
        if length params /= length argValues then
          Left $ Error n "Incorrect number of arguments."
        else
          Right ()
      let topScope = Map.fromList $ zip params argValues
      let innerEnv = Environment globals [topScope] printed
      (Environment globalsAfter _ printedAfter, returnValue) <- runFunction innerEnv body
      let envAfterCall = Environment globalsAfter scopes printedAfter
      Right (envAfterCall, returnValue)
    (VBuiltinFunction (fn)) -> do
      -- builtin functions might change globals idk
      (Environment globalsAfter _ printedAfter, returnValue) <- fn env (n, argValues)
      let envAfterCall = Environment globalsAfter scopes printedAfter
      Right (envAfterCall, returnValue)
    _ -> Left $ Error n ("Cannot call a value of " ++ displayValue objectValue)

evaluateArguments :: RuntimeFn [Expression] [Value]
evaluateArguments env [] = Right (env, [])
evaluateArguments env (expr : after) = do
  (envAfterHead, headValue) <- evaluate env expr
  (envAfterTail, tailValues) <- evaluateArguments envAfterHead after
  Right (envAfterTail, headValue : tailValues)

runFunction :: RuntimeFn [Statement] Value
runFunction env [] = Right (env, VNothing)
runFunction env (stmt : after) = do
  (envAfter, flow) <- execute env stmt
  case flow of
    Returned value -> Right (envAfter, value)
    Continue -> runFunction envAfter after

evaluateNumber :: RuntimeFn Token Value
evaluateNumber env (Token _ (TNumber n)) = Right (env, VNumber n)

evaluateString :: RuntimeFn Token Value
evaluateString env value = undefined

evaluateBoolean :: RuntimeFn Token Value
evaluateBoolean env value = undefined

evaluateVariable :: RuntimeFn Token Value
evaluateVariable env (Token n (TIdentifier variableName)) = case environmentGet env variableName of
  Just value -> Right (env, value)
  Nothing -> Left $ Error n ("Undefined variable '" ++ variableName ++ "'")
