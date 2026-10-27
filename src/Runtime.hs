module Runtime (
  Environment (..),
  Value (..),
  newEnvironment,
  interpret,
  displayValue,
) where

import Data.Map
import Parsing (parse)
import Syntax
import Tokens (tokenize)

data Environment = Environment {globals :: Map String Value, functionScopes :: [Map String Value]}

newEnvironment = Environment Data.Map.empty []

environmentGet :: Environment -> String -> Maybe Value
environmentGet (Environment globals []) name = Data.Map.lookup name globals
environmentGet (Environment _ (scope : _)) name = Data.Map.lookup name scope

environmentPut :: Environment -> String -> Value -> Environment
environmentPut (Environment globals []) name value = Environment (Data.Map.insert name value globals) []
environmentPut (Environment globals (scope : outerScopes)) name value = Environment globals ((Data.Map.insert name value scope) : outerScopes)

data Value = VNumber Integer | VString [Char] | VBoolean Bool | VFunction [Statement] | VNothing
  deriving (Eq)

displayValue :: Value -> String
displayValue (VNumber n) = show n
displayValue (VString s) = show s
displayValue (VBoolean b) = show b
displayValue (VFunction _) = "<function>"
displayValue (VNothing) = "<no value>"

type RuntimeFn a b = Environment -> a -> Either Error (Environment, b)

interpret :: RuntimeFn [Char] (IO ())
interpret env str = do
  tokens <- tokenize str
  stmts <- parse tokens
  undefined

execute :: RuntimeFn Statement ()
execute env (SVariableDeclaration newName value) = executeVariableDeclaration env (newName, value)
execute env (SFunctionDeclaration newVariable params body) = executeFunctionDeclaration env (newVariable, params, body)
execute env (SIfStatement keyword condition trueBranch falseBranch) = executeIfStatement env (keyword, condition, trueBranch, falseBranch)
execute env (SWhileStatement keyword condition loopBody) = executeWhileStatement env (keyword, condition, loopBody)
execute env (SReturnStatement keyword value) = executeReturnStatement env (keyword, value)
execute env (SBlockStatement openBrace body) = executeBlockStatement env (openBrace, body)
execute env (SExpressionStatement expression) = executeExpressionStatement env expression

executeVariableDeclaration :: RuntimeFn (Token, Expression) ()
executeVariableDeclaration env (newName, value) = undefined

executeFunctionDeclaration :: RuntimeFn (Token, [Token], [Statement]) ()
executeFunctionDeclaration env (newVariable, params, body) = undefined

executeIfStatement :: RuntimeFn (Token, Expression, Statement, Maybe Statement) ()
executeIfStatement env (keyword, condition, trueBranch, falseBranch) = undefined

executeWhileStatement :: RuntimeFn (Token, Expression, Statement) ()
executeWhileStatement env (keyword, condition, loopBody) = undefined

executeReturnStatement :: RuntimeFn (Token, Expression) ()
executeReturnStatement env (keyword, value) = undefined

executeBlockStatement :: RuntimeFn (Token, [Statement]) ()
executeBlockStatement env (openBrace, body) = undefined

executeExpressionStatement :: RuntimeFn Expression ()
executeExpressionStatement env expression = undefined

evaluate :: RuntimeFn Expression Value
evaluate env (EAssignment target value) = evaluateAssignment env (target, value)
evaluate env (EComparison left operator right) = evaluateComparison env (left, operator, right)
evaluate env (ESum left operator right) = evaluateSum env (left, operator, right)
evaluate env (EProduct left operator right) = evaluateProduct env (left, operator, right)
evaluate env (EUnary operator object) = evaluateUnary env (operator, object)
evaluate env (ECall object openParen args) = evaluateCall env (object, openParen, args)
evaluate env (ENumber value) = evaluateNumber env value
evaluate env (EString value) = evaluateString env value
evaluate env (EBoolean value) = evaluateBoolean env value
evaluate env (EVariable value) = evaluateVariable env value

evaluateAssignment :: RuntimeFn (Token, Expression) Value
evaluateAssignment env (target, value) = undefined

evaluateComparison :: RuntimeFn (Expression, Token, Expression) Value
evaluateComparison env (left, operator, right) = undefined

evaluateSum :: RuntimeFn (Expression, Token, Expression) Value
evaluateSum env (left, operator, right) = undefined

evaluateProduct :: RuntimeFn (Expression, Token, Expression) Value
evaluateProduct env (left, operator, right) = undefined

evaluateUnary :: RuntimeFn (Token, Expression) Value
evaluateUnary env (operator, object) = undefined

evaluateCall :: RuntimeFn (Expression, Token, [Expression]) Value
evaluateCall env (object, openParen, args) = undefined

evaluateNumber :: RuntimeFn Token Value
evaluateNumber env (Token _ (TNumber n)) = Right (env, VNumber n)

evaluateString :: RuntimeFn Token Value
evaluateString env value = undefined

evaluateBoolean :: RuntimeFn Token Value
evaluateBoolean env value = undefined

evaluateVariable :: RuntimeFn Token Value
evaluateVariable env (Token n (TIdentifier variableName)) = undefined
