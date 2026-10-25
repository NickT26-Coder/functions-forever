module Runtime (
  
) where

import Data.Map
import Syntax

data Environment = Environment [Map String Value]

data Value = VNumber Integer | VString [Char] | VBoolean Bool | VFunction [Statement] | VNothing

interpret :: [Char] -> Value
interpret = undefined

type RuntimeFn a b = Environment -> a -> Either Error (Environment, b)

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
evaluateNumber env value = undefined

evaluateString :: RuntimeFn Token Value
evaluateString env value = undefined

evaluateBoolean :: RuntimeFn Token Value
evaluateBoolean env value = undefined

evaluateVariable :: RuntimeFn Token Value
evaluateVariable env value = undefined
