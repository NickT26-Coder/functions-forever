# Grammar

### Metasyntax:
- `nonterm -> result`: `nonterm` can be substituted for `result`
- `{ nonterm }`: zero or more of `nonterm`
- `[ nonterm ]`: zero or one of `nonterm`
- `( nonterm )`: basic grouping, just `nonterm`
- `nonterm1 | nonterm2`: one of either `nonterm1` or `nonterm2`
- `"abc"`: exactly the characters `abc`

```
program -> { statement | declaration }
declaration -> variableDeclaration | functionDeclaration
variableDeclaration -> “let” identifier “=” expression “;”
functionDeclaration -> “def” identifier “(“ [ parameterList ] “)” block
parameterList -> identifier { “,” identifier }
statement -> ifStatement | whileStatement | returnStatement | block | expressionStatement;
ifStatement -> “if” “(“ expression “)” statement [ “else” statement ]
whileStatement -> “while” “(“ expression “)” statement
returnStatement -> “return” expression “;"
block -> “{“ { statement | declaration } “}”
expressionStatement -> expression “;”

expression -> assignment
assignment -> identifier = assignment | comparison
comparison -> comparison comparisonOperator sum | sum
comparisonOperator -> “>” | “<” | “>=” | “<=” | “==” | “!=”
sum -> sum (“+” | “-”) prod
prod -> prod ( “*” | “/”) | unary
unary -> ["-" | "!" ] unary | call
call -> call “(“ [ argumentList ] “)” | primary
argumentList -> expression { “,”  expression }
primary -> integer | string | boolean | identifier | “(“ expression “)”
integer -> digit { digit }
string -> “\"” { stringCharacter } “\"”
stringCharacter  -> any character except a double quote
boolean -> “true” | “false”
identifier -> letter { letter | digit }
digit -> “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9”
letter -> "_" | “A” | “B” | “C” | “D” | “E” | “F” | “G” | “H” | “I” | “J” | “K” | “L” | “M” | “N” | “O” | “P” | “Q” | “R” | “S” | “T” | “U” | “V” | “W” | “X” | “Y” | “Z” | “a” | “b” | “c” | “d” | “e” | “f” | “g” | “h” | “i” | “j” | “k” | “l” | “m” | “n” | “o” | “p” | “q” | “r” | “s” | “t” | “u” | “v” | “w” | “x” | “y” | “z”
```
