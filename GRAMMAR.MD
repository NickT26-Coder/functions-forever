# Grammar

```
program = { statement } ;
statement = variableDeclaration | ifStatement | whileStatement | functionDeclaration | returnStatement | expressionStatement; 
variableDeclaration = “let”, identifier, “=”, expression, “;” ;
returnStatement = “return”, expression, “;” ;
block = “{“, { statement} “;” “}” ;
functionDeclaration = “def”, identifier, “(“, [ parameterList ], “)”, block ;
parameterList = identifier, { “,”, identifier } ;
argumentList = expression “;” ;
ifStatement = “if”, “(“, expression, “)”, block, { “else”, “if”, “(“, expression, “)”, block }, [ “else”, block ] ;
whileStatement = “while”, “(“, expression, “)”, block ;
expression = assignment;
assignment = identifier = comparison
comparison = sumExpression, [ comparisonOperator, sumExpression ] ;
comparisonOperator = “>” | “<” | “>=” | “<=” | “==” | “!=” ;
sumExpression = term, { (“+” | “-”), term } ;
term = unary, { ( “*” | “/”), unary} ;
unary = primary | -unary | !unary | unary “(“, [ argumentList ], “)” ;
argumentList = expression, { “,” , expression } ;
primitive = integer | string | boolean | identifier | “(“, expression, “)” ;
integer = digit;
string = “”, { stringCharacter }, “” ;
stringCharacter  = any character except a double quote or new line ;
boolean = “true” | “false” ;
identifier = letter, { letter | digit | “_” } ;
digit = “0” | “1” | “2” | “3” | “4” | “5” | “6” | “7” | “8” | “9” ;
letter = “A” | “B” | “C” | “D” | “E” | “F” | “G” | “H” | “I” | “J” | “K” | “L” | “M” | “N” | “O” | “P” | “Q” | “R” | “S” | “T” | “U” | “V” | “W” | “X” | “Y” | “Z” | “a” | “b” | “c” | “d” | “e” | “f” | “g” | “h” | “i” | “j” | “k” | “l” | “m” | “n” | “o” | “p” | “q” | “r” | “s” | “t” | “u” | “v” | “w” | “x” | “y” | “z” ;
```