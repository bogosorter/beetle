```
-- The grammar distinguishes between expressions and return expressions. The
-- former can only represent simple arithmetic expressions, while the later can
-- also contain assignments and if expressions, and produced expression (which
-- comes with last) must start with the keyword 'return' to disambiguate. In the
-- AST, though, there is no difference between the two.

program = returnExpression ';'

returnExpression = assignment ';' returnExpression
                 -- The if statement has an implicit else
                 | 'if' expression ':' returnExpression ';' returnExpression
                 | 'case' expression 'of' typeSymbol symbol '->' returnExpression (';' typeSymbol symbol '->' returnExpression)*
                 | 'return' expression

assignment = typeAssignment
           | symbol '=' expression
           | symbol function
           | symbol (',' symbol)+ '=' expression -- tuple assignment
typeAssignment = typeSymbol '=' type
               | typeSymbol '=' typeSymbol type ('|' typeSymbol type)+

expression = logical
           | logical (',' logical) -- tuple creation
logical = logical' ('and' logical')?
logical' = logical'' ('or' logical'')?
logical'' = arithmetic (('==' | '<' | '>' | '<=' | '>=') arithmetic)?
arithmetic = factor (('+' | '-') factor)*
factor = atom (('*' | '/' | 'mod' | 'rem') atom)*
atom = symbol expressionCall
     | symbol structAccess
     | '-' atom -- unary minus
     | 'not' atom
     | typeIdentifier atom -- constructor
     | '(' expression ')' expressionCall
     | '{' symbol ':' logical (',' symbol ':' logical)* ','? '}' -- struct constructor
     | lambda
     | integer
     | boolean
expressionCall = '(' expression (',' expression)* ')' expressionCall
               | ϵ
structAccess = '.' symbol structAccess
               | ϵ
     
function = '(' symbol ':' type (',' symbol ':' type)* ')' ':' type '=' returnExpression
lambda = '(' symbol ':' type (',' symbol ':' type)* ')' ':' type '=' expression

type = 'integer' | 'boolean' | typeSymbol | '(' type (',' type)+ ')' | '{' symbol ':' type (',' symbol ':' type)* ','? '}' | type ('->' type)*;

symbol = ['a'-'z']['a'-'z''A'-'Z']*;
typeSymbol = ['A'-'Z']['a'-'z''A'-'Z']*;
```
