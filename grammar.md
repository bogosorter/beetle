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
                 | 'return' expression

assignment = symbol function
           | symbol '=' expression

expression = arithmetic (('==' | '<' | '>' | '<=' | '>=') arithmetic)?
arithmetic = atom (('+' | '-') atom)*
atom = symbol expressionCall
     | '(' expression ')' expressionCall
     | lambda
     | integer
     | boolean
expressionCall = '(' expression (',' expression)* ')' expressionCall
               | ϵ
     
function = '(' symbol ':' type (',' symbol ':' type)* ')' ':' type '=' returnExpression
lambda = '(' symbol ':' type (',' symbol ':' type)* ')' ':' type '=' expression

type = 'integer' | 'boolean' | '(' type ')' | type ('->' type)*;
```
