```
program = (assignment)* output

assignment = symbol '(' symbol ')' = expression ';'
           | symbol '=' expression ';'
output = '>' expression ';'

expression = 'if' expression 'then' expression 'else' expression
           | equality
equality = additive ('==' additive)?
additive = atom (('+' | '-') atom)*
atom = symbol '(' expression ')'
     | symbol
     | integer
     | boolean
     | '(' expression ')'
```

```
fib(n) = if (n == 0) then 1
         else if (n == 1) then 1
         else (fib(n - 1) + fib(n - 2));
> fib(5);
```
