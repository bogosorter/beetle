<div align="center">
    <img src="./images/beetle-small.png">
</div>

# beetle

The programming language that embraces bugs.

## About

*beetle* is a simple functional programming language with TypeScript-inspired syntax. I make no pretense of being a knowledgeable language designer (linguist?), but I hope that tinkering around will teach me a little about compilers. *beetle*'s compiler is written in Haskell and outputs code in the LLVM Intermediate Representation.

For now, *beetle* programs consist of a number of assignments followed by an expression (the program's output). As an example, consider this naive implementation of the *Red, Green, and Blue Tiles* problem from [Project Euler](https://projecteuler.net/problem=117):

```
tiles(n: integer): integer =
    if n < 0: return 0;
    if n == 0: return 1;
    return tiles(n - 1) + tiles(n - 2) + tiles(n - 3) + tiles(n - 4);

return tiles(5);
```

Other examples can be found under the `tests` directory (especially inside `tests/functions`).

## Building & Running

For Linux users, a `build.sh` file is provided. Please note that it will add an executable file named `beetle` to the `~/.local/bin/` directory. Usage example:

```
$ ./build.sh
$ beetle fibonacci.btl
$ ./fibonacci
13
```

The `-o` flag can be used to specify an output file, and the flags `-ast`, `-ir` and `-ll` can be used to generate files with the AST, Intermediate Representation and LLVM codes.

## Details

The only supported primitive types are integers and booleans. The language's grammar is displayed below. A curious particularity, which I have not yet seen in any other language, is that there are two different types of expressions, `returnExpressions` and plain `expressions`. Since this is a functional language, pretty much everything is an expression, but `returnExpressions` also allow the definitions of variables, `if` usage and must terminate with a `return`. This allows a clean syntax (similar to Python's, without cluttering from curly braces and `let` statements) while avoiding whitespace sensitivity. I must admit I'm quite proud of it.

Single-line comments start with `--`.

> The factored, non-left-recursive grammar can be found under `grammar.md`

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

assignment = symbol '=' expression
           | symbol function
           | symbol (',' symbol)+ '=' expression -- tuple assignment

expression = logical
           | logical (',' logical) -- tuple creation
logical = arithmetic (('==' | '<' | '>' | '<=' | '>=') arithmetic)?
arithmetic = atom (('+' | '-') atom)*
atom = symbol
     | atom '(' logical (',' logical)* ')' -- call
     | '(' expression ')'
     | - atom
     | lambda
     | integer
     | boolean
     
function = '(' symbol ':' type (',' symbol ':' type)* ')' ':' type '=' returnExpression
lambda = '(' symbol ':' type (',' symbol ':' type)* ')' ':' type '=' expression

type = 'integer' | 'boolean' | '(' type ')' | type ('->' type)*;
```
