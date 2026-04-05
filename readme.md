<div align="center">
    <img src="./images/beetle-small.png">
</div>

# beetle

The programming language that embraces bugs.

## About

*beetle* is a simple functional programming language with TypeScript-inspired syntax. I make no pretense of being a knowledgeable language designer (linguist?), but I hope that tinkering around will teach me a little about compilers.

*beetle*'s compiler is written in Haskell and outputs code in the LLVM Intermediate Representation. For now, every *beetle* program consists of a number of assignments followed by an expression (the program's output). As an example, consider this naive implementation of the *Red, Green, and Blue Tiles* problem from [Project Euler](https://projecteuler.net/problem=117):

```
tiles(n: integer): integer ->
    if n < 0 then 0
    else if n == 0 then 1
    else tiles(n - 1) + tiles(n - 2) + tiles(n - 3) + tiles(n - 4);

> tiles(5);
```

Other examples can be found under the `examples` directory.

## Building

The source code is under the `source` directory. For Linux users, a `build.sh` file is provided. Please note that it will add an executable file named `beetle` to the `~/.local/bin/` directory. Usage example:

```
$ ./build.sh
$ beetle fibonacci.btl
$ ./fibonacci
13
```

## Details

Only integers and boolean values are supported. The language's grammar is displayed below. Please note that some features are not yet implemented but already integrated in the grammar (for instance, higher-order functions and scopes).

```
program = (assignment)* output

assignment = symbol '(' symbol ':' type ')' ':' type '->' expression ';'
           | symbol '=' expression ';'
output = '>' expression ';'

expression = 'if' expression 'then' expression 'else' expression
           | logic
logic = arithmetic (('==' | '<' | '>' | '<=' | '>=') arithmetic)?
arithmetic = atom (('+' | '-') atom)*
atom = symbol '(' expression ')'
     | symbol
     | integer
     | boolean
     | '(' expression ')'

type = 'integer' | 'boolean' | '(' type ')' '->' type;
```
