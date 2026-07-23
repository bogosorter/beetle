<div align="center">
    <img src="./images/beetle-small.png">
</div>

# beetle

The programming language that embraces bugs.

## About

*beetle* is a simple functional programming language. I make no pretense of being a knowledgeable language designer (linguist?), but I hope that tinkering around will teach me a little about compilers. *beetle*'s compiler is written in Haskell and outputs code in the LLVM Intermediate Representation.

*beetle* programs consist of a number of assignments followed by a return expression (the program's output). As an example, consider this naive implementation of the *Red, Green, and Blue Tiles* problem from [Project Euler](https://projecteuler.net/problem=117):

```
tiles(n: integer): integer =
    if n < 0: return 0; -- impossible
    if n == 0: return 1; -- empty sequence
    return tiles(n - 1) + tiles(n - 2) + tiles(n - 3) + tiles(n - 4);

return tiles(5);
```

The next example is more involved: it evaluates an expression tree using both record and sum types. Notice how, instead of using pattern-matching, *beetle* relies on type assertions. Additionally, instead of introducing a new variable, the previous variable is automatically lowered to match the asserted type. Finally, the lines that evaluate `left` and `right` show that, since the compiler can deduce that all possible types for `expression` are records with `left` and `right` members, their values can be read even though the exact type of `expression` hasn't been determined yet.

```
Expression
    = Literal Integer
    | Addition {left: Expression, right: Expression}
    | Subtraction {left: Expression, right: Expression}
    | Multiplication {left: Expression, right: Expression}
    | Division {left: Expression, right: Expression}
    ;

evaluate(expression: Expression): Integer =
    if expression is Literal: return expression;

    left = evaluate(expression.left);
    right = evaluate(expression.right);

    if expression is Addition: return left + right;
    if expression is Subtraction: return left - right;
    if expression is Multiplication: return left * right;
    return left / right;

return evaluate(
    Addition {
        left: Literal 1,
        right: Subtraction {
            left: Multiplication {
                left: Literal 2,
                right: Division {left: Literal 4, right: Literal 2}
            },
            right: Literal 2
        }
    }
);
```

Other examples can be found under the `tests` directory (the most relevant examples are under `tests/complete` and `tests/functions`).

## Installing & Running

Executables for Linux users can be found under the [releases page](https://github.com/bogosorter/beetle/releases). [`clang`](https://clang.llvm.org/) is required. An example usage follows:

```
$ beetle fibonacci.btl
$ ./fibonacci
13
```

The `-o` flag can be used to specify an output file, and the flags `-ast`, `-tc`, `-s`, `-ir` and `-ll` can be used to generate files with the AST, type-checked AST, simplified AST, Intermediate Representation and LLVM codes.

## Details

*beetle* is a tiny language. It is strictly typed, with the only supported primitive types being integers, booleans and characters. These may be composed using tuples, structs and lists. Closures enable higher-order functions and multiple-argument functions (which are desugared into chains of single-argument functions). Single-line comments start with `--`.

The language's grammar is under `documentation/grammar.md`. A curious particularity, which I have not yet seen in any other language, is that there are two different types of expressions, `returnExpressions` and plain `expressions`. Since this is a functional language, pretty much everything is an expression, but `returnExpressions` also allow the definitions of variables, `if` usage and must terminate with a `return`. This allows a clean syntax (similar to Python's, without cluttering from curly braces and `let` statements) while avoiding whitespace sensitivity. I must admit I'm quite proud of it.
