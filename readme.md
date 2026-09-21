<div align="center">
    <img src="./images/beetle-small.png">
</div>

# beetle

The programming language that embraces bugs.

## About

*beetle* is a simple functional programming language. I make no pretense of being a knowledgeable language designer (linguist?), but I hope that tinkering around will teach me a little about compilers. *beetle*'s compiler is written in Haskell and outputs code in the LLVM Intermediate Representation. It features first-order functions and closures, sum types (including recursively defined types), and parametric polymorphism.

*beetle* programs consist of a number of assignments followed by a return expression (the program's output). As an example, consider this naive implementation of the *Red, Green, and Blue Tiles* problem from [Project Euler](https://projecteuler.net/problem=117):

```
tiles(n: Integer) -> Integer {
    if n < 0: return 0; -- impossible
    if n == 0: return 1; -- empty sequence
    return tiles(n - 1) + tiles(n - 2) + tiles(n - 3) + tiles(n - 4);
}

return tiles(5);
```

The next example is more involved: it evaluates an expression tree using both record and sum types. Notice that, while evaluating `left` and `right`, the compiler can deduce that all possible types for `expression` are records with `left` and `right` members, and their values can be read even though the exact type of `expression` hasn't been determined yet.

```
type Expression {
    Literal(Integer),
    Addition({left: Expression, right: Expression}),
    Subtraction({left: Expression, right: Expression}),
    Multiplication({left: Expression, right: Expression}),
    Division({left: Expression, right: Expression})
}

eval(expression: Expression) -> Integer {
    if expression is Literal(n): return n;

    left = eval(expression.left);
    right = eval(expression.right);

    match expression {
        Addition -> left + right,
        Subtraction -> left - right,
        Multiplication -> left * right,
        Division -> left / right
    }
}

return eval(
    Addition({
        left: Literal(1),
        right: Subtraction({
            left: Multiplication({
                left: Literal(2),
                right: Division({left: Literal(4), right: Literal (2)})
            }),
            right: Literal(2)
        })
    })
);
```

Other examples can be found under the `tests` directory (the most relevant examples are under `tests/complete`).

## Installing & Running

Executables for Linux users can be found under the [releases page](https://github.com/bogosorter/beetle/releases). [`clang`](https://clang.llvm.org/) is required. An example usage follows:

```
$ beetle fibonacci.btl
$ ./fibonacci
13
```

The `-o` flag can be used to specify an output file, and the flags `-ast`, `-tc`, `-s`, `-ir` and `-ll` can be used to generate files with the AST, type-checked AST, simplified AST, Intermediate Representation and LLVM codes.

## Details

*beetle* is a tiny language. It is strictly typed, with the only supported primitive types being integers, booleans and characters. These may be composed using tuples, structs and lists. Closures enable higher-order functions and multiple-argument functions (which are desugared into chains of single-argument functions). *beetle* supports parametric polymorphism. Single-line comments start with `--`.
