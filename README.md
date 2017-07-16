# tinyrw

**tinyrw** is a toy language for defining [rewriting systems](https://en.wikipedia.org/wiki/Rewriting) and executing them.

## Building

To build, you need the [`smlnj`](http://www.smlnj.org/) compiler. Given that you have it installed, run
```
sml -m main.cm
```
You may then run `script/repl.sh` to start a REPL which is currently the only way to use **tinyrw**.

## Usage

Let us define basic natural number arithmetic to illustrate the usage. We first run `./script/repl.sh` to get the repl started.

To define a new rule, we use the `:rule` command. There are two rewrite
rules needed to define addition on natural numbers:
```
> :rule plus(z, Y) => Y
New rule: plus(z, Y@0)  --->  Y@0.
> :rule plus(s(X), Y) => s(plus(X, Y))
New rule: plus(s(X@1), Y@0)  --->  s(plus(X@1, Y@0)).
```

To keep rewriting a term until no further rules apply, we have a command
`:norm` for normalizing terms.
```
> :norm plus(s(s(s(z))), s(s(s(z))))
s(s(s(s(s(s(z))))))
```
