# GrassVM
GrassVM is a programmable virtual CPU for the [Grass](http://www.blue.sky.or.jp/grass/) programming language (also see [here](https://esolangs.org/wiki/Grass)).

GrassVM supports an extended version of the [ELVM](https://github.com/shinh/elvm) instruction set and architecture written by [Shinichiro Hamaji](https://github.com/shinh).
Using LambdaVM, you can enjoy assembly programming to write programs in the Grass programming language.

GrassVM is based on [LambdaVM](https://github.com/woodrush/lambdavm) and [LambdaCraft](https://github.com/woodrush/lambdacraft).
The implementation details for these projects are also available at the repos for [lambda-8cc](https://github.com/woodrush/lambda-8cc) and [LambdaLisp](https://github.com/woodrush/lambdalisp).

## How it Works
### Compilation Chain
GrassVM's compilation chain is based on [@susisu](https://github.com/susisu)'s [Grassy](https://github.com/susisu/Grassy) toolkit.
The toolkit includes `grass` and `plant`.
`grass` is a Grass interpreter.
`plant` is a Grass transpiler that transpiles an OCaml-style language to Grass.
For example programs on `plant`, see [Grasspiler](https://github.com/susisu/Grasspiler) written also by [@susisu](https://github.com/susisu).

LambdaVM is first compiled to the OCaml-like `plant` syntax using LambdaCraft.
The output is a one-liner lambda term in the format `let main _ = fun x -> fun y -> ...` and is saved as `out/grassvm.ml`.
For the reason why `main` takes an unused argument, see the language specs.

`out/grassvm.ml` is then compiled to Grass using `plant`.


### Evaluation Strategy
The original [LambdaVM](https://github.com/woodrush/lambdavm) assumes a lazy evaluation strategy.
On the other hand, Grass is an eagerly evaluated language as specified in the [language specs](http://www.blue.sky.or.jp/grass/) (in the "Intuitive explanation" section).
Several modifications were made to let LambdaVM run in an eager evaluation strategy.


## Example
To build [rot13.w]([rot13.w](https://gist.github.com/woodrush/360a4e741e40084b34ecfe3aa9e00c95)) (without the newline pretty printing):

```sh
make
```

The requirements are described later.

This will build the file `out/a.w`.
You can run it as:

```sh
$ make bin/grass
$ echo 'Hello, world!' | bin/grass rot13.w
Uryyb, jbeyq!
$ echo 'Uryyb, jbeyq!' | bin/grass rot13.w
Hello, world!
$ bin/grass rot13.w  # Also runs as a REPL
Hello, world!
Uryyb, jbeyq!
```

The main source code for the virtual machine is [lambdavm.cl](src/lambdavm.cl).
The assembly listing is written in [rot13.cl](src/rot13.cl).
For details on the assembly, please see the [LambdaVM](https://github.com/woodrush/lambdavm) repo.


## Build from Source
### Requirements
- [stack](https://docs.haskellstack.org/en/stable/), the Haskell Tool Stack (2.9.1)
- [SBCL](https://www.sbcl.org/) (2.1.11.debian)

### Build Instructions
This will build [rot13.w](https://gist.github.com/woodrush/360a4e741e40084b34ecfe3aa9e00c95) (without the newline pretty printing)
as out/a.w:

```sh
make
```

This will take several minutes.
