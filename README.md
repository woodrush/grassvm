# GrassVM
GrassVM is a programmable virtual CPU for the [Grass](http://www.blue.sky.or.jp/grass/) programming language (also see [here](https://esolangs.org/wiki/Grass)).

GrassVM supports an extended version of the [ELVM](https://github.com/shinh/elvm) instruction set and architecture written by [Shinichiro Hamaji](https://github.com/shinh).
Using GrassVM, you can enjoy assembly programming to write programs in the Grass programming language.

GrassVM is based on [LambdaVM](https://github.com/woodrush/lambdavm) and [LambdaCraft](https://github.com/woodrush/lambdacraft).
The implementation details for these projects are also available at the repos for [lambda-8cc](https://github.com/woodrush/lambda-8cc) and [LambdaLisp](https://github.com/woodrush/lambdalisp).


## How it Works
### Compilation Chain
GrassVM's compilation chain is based on [@susisu](https://github.com/susisu)'s [Grassy](https://github.com/susisu/Grassy) toolkit.
The toolkit includes `grass` and `plant`.
`grass` is a Grass interpreter.
`plant` is a Grass transpiler that transpiles an OCaml-style language to Grass.
For example programs on `plant`, see [Grasspiler](https://github.com/susisu/Grasspiler) written also by [@susisu](https://github.com/susisu).

### Having Lots of Fun
LambdaVM is first compiled to the OCaml-like `plant` syntax using LambdaCraft.
The output is a one-liner lambda term:

```ocaml
let main = (fun x -> fun y -> ((fun z -> ((fun a -> ((fun b -> (a (fun c -> (b b c)))) 
(fun b -> (a (fun c -> (b b c)))))) (fun a -> fun b -> fun c -> fun d -> ((fun e -> ((
fun f -> (f (fun g -> fun h -> fun i -> fun i -> fun k -> k) (fun g -> fun h -> g))) e
)) b (fun e -> (d c (Succ c))) (fun e -> (b (fun f -> fun g -> (a g c (fun h -> fun c 
-> (a g c (fun j -> fun c -> (d (fun l -> (l h j)) c)))))))) (fun e -> e))) z ((fun a 
-> fun b -> ((fun a -> fun b -> ((fun a -> fun b -> (a (a b))) ((fun a -> fun b -> ((
...
```

This file is saved as `out/rot13.ml`.
For the reason why `main` takes an unused argument, please see the language specs.

`out/grassvm.ml` is then compiled to Grass using `plant`.

### Evaluation Strategy
The original [LambdaVM](https://github.com/woodrush/lambdavm) assumes a lazy evaluation strategy.
On the other hand, Grass is an eagerly evaluated language as specified in the [language specs](http://www.blue.sky.or.jp/grass/) (in the "Intuitive explanation" section).
Several modifications were made to let LambdaVM run in an eager evaluation strategy.

### I/O Wrapper
GrassVM first creates a binary tree that stores all of the 256 characters from 0 to 255.
This structure is used to convert to and from Grass's primitive character object to a list of booleans.
Using this structure, it defines the functions `putchar` and `getchar` which is later referenced inside LambdaVM.
The definitions for these structures are written in [grassvm.cl](src/lambdavm.cl).

### Source Codes
The main source code for the virtual machine is [lambdavm.cl](src/lambdavm.cl).
The I/O wrapper is written in [grassvm.cl](src/lambdavm.cl).
The assembly listing is written in [rot13.cl](src/rot13.cl).
For details on the assembly, please see the [LambdaVM](https://github.com/woodrush/lambdavm) repo.


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
