# GrassVM
GrassVM is a programmable virtual CPU for the [Grass](http://www.blue.sky.or.jp/grass/) programming language.

GrassVM supports an extended version of the [ELVM](https://github.com/shinh/elvm) instruction set and architecture written by [Shinichiro Hamaji](https://github.com/shinh).
Using GrassVM, you can enjoy writting Grass programs in assembly programming style.

GrassVM is based on [LambdaVM](https://github.com/woodrush/lambdavm) and [LambdaCraft](https://github.com/woodrush/lambdacraft).
The implementation details for these projects are also available at the repos for [lambda-8cc](https://github.com/woodrush/lambda-8cc) and [LambdaLisp](https://github.com/woodrush/lambdalisp).

### Lisp in Grass
GrassVM is now integrated into ELVM as the ELVM Grass backend.
Using ELVM, you can compile C to Grass.
An example is [lisp.c](https://github.com/shinh/elvm/blob/master/test/lisp.c) included in the ELVM repository.
The compiled Grass program [lisp.w](https://gist.github.com/woodrush/25372fe1c4bbc56c9598e09401d2190d) is available in [my Gist](https://gist.github.com/woodrush/25372fe1c4bbc56c9598e09401d2190d), together with a Makefile to build lisp.w from source.

Instead of writing C, you can hand-assemble Grass programs using the contents of this repo.


### Grass Programming Tips
I've documented tips on writing raw Grass programs in [grass-tips.md](grass-tips.md).

## Usage
### Requirements
- [stack](https://docs.haskellstack.org/en/stable/), the Haskell Tool Stack (2.9.1)
- [SBCL](https://www.sbcl.org/) (2.1.11.debian)

### Build Instructions
This will build [rot13.w](https://gist.github.com/woodrush/360a4e741e40084b34ecfe3aa9e00c95) (without the newline pretty printing):

```sh
make
```

This will build the file `out/rot13.w`.
This will take several minutes.

Note that running `make` will also install `grass` and `plant`
under the location specified by `stack`.
The Makefile assumes `~/.local/bin` as the installation location,
specified with the configuration `STACK_BIN_PATH=~/.local/bin`.
If `make` fails finding `grass` and `plant`, try changing this configuration.

`out/rot13.w` can be run as:

```sh
$ make bin/grass_ml
$ echo 'Hello, world!' | bin/grass_ml out/rot13.w
Uryyb, jbeyq!
$ echo 'Uryyb, jbeyq!' | bin/grass_ml out/rot13.w
Hello, world!
$ bin/grass_ml out/rot13.w  # Also runs as a REPL
Hello, world!
Uryyb, jbeyq!
```

### Building the GrassVM Core in the ELVM Grass Backend
To build the variable `GRASS_VM` in [wcore.h](https://github.com/woodrush/elvm/blob/grass-backend/target/wcore.h)
from the ELVM Grass backend, run:

```sh
make out/main.w
```

## How it Works
Here I will explain how [rot13.w](out/rot13.w) is built using GrassVM.

### Compilation Chain
GrassVM's compilation chain is based on [@susisu](https://github.com/susisu)'s [Grassy](https://github.com/susisu/Grassy) toolkit.
The toolkit includes `grass` and `plant`.
`grass` is a Grass interpreter.
`plant` is a Grass transpiler that transpiles an OCaml-style language to Grass.
For example programs for `plant`, see [Grasspiler](https://github.com/susisu/Grasspiler) written also by [@susisu](https://github.com/susisu).

### Having Lots of Fun
LambdaVM is first compiled to the OCaml-like `plant` syntax using LambdaCraft.
The output is a one-liner lambda term:

```ocaml
let main _ = (fun x -> fun y -> ((fun z -> ((fun a -> ((fun b -> (a (fun c -> (b b c))
)) (fun b -> (a (fun c -> (b b c)))))) (fun a -> fun b -> fun c -> fun d -> ((fun e ->
((fun f -> (f (fun g -> fun h -> fun i -> fun i -> fun k -> k) (fun g -> fun h -> g)))
e)) b (fun e -> (d c (Succ c))) (fun e -> (b (fun f -> fun g -> (a g c (fun h -> fun c
-> (a g c (fun j -> fun c -> (d (fun l -> (l h j)) c)))))))) (fun e -> e))) z ((fun a 
-> fun b -> ((fun a -> fun b -> ((fun a -> fun b -> (a (a b))) ((fun a -> fun b -> ((
...
```

This is generated by running [rot13.cl](src/rot13.cl) in Common Lisp.
This program generates the lambda term encodings for the assembly listing
and applies it to GrassVM, creating a standalone lambda term that can be run in a Grass interpreter
(after transpiling it with `plant`).
This file is saved as `out/rot13.ml`.
For the reason why `main` takes an unused argument, please see the language specs.

`out/rot13.ml` is then transpiled to Grass using `plant`.

### Source Codes
The main source code for the virtual machine is [lambdavm.cl](src/lambdavm.cl).
The I/O wrapper is written in [grassvm.cl](src/lambdavm.cl).
The assembly listing is written in [rot13.cl](src/rot13.cl).
For details on the assembly, please see the [LambdaVM](https://github.com/woodrush/lambdavm) repo.


## ELVM Grass Backend Implementation Details
rot13.w was compiled using `plant`.
Here, the entire source code including the assembly listing written in rot13.cl was compiled using `plant`.

In the ELVM Grass backend, instead of using `plant`, the assembly listing is directly written down as a raw Grass program.
This is done as follows:

- The [GrassVM Core](https://github.com/shinh/elvm/blob/master/target/wcore.h) containing GrassVM, the memory loader, and the program is first printed
- The memory and instruction list are printed

Here the GrassVM core is pre-compiled using `plant`, and included as wcore.h in the ELVM Grass backend.
The GrassVM core can be build by running `make out/main.w`.

The main difference between rot13.cl and the ELVM Grass backend is how the assembly listing is generated.
This is largely helped by the memory and program builders.

This section descirbes the modifications that were made to build the ELVM Grass backend,
along with other further implementations details.


### The Memory and Program Builder
GrassVM is based on [LambdaVM](https://github.com/woodrush/lambdavm).
LambdaVM accepts a memory list and program list, each expressed as a list of 24-bit integers
and a list of lists of instructions.
[rot13.cl](src/rot13.cl) builds these lists as built-in lambda terms compiled by LambdaCraft and `plant`.

To build these lists in the [ELVM Grass backend](https://github.com/woodrush/elvm/blob/grass-backend/target/w.c), the Grass source code for building these lists
must be generated in the C source code of ELVM.
To make this easy, GrassVM uses a stack machine which I named as the memory builder and program builder.

The memory and program builders are defined as follows in [main.cl](src/main.cl) as `MemlistBuilder` and `ProglistBuilder`.

#### Memory Builder
`MemlistBuilder` is defined as follows:

```lisp
(defrec-lazy MemlistBuilder (cont curlist option)
  (if option
    ;; t -> construct int
    (lambda (b1 b2 b3 b4 b5 b6 b7 b8 b9 b10
             b11 b12 b13 b14 b15 b16 b17 b18 b19 b20
             b21 b22 b23 b24)
      (MemlistBuilder
        cont
        (cons
          (list b1 b2 b3 b4 b5 b6 b7 b8 b9 b10
            b11 b12 b13 b14 b15 b16 b17 b18 b19 b20
            b21 b22 b23 b24)
          curlist)))
    ;; nil -> return int
    (cont curlist)))
```

`MemlistBuilder` is a state machine that behaves as follows:

- It maintains the state `curlist` and accepts the input argument `option`.
  - When `option` is `t`, it returns a function that accepts 24 arguments.
    The arguments are packed into a cons list with 24 elements, and pushed to `MemlistBuilder`'s internal state `curlist`.
    A new closure of `MemlistBuilder` with an updated `curlist` is then returned.
  - When `option` is `nil`, it applies `curlist` to `cont` and returns its result.

Since `MemlistBuilder` pushes its incoming item on top of the stack as `(cons next curlist)`,
the integers must be pushed to `MemlistBuilder` in reverse order when creating the list.
The ELVM Grass backend therefore reverses the memory list first when emitting the memory initialization clauses.

#### Program Builder
`ProglistBuilder` is defined as follows:

```lisp
(defun-lazy iscons4-not-nil (expr)
  (expr (lambda (a b c d) (lambda (x) t)) nil))

(defrec-lazy ProglistBuilder (cont curlist curtag input)
  (if (iscons4-not-nil input)
    ;; The input is a cons4 -> build tag
    (ProglistBuilder cont curlist (cons input curtag))
    ;; The input is nil
    (if (isnil curtag)
      ;; curtag is closed -> apply the program list to the VM and return it
      (cont curlist)
      ;; curtag is open -> close tag and stack it on curlist
      (ProglistBuilder cont (cons curtag curlist) nil))))
```

`ProglistBuilder` is a state machine that behaves as follows:

- It maintains the states `curlist` and `curtag`, each representing the tag list (the entire program) and the instruction list of the current program counter. It accepts an input `input`.
  - When `input` is a 4-tuple, i.e., it is a [LambdaVM instruction](https://github.com/woodrush/lambdavm#instruction-structure),
    - It pushes the instruction to the top of `curtag`, and returns a new closure of `ProglistBuilder` with an updated `curtag`.
  - When `input` is `nil`,
    - And when `curtag` is not `nil` and is a cons cell, it pushes `curtag` on top of `curlist`, and returns a new closure of `ProglistBuilder` with an updated `curlist`.
    This has the effect of incrementing the program counter.
    - When `curtag` is also `nil`, it applies `curlist` to `cont` and returns its result.
    This has the effect of finishing the program list construction, and extracting the constructed program list.

Similar to `MemlistBuilder`, `ProglistBuilder` also pushes its incoming instruction and tag on top of the stack as `(cons next curlist)`.
Therefore, the instructions must be pushed in reverse order when creating the list.
The ELVM Grass backend therefore reverses the instruction list first when emitting the program list.


### Self Application Specification
The memory and program builders are combined with GrassVM in main.cl as follows:

```lisp
(def-lazy GrassVMCore
  (MemlistBuilder
    (lambda (memlist)
      (ProglistBuilder
        (lambda (proglist)
          (lambda (x) (GrassVM memlist proglist))) nil nil))
    nil))
```

`GrassVMCore` first exposes `MemlistBuilder` to build the memory initialization clauses.
When the memory initialization finishes, it then exposes `ProglistBuilder` to build the program initialization clauses.
When `ProglistBuilder` finishes running, it returns the object `(lambda (x) (GrassVM memlist proglist))`,
encapsulating a standalone `GrassVM` applied with `memlist` and `proglist` thus making it ready to run.

The reason why `GrassVM` is contained in a lambda `(lambda (x) ...)` is because of the self-application specification of the Grass language.
In Grass, it is specified that when the interpreter encounters the final `v` definition clause in the program,
it takes the object at the top of the stack and calls it with the object itself as the argument.
After finishing building the memory and program, the object `(lambda (x) (GrassVM memlist proglist))`
is left at the top of the stack.
The interpreter therefore applies this object to itself to start the main execution phase.
This exposes the closure `(GrassVM memlist proglist)` to the execution region,
thus starting the execution of the program loaded to GrassVM.


### ELVM Implementation
Grass is a stack-based language. When a new value is created via function application,
the result is pushed to the top of the environment stack.
Due to the designs of the memory and program builder, after each integer or instruction is pushed to the builder functions,
the new closure is pushed to the top of the stack.
This makes writing raw Grass code for building the memory and programs easy, since the VM always stays at the top of the stack before pushing another integer or instruction.

Since all of the necessary Grass primitive function references are already contained inside the VM,
the ELVM implementation can forget about the total depth of the stack, so it suffices to only track the relative position of the VM within the environment stack.


### I/O Wrapper
GrassVM first creates a binary tree that stores all of the 256 characters from 0 to 255.
This structure is used to convert to and from Grass's primitive character object to a list of booleans.
Using this structure, it defines the functions `putchar` and `getchar` which is later referenced inside LambdaVM.
The definitions for these structures are written in [grassvm.cl](src/lambdavm.cl).

### Evaluation Strategy
The original [LambdaVM](https://github.com/woodrush/lambdavm) assumes a lazy evaluation strategy.
On the other hand, Grass is an eagerly evaluated language as specified in the [language specs](http://www.blue.sky.or.jp/grass/) (in the "Intuitive explanation" section).

Several modifications were made to let LambdaVM run in an eager evaluation strategy.
Most of the difficult changes were made by simply changing the definitions of `if` and `typematch-nil-cons` defined in [macros.cl](src/macros.cl).
The way Lisp abstracts these control flows with macros helped a lot when rewriting this program for a different evaluation strategy.


### Refining the Interpreter
GrassVM is tested using the interpreter [grass.ml](https://gist.github.com/woodrush/3d85a6569ef3c85b63bfaf9211881af6),
originally written by [@ytomino](https://github.com/ytomino) and modified by [@youz](https://github.com/youz) and [@woodrush](https://github.com/woodrush).
Here are the modifications made from the original implementation:

- Let the program compile on a newer version of ocamlc (by [@youz](https://github.com/youz))
  - This is done in [this revision](https://gist.github.com/woodrush/3d85a6569ef3c85b63bfaf9211881af6/revisions#diff-d45429b677faa4e32367de9accb2bb897144711a89ec8346165f896827cd52f8R125).
- Fix the behavior of the Grass character primitive (by [@youz](https://github.com/youz))
  - This is documented at the last section in [this blog entry](https://youz.hatenablog.com/entry/2020/05/04/204707) (in Japanese) by @youz.
  - The original interpreter throws an error when a non-character object is passed to character primitives.
    On the other hand, the [Grass language specs](http://www.blue.sky.or.jp/grass/) specify that `False` is returned in this case.
    The modified version fixes this issue.
- Flush the output every time when `Out` is called (by [@woodrush](https://github.com/woodrush))
  - Originally, the output was only flushed when a newline was printed.
    This makes programs such as [lisp.c](https://github.com/shinh/elvm/blob/master/test/lisp.c) in the ELVM repository not print the `> ` prompt until the user finishes typing their input, 
    since lisp.c doesn't print a newline until the user presses return.
    The modified version fixes this issue.


## Miscellaneous
### Writing Assembly in Raw Grass
In the previous section, the assembly was transpiled to lambda terms using `plant`.
You can also choose to directly write GrassVM assembly in raw Grass terms.
This is done in [asm.w](examples/asm.w), which is connected to `out/main.w`
which can be generated by `make out/main.w`.
The standalone Grass program can be built by `out/asm-standalone.w`.
This can be run by `make run-asm`.
