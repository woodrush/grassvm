# GrassVM Details
## Refining the Interpreter
GrassVM is tested using the interpreter [grass.ml](https://gist.github.com/woodrush/3d85a6569ef3c85b63bfaf9211881af6),
originally written by [@ytomino](https://github.com/ytomino) and modified by [@youz](https://github.com/youz) and [@woodrush](https://github.com/woodrush).
Here are the modifications made from the original implementation:

- Let the program compile on a newer version of ocamlc (by @youz)
  - This is done in [this revision](https://gist.github.com/woodrush/3d85a6569ef3c85b63bfaf9211881af6/revisions#diff-d45429b677faa4e32367de9accb2bb897144711a89ec8346165f896827cd52f8R125).
- Fix the behavior of the Grass primitive `In` (by @woodrush)
  - The original interpreter throws an error when a non-character object is passed to `In`.
    On the other hand, the [Grass language specs](http://www.blue.sky.or.jp/grass/) specify that:
    - `In` can accept non-character objects.
    - `In` returns the Grass character object matching the standard input when the standard input is not EOF.
    - When the standard input is EOF, `In` returns its argument as is, i.e., it behaves as the identity function.
  - The interpreter is fixed so that these specifications are met, instead of throwing an error.
- Flush the output every time when `Out` is called
  - Originally, the output was only flushed when a newline was printed.
    This makes programs such as [lisp.c](https://github.com/shinh/elvm/blob/master/test/lisp.c) in the ELVM repository not print the `> ` prompt until the user finishes typing their input, 
    since lisp.c doesn't print a newline until the user presses return.
