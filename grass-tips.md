# Grass Programming Tips

## Examples
### Printing `w`
```text
wWWwwwwv ; push (lambda (x) (Out u)) := print_u
w        ; defines main
WWw      ; apply print_u main
WWWw     ; apply print_u main
WWWWw    ; apply print_u main
WWWWWw   ; apply print_u main
```

This prints `wwww`.

- The first line defines `(lambda (x) (Out w)) := printw`.
  Since the VM is at the initial state, the De Bruijn indices for `Out` and `w` are 2 = 1 + 1 and 4 = 1 + 3.
  The extra 1 is created by the surrounding abstraction.
  `w` is written as `u` to prevent it from being parsed.
- The second line defines `main`. In Grass, the final function at the top of the stack (here, `main`)
  is called with itself as the argument when the program starts.
  To handle this specification, `main` takes an extra unused parameter.
- `printw` is then called with `main`. The argument can be anything since it is unused.
- The De Bruijn index for `printw` increases (`WW`, `WWW`, `WWWW`, `WWWWW`) since the result of `apply printw main` are being pushed to the top of the stack
  as `apply printw main` is executed.
  - The argument `w` first points to `main`, but later points to the result of the previous `apply printw main`.

### Destructing a Cons Cell

```text
wWwwwwWwwwwwv ; push (cons u u)
wwv           ; push nil
w             ; def-main
WWWww         ; apply (cons u u) nil
WWWWWw        ; apply Out 1
```

This prints `w`.

1. Creates `(cons w w)` (`w` is written as `u` to avoid from it being parsed)
   Here, `w` is the Grass primitive function which has the De Bruijn index 4 = 1 + 3. The extra 1 is for the surrounding abstraction.
1. Creates `nil`
1. The definition of main starts here
1. `nil` is applied to `(cons u u)`
1. `Out` is applied to the top of the stack.
   `Out` is a Grass primitive, and has the De Bruijn index of 5 at this point in the program.

## Resources
The following blog post (in Japanese) helped very much in understanding Grass:
- https://youz.hatenablog.com/entry/20080601/1212303662