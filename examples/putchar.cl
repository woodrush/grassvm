(load "./src/lambdavm.cl")

(def-lazy asm
  (list
    (list
      (cons4 inst-io t (list t t t t  t t t t  t t t t  t t t t   t nil t t t t t nil) io-putc))))

(format t "let main = ~a" (compile-to-ml-lazy asm))
