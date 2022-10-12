(load "./src/lambdavm.cl")
(load "./src/grassvm.cl")

(def-main (LambdaVM 8 16 memlist proglist nil))

(defun-lazy GrassVM (memlist proglist)
  main)


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

(def-lazy GrassVMCore
  (MemlistBuilder
    (lambda (memlist)
      (ProglistBuilder (GrassVM memlist) nil nil))
    nil))

(def-lazy standalone
  (GrassVMcore
    ;; memlist
    t
    t t t t  t t t t  t t t t  t t t t  t nil t t t t nil t
    nil
    ;; proglist
    (cons4 inst-jmp t (list t t t t  t t t t  t t t t  t t t t  t t t t t t t t) nil)
    nil
    (cons4 inst-io nil (list t) io-putc)
    (cons4 inst-load t (list t t t t  t t t t  t t t t  t t t t  t t t t t t t t) (list t))
    nil
    nil
    ))
;; (format t "let main = ~a" (compile-to-ml-lazy GrassVMCore))
(format t "let main = ~a" (compile-to-ml-lazy standalone))
