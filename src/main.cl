(load "./src/lambdavm.cl")
(load "./src/grassvm.cl")

(def-main (LambdaVM 8 16 memlist proglist nil))

(defun-lazy GrassVM (memlist proglist)
  main)


(defrec-lazy IntBuilder (cont curlist option)
  (if option
    ;; t -> construct int
    (lambda (b1 b2 b3 b4 b5 b6 b7 b8 b9 b10
             b11 b12 b13 b14 b15 b16 b17 b18 b19 b20
             b21 b22 b23 b24)
      (IntBuilder
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

(defrec-lazy ProgBuilder (cont curlist curtag input)
  (if (iscons4-not-nil input)
    ;; The input is a cons4 -> build tag
    (ProgBuilder cont curlist (cons input curtag))
    ;; The input is nil
    (if (isnil curtag)
      ;; curtag is closed -> apply the program list to the VM and return it
      (cont curlist)
      ;; curtag is open -> close tag and stack it on curlist
      (ProgBuilder cont (cons curtag curlist) nil))))

(def-lazy GrassVMCore
  (ProgBuilder
    (lambda (proglist)
      (IntBuilder (GrassVM proglist) nil))
    nil
    nil))

(format t "let main = ~a" (compile-to-ml-lazy GrassVMCore))
