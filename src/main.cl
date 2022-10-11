(load "./src/lambdavm.cl")
(load "./src/grassvm.cl")

(def-main (LambdaVM 8 16 memlist proglist nil))

(defun-lazy GrassVM (memlist proglist)
  main)


(format t "let main = ~a" (compile-to-ml-lazy GrassVM))
