(load "./src/lambdacraft.cl")
(load "./src/macros.cl")

(defun-lazy main (*OUT* *SUCC* *W* *IN*)
  (*OUT* (*SUCC* *W*)))

(format t (compile-to-ml-lazy main))
