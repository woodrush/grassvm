(load "./src/lambdacraft.cl")
(load "./src/macros.cl")

(defun-lazy main (OUT *SUCC* W IN)
  (OUT
    (if (W W)
      W
      (*SUCC* (*SUCC* W)))))

(format t (compile-to-ml-lazy main))

