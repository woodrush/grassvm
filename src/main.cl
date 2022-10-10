(load "./lambdacraft.cl")

(defun-lazy main (OUT SUCC W IN)
  (OUT W))

(format t (compile-to-ml-lazy main))
