(load "./src/lambdacraft.cl")
(load "./src/macros.cl")

(def-lazy char-table
  (cons
    (cons W (*SUCC* W))
    (cons (2 *SUCC* W) ((succ 2) *SUCC* W))))

(defun-lazy main (OUT *SUCC* W IN)
  (OUT (cdr (car char-table))))

(format t (compile-to-ml-lazy main))

