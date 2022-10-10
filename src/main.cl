(load "./src/lambdacraft.cl")
(load "./src/macros.cl")

(def-lazy char-table
  (cons
    (cons ((+ 128 (succ 8)) *SUCC* W) (*SUCC* W))
    (cons (2 *SUCC* W) ((succ 2) *SUCC* W))))

(defrec-lazy inf-w (x)
  (inf-w (OUT (*SUCC* x))))

(defun-lazy main (OUT *SUCC* W IN)
  ;; (OUT (char-table t t))
  (inf-w W)
  )

(format t (compile-to-ml-lazy main))

