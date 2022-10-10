(load "./src/lambdacraft.cl")
(load "./src/macros.cl")

;; (def-lazy char-table
;;   (cons
;;     (cons ((+ 128 (succ 8)) *SUCC* W) (*SUCC* W))
;;     (cons (2 *SUCC* W) ((succ 2) *SUCC* W))))

(defrec-lazy inf-w (x)
  (inf-w (OUT (*SUCC* x))))

(defrec-lazy gen-char-table (depth curchar cont)
  (if (isnil depth)
    (cont curchar (*SUCC* curchar))
    (do
      (<- (_ cdr-depth) (depth))
      (<- (left-tree curchar) (gen-char-table cdr-depth curchar))
      (<- (right-tree curchar) (gen-char-table cdr-depth curchar))
      (cont (cons left-tree right-tree) curchar))))

(defun-lazy main (OUT *SUCC* W IN)
  (do
    (<- (char-table curchar) (gen-char-table (list t t t t t t t t) ((+ 128 (succ 8)) *SUCC* W)))
    (OUT (char-table t nil t nil t nil t t))))

(format t (compile-to-ml-lazy main))

