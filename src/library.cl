(load "./src/lambdavm.cl")


(def-lazy reg-A  (list t))
(def-lazy reg-B  (list nil t t))
(def-lazy reg-SP (list nil t nil))
(def-lazy reg-D  (list nil nil t))
(def-lazy reg-BP (list nil nil nil t))
(def-lazy reg-C  (list nil nil nil nil))


(defparameter formlist `(
    reg-A
    reg-B
    reg-C
    reg-D
    reg-SP
    reg-BP
))


(defun print-expression (expr)
  (format t (concatenate 'string (write-to-string expr)": " (eval `(compile-to-ml-lazy ,expr))))
  (terpri))

(mapcar #'print-expression formlist)
