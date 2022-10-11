(load "./src/grassvm.cl")

;; (load "./src/yes.cl")
;; (load "./src/fizzbuzz.cl")
(load "./src/rot13.cl")


(def-main (standalone nil))

(format t "let main _ = ~a" (compile-to-ml-lazy main))
