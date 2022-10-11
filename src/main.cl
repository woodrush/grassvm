;; (load "./src/yes.cl")
;; (load "./src/fizzbuzz.cl")
(load "./src/rot13.cl")


;; *SUCC* captured from `main`
(defrec-lazy gen-char-table (depth curchar cont)
  (cond
    ((isnil depth)
      (cont curchar (*SUCC* curchar)))
    (t
      (do
        (<- (_ cdr-depth) (depth))
        (<- (left-tree curchar) (gen-char-table cdr-depth curchar))
        (<- (right-tree curchar) (gen-char-table cdr-depth curchar))
        (cont (cons left-tree right-tree) curchar)))))

(defrec-lazy lookup-char-table (curtable address)
  (cond
    ((isnil address)
      curtable)
    (t
      (do
        (<- (car-addr cdr-addr) (address))
        (lookup-char-table (curtable car-addr) cdr-addr)))))


;; add* defined in lambdavm.cl
(defun-lazy inc-char (c)
  (do
    (<- (_ c) (add* nil t c (list t t t t t t t t)))
    c))


(def-lazy null-primitive-char ((+ 128 (succ 8)) *SUCC* W))
(def-lazy char-zero (list t t t t t t t t))


(def-lazy main ;(OUT *SUCC* W IN)
  (do
    (let* char-zero char-zero)
    (let* add* add*)
    (let* lookup-char-table lookup-char-table)
    (<- (CHARTABLE _) (gen-char-table char-zero null-primitive-char))
    (let* getchar (lambda (_)
      (do
        (let* addr2char (lambda (addr) (lookup-char-table CHARTABLE addr)))
        (let* query (IN (lambda (x) nil)))
        (if-then-return (query query)
          ((letrec-lazy matchchar (curaddr)
            (if (query (addr2char curaddr))
              curaddr
              (matchchar (inc-char curaddr))))
          char-zero))
        ;; Return char-zero for EOF
        char-zero)))
    (let* putchar (lambda (c)
      (OUT (lookup-char-table CHARTABLE c))))
    ;; (let* n (getchar W))
    ;; (putchar n)
    ;; (<- (_ n) (add* nil t n char-zero))
    ;; (putchar n)
    ;; (<- (_ n) (add* nil t n char-zero))
    ;; (putchar n)
    ;; (putchar n)
    (standalone nil)))


(format t "let main _ = ~a" (compile-to-ml-lazy main))


