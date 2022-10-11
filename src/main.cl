(load "./src/yes.cl")


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


(defrec-lazy add* (initcarry is-add n m cont)
  (cond
    ((isnil n)
      (cont initcarry n))
    (t
      (do
        (<- (car-n cdr-n) (n))
        (<- (car-m cdr-m) (m))
        (<- (carry curlist) (add* initcarry is-add cdr-n cdr-m))
        (let* not-carry (not carry))
        (let* car-m (if is-add car-m (not car-m)))
        (let* f (lambda (a b)
          (if car-n
            (if car-m a b)
            (if car-m b a))))
        (<- (curbit nextcarry)
          ((lambda (cont)
            (do
              ((if (f carry not-carry)
                (cont t)
                (cont nil))
               (f car-m carry))))))
        (cont nextcarry (cons curbit curlist))))))


(defun-lazy inc-char (c)
  (do
    (<- (_ c) (add* nil t c (list t t t t t t t t)))
    c))


(def-lazy null-primitive-char ((+ 128 (succ 8)) *SUCC* W))
(def-lazy char-zero (list t t t t t t t t))



(defun-lazy main (OUT *SUCC* W IN)
  (do
    (<- (CHARTABLE _) (gen-char-table (list t t t t t t t t) null-primitive-char))
    (let* getchar (lambda (_)
      (do
        (let* addr2char (lambda (addr) (lookup-char-table CHARTABLE addr)))
        (let* query (IN (lambda (x) nil)))
        ;; Return nil for EOF
        (if-then-return (not (query query))
          nil)
        ((letrec-lazy matchchar (curaddr)
          (if (query (addr2char curaddr))
            curaddr
            (matchchar (inc-char curaddr))))
        (list t t t t t t t t)))))
    (let* putchar (lambda (c)
      (OUT (lookup-char-table CHARTABLE c))))
    (let* n (getchar W))
    (putchar n)
    (<- (_ n) (add* nil t n char-zero))
    (putchar n)
    (<- (_ n) (add* nil t n char-zero))
    (putchar n)
    (putchar n)
    (standalone nil)))


(format t (compile-to-ml-lazy main))


