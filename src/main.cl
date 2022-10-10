(load "./src/lambdacraft.cl")
(load "./src/macros.cl")

;; (def-lazy char-table
;;   (cons
;;     (cons ((+ 128 (succ 8)) *SUCC* W) (*SUCC* W))
;;     (cons (2 *SUCC* W) ((succ 2) *SUCC* W))))

(defrec-lazy inf-w (x)
  (inf-w (OUT (*SUCC* x))))

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


(def-lazy getchar
  (do
    (let* query (IN (lambda (x) nil)))
    ;; Return nil for EOF
    (if-then-return (not (query query))
      nil)
    ((letrec-lazy matchchar (curaddr)
      (if (query (addr2char curaddr))
        curaddr
        (matchchar (inc-char curaddr))))
     (list t t t t t t t t))))


(defun-lazy inc-char (c)
  (do
    (<- (_ c) (add* nil t c (list t t t t t t t t)))
    c))

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



(def-lazy null-primitive-char ((+ 128 (succ 8)) *SUCC* W))

(defun-lazy main (OUT *SUCC* W IN)
  (do
    (<- (CHARTABLE _) (gen-char-table (list t t t t t t t t) null-primitive-char))
    (let* addr2char (lambda (addr) (lookup-char-table CHARTABLE addr)))
    (let* char-table CHARTABLE)
    ;; (let* n (list t nil t nil t nil t nil))
    (let* n getchar)
    (let* char-zero (list t t t t t t t t))
    (OUT (lookup-char-table char-table n))
    (<- (_ n) (add* nil t n char-zero))
    (OUT (lookup-char-table char-table n))
    (OUT (char-table t nil t nil t nil t t))))


(format t (compile-to-ml-lazy main))

