(defun-lazy t (x y) x)
(defun-lazy nil (x y) y)
(defun-lazy cons* (x y f) (f x y))
(defun-lazy car* (l) (l t))
(defun-lazy cdr* (l) (l nil))
(defmacro-lazy car (l) `(,l t))
(defmacro-lazy cdr (l) `(,l nil))
(defmacro-lazy cons (x y) `(lambda (f) (f ,x ,y)))

(defun-lazy isnil (l) ((lambda (a) (a (lambda (v n x) nil) t)) l))

(defmacro-lazy not (x) `(,x nil t))
(defmacro-lazy and (x y) `(,x ,y nil))
(defmacro-lazy or (x &rest r)
  (if (not r) x `(,x t (or ,@r))))

(defmacro-lazy xor (x y) `(if ,x (not ,y) ,y))
(defun-lazy xnor (x y) (if x y (not y)))

(defmacro-lazy succ (n) `(lambda (f x) (f (,n f x))))
(defun-lazy pred (n f x) (n ((lambda (g h) (h (g f)))) (lambda (u) x) (lambda (u) u)))
(defmacro-lazy + (m n) `(lambda (f x) (,m f (,n f x))))
(defmacro-lazy - (m n) `(,n pred ,m))
(defmacro-lazy * (m n) `(lambda (f x) (,m (,n f) x)))
(defmacro-lazy iszero (n) `(,n (lambda (x) nil) t))

(defmacro-lazy <= (m n) `(iszero (- ,m ,n)))
(defmacro-lazy < (m n) `(<= (succ ,m) ,n))
(defmacro-lazy >= (m n) `(<= ,n ,m))
(defmacro-lazy = (m n) `(and (<= ,m ,n) (<= ,n ,m)))
(defun-lazy 0 (f x) x)
(defun-lazy 1 (f x) (f x))
(defun-lazy 2 (f x) (f (f x)))
(def-lazy 4 ((lambda (x) (x x)) 2))
(def-lazy 8 (* 2 4))
(def-lazy 16 ((lambda (x) (x x x)) 2))
(def-lazy 32 (* 2 16))
(def-lazy 64 (* 2 32))
(def-lazy 128 (* 2 64))
(def-lazy 256 ((lambda (x) (x x)) 4))

(defmacro-lazy if (x y z) `((,x (lambda (x) ,y) (lambda (x) ,z)) (lambda (x) x)))

(defmacro-lazy let (argpairs body)
  ;; Syntax: (let ((x1 v1) (x2 v2) ...) body)
  (labels
    ((let-helper (argpairs)
      (cond ((not argpairs) body)
            (t `((lambda (,(car (car argpairs))) ,(let-helper (cdr argpairs)))
                 ,(car (cdr (car argpairs))))))))
    (let-helper argpairs)))

(defmacro-lazy cond (&rest clauses)
  (cond ((not (cdr clauses))
           (cond ((not (eq (car (car clauses)) t))
                    (lazy-error "No default case provided for cond"))
                 (t (car (cdr (car clauses))))))
        (t `(if ,(car (car clauses))
              ,(car (cdr (car clauses)))
              (cond ,@(cdr clauses))))))

(defmacro-lazy list (&rest items)
  (if items
    `(cons ,(car items) (list ,@(cdr items)))
    `nil))

(defmacro-lazy list-tail (item &rest items)
  (if items
    `(cons ,item (list-tail ,@items))
    item))

(defun-lazy nth (n list)
  (-> list (n cdr*) car*))

(defmacro-lazy nth (n list)
  `(-> ,list (,n cdr*) car*))


(def-lazy Z-comb
  (lambda (f)
    ((lambda (x) (f (lambda (y) (x x y))))
     (lambda (x) (f (lambda (y) (x x y)))))))

(defmacro-lazy letrec-lazy (name args body)
  (if args
    `(Z-comb (lambda (,name) (lambda ,args ,body)))
    `(Z-comb (lambda (,name) ,body))))

(defmacro defrec-lazy (name args body)
  `(def-lazy ,name (letrec-lazy ,name ,args ,body)))


(defmacro-lazy -> (target &rest args)
  (if (not args)
    target
    `(-> (,(car args) ,target) ,@(cdr args))))

(defun-lazy take (n l)
  ((letrec-lazy take (n l ret)
      (cond
        ((iszero n)
          (reverse ret))
        (t
          (take (pred n) (cdr l) (cons (car l) ret)))))
   n l nil))

(defrec-lazy length (l)
  ((letrec-lazy length (l n)
      (if (isnil l)
        n
        (length (cdr l) (succ n))))
    l 0))

(defrec-lazy reverse* (l curlist)
  (if (isnil l) curlist (reverse* (cdr l) (cons (car l) curlist))))
(defun-lazy reverse (l)
  (reverse* l nil))

(defmacro-lazy if-then-return (condition then else)
  `(if ,condition ,then ,else))

(defmacro-lazy let* (name value body)
  `(let ((,name ,value)) ,body))

(defmacro-lazy do* (top &rest proc)
  (cond ((not proc)
          top)
        ((eq '<- (car (car proc)))
          (let* ((topproc (car proc))
                 (arglist (car (cdr topproc)))
                 (body (car (cdr (cdr topproc)))))
            `(do*
                ,(append body `((lambda ,arglist ,top)))
                ,@(cdr proc))))
        (t
          `(do*
              ,(append (car proc) `(,top))
              ,@(cdr proc)))))

(defmacro-lazy do (&rest proc)
  `(do* ,@(reverse proc)))

(defmacro-lazy typematch-nil-cons (expr cons-args nil-case cons-case)
  `(,expr
     (lambda ,cons-args
       (lambda (_) ,cons-case))
     ,nil-case))

