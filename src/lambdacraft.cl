;;===============================================================================
;; MIT License
;;
;; Copyright (c) 2022 Hikaru Ikuta
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;===============================================================================
(defparameter profile-index-depth nil)
(defparameter lambdacraft-loaded t)
(defparameter **error-on-undefined-var** t)

(defun islambda (expr)
  (and (not (atom expr)) (atom (car expr)) (eq 'lambda (car expr))))

(defun lambdaargs (expr)
  (car (cdr expr)))

(defun lambdaarg-top (expr)
  (car (car (cdr expr))))

(defun lambdabody (expr)
  (car (cdr (cdr expr))))

(defun decorate-varname (var)
  (concatenate 'string "[" (write-to-string var) "]"))

(defun curry (expr)
  (labels
    ((normalize-app (ret l)
       (if (not l) ret (normalize-app (list ret (curry (car l))) (cdr l))))
     (curry-lambda (args body)
       `(lambda (,(car args))
          ,(if (= 1 (length args))
               (curry body)
               (curry-lambda (cdr args) body)))))
    (cond ((atom expr)
             expr)
          ((islambda expr)
             (curry-lambda (lambdaargs expr) (lambdabody expr)))
          ((= 1 (length expr))
             (curry (car expr)))
          (t
             (normalize-app (curry (car expr)) (cdr expr))))))

(defun to-de-bruijn (body env)
  (labels
    ((lookup (env var)
       (let ((i (position var env :test #'equal)))
         (if profile-index-depth
          (if i (format nil "~%~d:~a~%" (+ 1 i) (write-to-string var))
                (format nil "~%?:~a~%" (write-to-string var)))
          (if i (+ 1 i) (decorate-varname var)))
         )))
    (if (atom body)
        (list (lookup env body))
        (if (not (islambda body))
            `(app ,@(to-de-bruijn (car body) env) ,@(to-de-bruijn (car (cdr body)) env))
            `(abs ,@(to-de-bruijn (lambdabody body) (cons (lambdaarg-top body) env)))))))

(defun to-blc-string (body)
  (labels
    ((int2varname (n)
        (if (> n 0) (concatenate 'string "1" (int2varname (- n 1))) "0"))
     (token2string (token)
        (cond ((not token) "")
              ((eq token 'abs) "00")
              ((eq token 'app) "01")
              ((stringp token) token)
              (t (int2varname token)))))
    (let ((curstring ""))
      (loop
        (cond
          ((not body)
            (return curstring))
          (t
            (setq curstring (concatenate 'string curstring (token2string (car body))))
            (setq body (cdr body))))))))

(defun compile-to-blc (expr)
  (to-blc-string (to-de-bruijn (curry expr) nil)))


;;================================================================
;; The macro system
;;================================================================
(defparameter lazy-env (make-hash-table :test #'equal))
(defparameter lazy-var-list ())
(defparameter lazy-macro-list ())

(defmacro lazy-error (&rest message)
  `(error (concatenate 'string "Lazy K CL Error: " ,@message)))

(defun mangle-varname (name)
  (cond ((stringp name)
          (intern (concatenate `string (write-to-string name) "-**LAZY-VAR-STR**")))
        (t
          (intern (concatenate `string (write-to-string name) "-**LAZY-VAR**")))))

(defun mangle-macroname (name)
  (cond ((stringp name)
          (intern (concatenate `string (write-to-string name) "-**LAZY-MACRO-STR**")))
        (t
          (intern (concatenate `string (write-to-string name) "-**LAZY-MACRO**")))))

(defmacro def-lazy (name expr)
  `(progn
      (setf lazy-var-list (cons ',name lazy-var-list))
      (setf (gethash (mangle-varname ',name) lazy-env) ',expr)
      ',expr))

(defmacro defun-lazy (name args expr)
  `(progn
    (def-lazy ,name (lambda ,args ,expr))
    '(lambda ,args ,expr)))

(defmacro defmacro-lazy (name args &rest expr)
  (setf lazy-macro-list (cons name lazy-macro-list))
  `(progn
    (defun ,(mangle-macroname name) ,args ,@expr)
    '(,name ,args)))

(defun eval-lazy-var (name)
  (gethash (mangle-varname name) lazy-env))

(defun eval-lazy-macro (name argvalues)
  (apply (mangle-macroname name) argvalues))

(defun macroexpand-lazy-raw (expr env history)
  (cond ((atom expr)
          (cond ((position expr env :test #'equal)
                  expr)
                ((position expr history :test #'equal)
                  (lazy-error (format nil "Recursive expansion of macro/variable ~a. Expansion stack: ~a~%When writing recursive functions, please use anonymous recursion." expr (reverse (cons expr history)))))
                ((position expr lazy-var-list :test #'equal)
                  (macroexpand-lazy-raw (eval-lazy-var expr) env (cons expr history)))
                (t
                  expr)))
        ((islambda expr)
          `(lambda ,(lambdaargs expr)
            ,(macroexpand-lazy-raw (lambdabody expr) (append env (lambdaargs expr)) history)))
        ((position (car expr) lazy-macro-list :test #'equal)
          (macroexpand-lazy-raw (eval-lazy-macro (car expr) (cdr expr)) env history))
        (t
          (mapcar (lambda (expr) (macroexpand-lazy-raw expr env history)) expr))))

(defmacro macroexpand-lazy (expr)
  `(macroexpand-lazy-raw ',expr nil nil))

 
;;================================================================
;; Lazy K support (compilation to SKI combinator calculus)
;;================================================================
(defun count-occurrences-in (expr var)
  (cond ((atom expr) (if (equal var expr) 1 0))
        ((islambda expr)
         (if (equal (lambdaarg-top expr) var)
             0
             (count-occurrences-in (cdr (cdr expr)) var)))
        (t (reduce '+ (mapcar (lambda (x) (count-occurrences-in x var)) expr)))))

(defun occurs-freely-in (expr var)
  (cond ((atom expr) (equal var expr))
        ((islambda expr)
         (if (equal (lambdaarg-top expr) var)
             nil
             (occurs-freely-in (cdr (cdr expr)) var)))
        (t (or (occurs-freely-in (car expr) var)
               (occurs-freely-in (cdr expr) var)))))

(defun t-rewrite (expr)
  (cond ((atom expr) expr)
        ((equal 'lambda (car expr))
         (let ((arg  (lambdaarg-top expr))
               (body (lambdabody expr)))
              (cond ((equal arg body) 'I-comb**)
                    ((not (occurs-freely-in body arg))
                       `(K-comb** ,(t-rewrite body)))
                    ((islambda body)
                       (t-rewrite `(lambda (,arg) ,(t-rewrite body))))
                    (t `((S-comb** ,(t-rewrite `(lambda (,arg) ,(car body))))
                            ,(t-rewrite `(lambda (,arg) ,(car (cdr body)))))))))
        (t (mapcar #'t-rewrite expr))))

(defun flatten-ski (expr)
  (if (atom expr)
      (cond
        ((eq expr 'S-comb**) "s")
        ((eq expr 'K-comb**) "k")
        ((eq expr 'I-comb**) "i")
        (t (decorate-varname expr)))
      (concatenate `string "`" (flatten-ski (car expr)) (flatten-ski (car (cdr expr))))))

(defun compile-to-ski (expr)
  (flatten-ski (t-rewrite (curry expr))))


(defun rewrite-ski (expr)
  (if (atom expr)
      (cond
        ((eq expr 'S-comb**) "S")
        ((eq expr 'K-comb**) "K")
        ((eq expr 'I-comb**) "I")
        (t (decorate-varname expr)))
      (concatenate `string "(" (rewrite-ski (car expr)) (rewrite-ski (car (cdr expr))) ")")))

(defun compile-to-ski-parens (expr)
  (rewrite-ski (t-rewrite (curry expr))))


;;================================================================
;; Additional compilers
;;================================================================
(defparameter plaintext-lambda-env-vars
  (list
    "x" "y" "z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "α" "β" "γ" "δ" "ε" "ζ" "η" "θ" "κ" "μ" "ν" "ξ" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω"))

(defun int-to-alphabet (i)
  (if (< i (length plaintext-lambda-env-vars))
    (nth i plaintext-lambda-env-vars)
    (format nil "~a_~a"
      (nth (mod i (length plaintext-lambda-env-vars)) plaintext-lambda-env-vars)
      (floor i (length plaintext-lambda-env-vars)))))

(defun lambda-compiler-builder (app-format-var app-format-app abs-format)
  (let ((compiler ()))
    (setq compiler
      (lambda (body &optional (env ()))
        (labels
          ((lookup (env var)
            (let ((i (position var (reverse env) :test #'equal)))
              (if i
                (int-to-alphabet i)
                (if **error-on-undefined-var**
                  (lazy-error (format nil "Undefined variable ~a. Environment: ~a" var env))
                  (decorate-varname var))))))
          (cond
            ((atom body)
              (lookup env body))
            ((not (islambda body))
              (format nil
                "(~a ~a)"
                (let ((s (funcall compiler (car body) env)))
                  (if (and (not (atom (car body))) (not (islambda (car body))))
                    (subseq s 1 (- (length s) 1))
                    s))
                (funcall compiler (car (cdr body)) env)))
            (t
              (format nil abs-format
                (lookup (cons (lambdaarg-top body) env) (lambdaarg-top body))
                (let ((s (funcall compiler (lambdabody body) (cons (lambdaarg-top body) env))))
                  (if (islambda (lambdabody body))
                    (subseq s 1 (- (length s) 1))
                    s))))))))))

;; (defparameter to-plaintext-lambda* (lambda-compiler-builder "(~a ~a)" "((~a) ~a)" "\\~a.~a"))
;; (defun to-plaintext-lambda (&rest args)
;;   (apply to-plaintext-lambda* args))

(defparameter to-lam* (lambda-compiler-builder "(~a ~a)" "(~a ~a)" "(\\~a.~a)"))
(defun to-lam (&rest args)
  (apply to-lam* args))

(defparameter to-ml* (lambda-compiler-builder "(~a ~a)" "(~a ~a)" "(fun ~a -> ~a)"))
(defun to-ml (&rest args)
  (apply to-ml* args))

;; (defparameter to-js-arrow* (lambda-compiler-builder "~a(~a)" "(~a)(~a)" "(~a) => ~a"))
;; (defun to-js-arrow (&rest args)
;;   (apply to-js-arrow* args))

;; (defparameter to-js* (lambda-compiler-builder "~a(~a)" "(~a)(~a)" "function (~a) { return ~a; }"))
;; (defun to-js (&rest args)
;;   (apply to-js* args))

;; (defparameter to-python* (lambda-compiler-builder "~a(~a)" "(~a)(~a)" "lambda ~a: ~a"))
;; (defun to-python (&rest args)
;;   (apply to-python* args))

;; (defun compile-to-js (expr)
;;   (to-js (curry expr)))

;; (defun compile-to-js-arrow (expr)
;;   (to-js-arrow (curry expr)))

;; (defun compile-to-python (expr)
;;   (to-python (curry expr)))

;; (defun compile-to-plaintext-lambda (expr)
;;   (to-plaintext-lambda (curry expr)))

(defun compile-to-lam (expr)
  (to-lam (curry expr)))

(defun compile-to-ml (expr)
  (to-ml (curry expr)))


;;================================================================
;; Utilities
;;================================================================
;; (defmacro compile-to-plaintext-lambda-lazy (expr-lazy)
;;   `(compile-to-plaintext-lambda (macroexpand-lazy ,expr-lazy)))

(defmacro compile-to-ml-lazy (expr-lazy)
  `(compile-to-ml (macroexpand-lazy ,expr-lazy)))

(defmacro compile-to-lam-lazy (expr-lazy)
  `(compile-to-lam (macroexpand-lazy ,expr-lazy)))

(defmacro compile-to-blc-lazy (expr-lazy)
  `(compile-to-blc (macroexpand-lazy ,expr-lazy)))

(defmacro compile-to-ski-lazy (expr-lazy)
  `(compile-to-ski (macroexpand-lazy ,expr-lazy)))

(defmacro compile-to-ski-parens-lazy (expr-lazy)
  `(compile-to-ski-parens (macroexpand-lazy ,expr-lazy)))

;; (defmacro compile-to-js-lazy (expr-lazy)
;;   `(compile-to-js (macroexpand-lazy ,expr-lazy)))

;; (defmacro compile-to-js-arrow-lazy (expr-lazy)
;;   `(compile-to-js-arrow (macroexpand-lazy ,expr-lazy)))

;; (defmacro compile-to-python-lazy (expr-lazy)
;;   `(compile-to-python (macroexpand-lazy ,expr-lazy)))

(defmacro compile-to-lisp-lazy (expr-lazy)
  `(progn
    (setq *print-pretty* nil)
    (write-to-string (macroexpand-lazy ,expr-lazy))))

(defmacro compile-to-lisp-pretty-lazy (expr-lazy)
  `(progn
    (setq *print-pretty* t)
    (write-to-string (macroexpand-lazy ,expr-lazy))))
