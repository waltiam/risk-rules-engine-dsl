; (in-package "common-lisp-user")
(unlock-package "SB-ALIEN")

(defun double (x) (* 2 x))

(defun fib (limit)
  (let* ((i 1)
         (j 1))
    (do ((f (cons i j) (cons f (reduce '+ (list i j)))))
        ((< limit (car f)) f)
      (progn (print f) (setq i j j (car f))))))

(defun average (x y) (/ (+ x y) 2))

(defun square (x) (* x x))

(defun one-more-p (x y) (= 1 (- y x)))

; ha - namespaces in lisp
(setq double 4)

(pprint (double double))

(double double)

; compilation

(defun foo (X) (1+ x))

(defun powers (x)
  (values x (sqrt x) (expt x 2)))

; imperative
; (defun fun (x) 
;   (list 'a (expt (car x) 2)))

(defun myfunc (a b))

; https://lisp-docs.github.io/cl-language-reference/chap-5/f-d-dictionary/apply_function
; https://common-lisp.net/documentation
; https://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html

; recursive, this example is not tail recursive because the recursive call is further called in the `(* x (factorial ...))`

(defun factorial (x)
  "Factorial (x!) function, where 1 * 2 ... * n.  Using recursion"
  (if (= x 0)
      1
      (* x (factorial (- x 1)))))

(defun factorial-tail (x &optional (acc 1))
  "Factorial (x!) function, where 1 * 2 ... * n.  Using tail recursion _should_ be more performant."
  (if (= x 0)
      acc
      (factorial-tail (- x 1) (* x acc))))

; as a comparison - the none recursive version
(defun factorial-loop (n)
  "Factorial (n!) function, where 1 * 2 ... * n.  Using a loop."
  (let ((result 1))
    (dotimes (i n result)
      (setf result (* result (+ i 1))))))

; need to explore performance measuring
; https://archive.org/details/PerformanceAndEvaluationOfLispSystems/mode/2up


; macros
(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

; try these - 
(when (= 1 1) (print "true") (print "same as if but with multiple 'forms'") nil)

(when (= 1 1) '(print "true") '(print "same as if but with multiple 'forms'") '(and only the last form being returned))

; functional

(mapcar #'(lambda (x) (* x x)) '(1 2 3))

; math operators
(+ 1 4) ; -> 1 + 4 = 5
(- 1 4) ; -> 1 - 4 = -3
(/ 4 2) ; -> 4 / 2 = 2 
(* 4 2) ; -> 4 * 2 = 8 

; lists


(defparameter my-list '(1 2 3 4))
(car my-list)
(cdr my-list)
(cons 0 my-list)

; is prime
(defun divisible-p (a b) (= 0 (mod a b)))
(defun prime-p (n)
  (if (< n 2)
      nil
      (loop for i from 2 to (sqrt n) never (divisible-p n i))))

;numbers
1
23.45
1/5

;chars
#\G
#\5

;strings
""
" a value"
" over two
lines"

; s-expressions (symbolic expressions)
42 ; numbers
'foo ; symbols
'(foo 2.3) ; lists
(+ 2 4) ; also a list

; lists
(cons '0 '(1 2))
(cons 1 (cons 2 (cons 3 nil)))
; same as 
(list 1 2 3)

; macros
(defmacro do-math-apply (op &rest args)
  `(apply ,op ',args))

(defmacro do-math-funcall (op &rest args)
  `(funcall ,op ,@args))

(do-math-apply '+ 1 2 3 4)

;atoms 
42
"hey there"
:dog ; keyword

; quoted
'dog ; quoted symbol
`dog

; scope 
(let ((x 23)
      (y 45))
  (+ x y))