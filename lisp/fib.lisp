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