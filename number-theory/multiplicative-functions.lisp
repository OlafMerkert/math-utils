(defpackage :number-theory/multiplicative-functions
  (:use :cl :ol
        :iterate
        :number-theory)
  (:export
   #:apply-multiplicative
   #:defmultfun))

(in-package :number-theory/multiplicative-functions)

(defun apply-multiplicative (mfun n &key (combination #'*))
  "Apply a multiplicative function, given by (MFUN p e), where p^e is a prime power, on the integer N. The results of MFUN on the prime power decomposition of N are then put together with the binary COMBINATION."
  (let ((factors (factorise n :singletons)))
    (reduce combination
            factors
            :key (lambda (x) (funcall mfun (car x) (cdr x))))))

(defmacro! defmultfun (name args &body body)
  "Define a multiplicative function by defining its behaviour for
  prime powers.  Anaphoric vars are P and E.  Additionally, more ARGS can be defined"
  `(defun ,name (,g!n . ,args)
     (apply-multiplicative (lambda (p e) (declare (ignorable p e)) ,@body) ,g!n)))

;; simple examples
(defmultfun euler-phi ()
  (* (- p 1) (expt p (- e 1))))

(defmultfun moebius ()
  (if (< 1 e)
      0
      -1))

(defmultfun euler-phi2 ()
  (* (- (expt p 2) 1) (expt p (* 2 (- e 1)))))

(defmultfun liouville ()
  (if (oddp e)
      -1
      1))

(defmultfun divisor-sum (k)
  (iter (for i from 0 to e)
        (summing (expt p (* i k)))))

(defmultfun convolution (f g)
  (iter (for i from 0 to e)
        (summing (* (funcall f (expt p i))
                    (funcall g (expt p (- e i)))))))

;;; some experimental stuff
(defun crt-map-to-product (n)
  (apply-multiplicative
   (lambda (p e)
     (lambda (x)
       (mod x (expt p e))))
   n
   :combination #'list+))

(defun list+ (car cdr)
  (if (consp cdr)
      (cons car cdr)
      (list car cdr)))


(defun to-product-function (list-of-functions)
  (lambda (x)
    (mapcar (lambda (y) (funcall y x))
            list-of-functions)))
