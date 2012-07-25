(defpackage :number-theory/primes
  (:nicknames :nt-p)
  (:import-from :cl-utilities :expt-mod)
  (:use :cl :ol)
  (:export
   :prime-p
   :prime-p%
   :fermat-test-iterations
   :divides-p
   :table
   :factor-search
   :fermat
   :divisible-p))

(in-package :number-theory/primes)

;;; some primality tests

(defgeneric prime-p% (method number)
  (:documentation "The common interface for primality testing
  functions. prime-p will select different implementations based on
  availability and number size. Return value is either (values
  number), if number is prime, or (values nil divisor) or (values nil)
  if it is not."))

(defmethod prime-p% (method (number integer))
  "by default, all numbers are assumed to be non-prime."
  nil)

(defmethod prime-p% ((method (eql 'table)) (number integer))
  "check for a few common small primes, up to twenty"
  (case number
    ((2 3 5 7 11 13 17 19 23) number)
    (t nil)))

(defun divides-p (d n)
  "determine whether the integer d divides the integer n."
  (declare (inline))
  (zerop (mod n d)))

(defun divisible-p (n)
  "search for a true divisor (not 1 or the number itself) of the
  number. If none exists, return nil,otherwise the divisor."
  (do ((i 2 (1+ i))
       (s (floor (sqrt n))))
      ((> i s) nil)
    (when (divides-p i n)
      (return i))))

(defmethod prime-p%  ((method (eql 'factor-search)) (n integer))
  "check for all possible factors. If this is not a prime, there must
be a factor lower than (sqrt n)."
  (declare (inline divisible-p))
  (aif (divisible-p n)
       (values nil it)
       n))

(defparameter fermat-test-iterations 10)

(defmethod prime-p%  ((method (eql 'fermat)) (n integer))
  "perform a fermat test to determine with decent probability, whether
n is a prime or not."
  (let ((k   fermat-test-iterations)
        (n-1 (- n 1))
        (n-2 (- n 2)))
    (dotimes (j k n)
      (let* ((r (+ 2 (random n-2)))
             (p (expt-mod r n-1 n)))
        (unless (= 1 p)
          (return nil))))))

(defun prime-p (number)
  "Test whether the given integer is a prime number."
  (cond ((minusp number) (prime-p (- number))) ; deal with negatives first
        ((<= number 25)
         (prime-p% 'table number))
        ((<= number 10000)
         (prime-p% 'factor-search number))
        (t
         (prime-p% 'fermat number))))


