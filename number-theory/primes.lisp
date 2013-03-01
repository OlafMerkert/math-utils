(defpackage :number-theory/primes
  (:nicknames :nt-p)
  (:import-from :cl-utilities :expt-mod)
  (:use :cl :ol
        :iterate
        :number-theory)
  (:export
   :prime-p
   :fermat-test-iterations
   :divides-p
   :divisible-p
   :erastothenes-sieve
   :ord-p))

(in-package :number-theory/primes)

;;; some primality tests
(defmethod prime-p/table (number)
  "check for a few common small primes, up to ten."
  (case number
    ((2 3 5 7) number)
    (t nil)))

(defun divides-p (d n)
  "determine whether the integer d divides the integer n."
  (declare (inline))
  (zerop (mod n d)))

(defun ord-p/integer (p n)
  "determine the largest power P^K dividing the integer N."
  (do ((m n)
       (k 0))
      (nil)
    (multiple-value-bind (q r) (floor m p)
      (cond ((zerop r)
             (incf k)
             (setf m q))
            (t (return (values k m)))))))

(defun ord-p (p n)
  "determine the largest power P^K dividing the rational N."
  (if (integerp n)
      (ord-p/integer p n)
      (multiple-value-bind (k-n f-n)
          (ord-p/integer p (numerator n))
        (multiple-value-bind (k-d f-d)
            (ord-p/integer p (denominator n))
          (values (- k-n k-d)
                  (/ f-n f-d))))))

(defun divisible-p (n)
  "search for a true divisor (not 1 or the number itself) of the
  number. If none exists, return nil,otherwise the divisor."
  (do ((i 2 (1+ i))
       (s (floor (sqrt n))))
      ((> i s) nil)
    (when (divides-p i n)
      (return i))))

(defun prime-p/factor-search  (n)
  "check for all possible factors. If this is not a prime, there must
be a factor lower than (sqrt n)."
  (declare (inline divisible-p))
  (aif (divisible-p n)
       (values nil it)
       n))

(defparameter fermat-test-iterations 10)

(defun prime-p/fermat  (n)
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

(declaim (inline prime-p/table prime-p/factor-search prime-p/fermat))

(defun prime-p (number)
  "Test whether the given integer is a prime number. prime-p will
  select different implementations based on availability and number
  size. Return value is either (values number), if number is prime,
  or (values nil divisor) or (values nil) if it is not."
  (cond ((minusp number) (prime-p (- number))) ; deal with negatives first
        ((< number 11)
         (prime-p/table number))
        ((< number 10000)
         (prime-p/factor-search number))
        (t
         (prime-p/fermat number))))

(defun erastothenes-sieve (s)
  "Sieb des Erastothenes. Berechne einen Vektor aller Primzahlen <= S."
  (iter (with candidates = (make-array (+ s 1)
                                       :element-type 'boolean
                                       :initial-element t) )
        (for i initially 2 then (position t candidates :start (+ i 1)))
        (while i)
        (iter (for j from (* 2 i) by i to s)
              (setf (aref candidates j) nil))
        (collect i result-type vector)))
