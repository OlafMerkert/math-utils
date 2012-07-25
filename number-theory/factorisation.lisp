(defpackage :number-theory/factorisation
  (:nicknames :nt-f)
  (:use :cl :ol )
  (:export
   :factorise))

(in-package :number-theory/factorisation)

(defun factorise% (n)
  "Find all the prime factors of the given integer and return them in
a list, sorted by size. Multiple factors appear multiple times."
  (do ((i 2)
       (m n)
       (s (floor (sqrt n)))
       (divs nil))
      ((or (= m 1) (> i s))
       (if (= m 1) (nreverse divs) (nreverse (cons m divs))))
    (multiple-value-bind (mm rr) (floor m i)
      (if (zerop rr)
          (progn
            (push i divs)
            (setf m mm))
          (incf i)))))

(defun factorise (n &optional (compress t))
  "As factorise%, but if compress is t, compress the list of
  factors."
  (ecase compress
    ((nil) #1=(factorise% n))
    ((t)  (compress #1#))
    ((:singletons) (compress #1# :singletons t))))
