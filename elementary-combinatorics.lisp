(defpackage :elementary-combinatorics
  (:nicknames :combi)
  (:use :cl :ol :iterate )
  (:export
   #:binomial
   #:factorial))

(in-package :elementary-combinatorics)

(defun binomial (n k)
  "Calculate the binomial coefficient."
  (cond ((< n k) 0)
        ((< (* 2 k) n)
         (binomial n (- n k)))
        (t 
         (iter (for i from 1 to k)
               (for j downfrom n)
               (multiplying (/ j i))))))

(defun factorial (n)
  "Calculate the factorial."
  (if (minusp n)
      (error "Cannot compute factorial of negative number ~A." n)
      (iter (for i from 1 to n)
            (multiplying i))))
