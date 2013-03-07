(defpackage :elementary-combinatorics
  (:nicknames :combi)
  (:use :cl :ol :iterate )
  (:export
   #:binomial))

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
