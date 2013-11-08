(defpackage :factorisation/datastructures
  (:nicknames :fac-ds)
  (:use :cl :ol :iterate )
  (:export
   #:factor
   #:copy-factor-list
   #:make-factor
   #:factor-base
   #:factor-exponent
   #:map-on-exponents
   #:map-on-factors
   #:merge-factorisations%
   #:merge-factorisations))

(in-package :factorisation/datastructures)

;; every factor consists of the base, and of an integer indicating the multiplicity.
(defstruct factor  base (exponent 1))

(defun copy-factor-list (list)
  "Create a copy of a `list' of `factors'."
  (mapcar (lambda (factor) (make-factor :base (factor-base factor)
                                   :exponent (factor-exponent factor)))
          list))

(defun map-on-exponents (mult-function factorisation)
  "Map over a `factorisation' (i.e. factor list) by applying
`mult-function' on the exponents of the factors."
  (mapcar (lambda (factor)
            (make-factor :base (factor-base factor)
                         :exponent (funcall mult-function (factor-exponent factor))))
          factorisation))

(defun map-on-factors (factor-function factorisation &optional (multiplicity 1) disjoint)
  "Here `factor-function' should take a factor as argument, producing
a new factor list. The multiplicities of the result are then adjusted
so the original multiplicity as well as the global `multiplicity'. The
results are then merged together. If it is known that the results of
`factor-function' on the different factors will all produce different
factors, `disjoint' speeds up the operation.

The main use of this function is to refine factorisations."
  (flet ((on-factor (factor)
           (map-on-exponents
            (lambda (mult)
              (cl:* mult multiplicity (factor-exponent factor)))
            (funcall factor-function (factor-base factor)))))
    (if disjoint
        (mapcan #'on-factor factorisation)
        (reduce #'merge-factorisations% factorisation :key #'on-factor))))




(defun merge-factorisations% (list1 list2)
  "Take two lists of `factors', and merge them together, accounting
for factors appearing in both lists by adding multiplicities. This is
the destructive version."
  (mapc (lambda (factor)
          (aif (find factor list1 :key #'factor-base :test #'gm:=)
               (incf (factor-exponent it) (factor-exponent factor))
               (push factor list1)))
        list2)
  list1)

(defun merge-factorisations (list1 list2)
  "Take two lists of `factors', and merge them together, accounting
for factors appearing in both lists by adding multiplicities. This is
the non-destructive version"
  (merge-factorisations% (copy-factor-list list1) list2))

(defun merge-duplicate-factors (list)
  "Merge together any duplicate factors occuring in a factorisation."
  (merge-factorisations% nil list))
