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
   #:merge-factorisations
   #:multiply-factors
   #:sort-factors-by
   #:compact-factors))

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
          (aif (find (factor-base factor) list1 :key #'factor-base :test #'gm:=)
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

(defun multiply-factors (factor-list)
  "inverse to the `factorise' function - produce the product of the
factors with their multiplicities."
  (reduce #'gm:generic-*
          factor-list
          :key (lambda (factor) (expt (factor-base factor)
                                 (factor-exponent factor)))))

(defun sort-factors-by (factor-list &key (comparator #'<) (key #'polynomials:degree))
  "Sort a list of factors, by default we sort for ascending degree."
  (sort factor-list comparator :key (compose key #'factor-base)))

(defun compact-factors (factor-list &optional (compress t))
  "Produce a sorted and compressed list of factors, in the style of
  nt:factorise. However, here we only distinguish between t and :singletons for `compress', because we already know all about multiplicities"
  (nt-f::compress-wrap
   (mapcar (lambda (f)
             (if (= 1 (factor-exponent f)) (factor-base f)
                 (cons (factor-base f) (factor-exponent f))))
           (sort-factors-by factor-list))
   compress))
