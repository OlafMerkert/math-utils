(defpackage :generic-math
  (:shadow :+ :- :* :/ :expt :=)
  (:use :cl :ol)
  (:export
   :argument
   :+ :generic-+
   :- :generic--
   :* :generic-*
   :/ :generic-/
   := :generic-=
   :+-unit
   :*-unit
   :simplify
   :expt
   :define-generic-binary-operation))

(in-package :generic-math)

(defmacro! define-generic-binary-operation
    (name unit &optional single-argument reduce-right)
  "Define a funktion NAME which returns UNIT if called with 0
arguments (if UNIT is :NONE, an error message is created); which acts
as identity when called with 1 argument, as long as SINGLE-ARGUMENT is
nil--otherwise return the expression in SINGLE-ARGUMENT, where
ARGUMENT is anaphoric for the single argument; which for more than 2
arguments calls reduce with the binary generic function generic-NAME
that is automatically defined--whereby REDUCE-RIGHT is passed
as :from-end parameter to reduce."
  `(progn
     (defgeneric ,(symb 'generic- name) (a b))
     (defun ,name (&rest ,g!summands)
      (case (length ,g!summands)
        ((0) ,(if (eq unit :none)
                  `(error "Invalid number of arguments: 0")
                  unit))
        ((1) ,(if single-argument
                  `(let ((argument (first ,g!summands)))
                     ,single-argument)
                  `(first ,g!summands)))
        ((2) (apply #',(symb 'generic- name) ,g!summands))
        (t   (reduce #',(symb 'generic- name) ,g!summands
                     :from-end ,reduce-right))))))

(define-generic-binary-operation + 0)
(defmethod generic-+ ((a number) (b number))
  (cl:+ a b))

(defgeneric +-unit (number))
(defmethod +-unit ((number number))
  0)

(define-generic-binary-operation - :none (generic-- (+-unit argument) argument))
(defmethod generic-- ((a number) (b number))
  (cl:- a b))

(define-generic-binary-operation * 1)
(defmethod generic-* ((a number) (b number))
  (cl:* a b))

(defgeneric *-unit (number))
(defmethod *-unit ((number number))
  1)

(define-generic-binary-operation / :none (generic-- (*-unit argument) argument))
(defmethod generic-/ ((a number) (b number))
  (cl:/ a b))

(defgeneric expt (base power))
(defmethod expt ((base number) (power number))
  (cl:expt base power))

(defgeneric simplify (number)
  (:documentation "Get the number into a unique, canonical
  representation, such that equality comparison is more efficient."))

(defmethod simplify (number)) ; by default no simplification is done.

(defgeneric generic-= (a b))

(defun = (&rest summands)
   (case (length summands)
     ((0) (error "invalid number of arguments: 0"))
     ((1) t)
     ((2)
      (simplify (first summands))
      (simplify (second summands))
      (apply #'generic-= summands))
     (t (mapc #'simplify summands)
        (every #'generic-= summands (rest summands)))))

(defmethod generic-= ((a number) (b number))
  (= a b))