(defpackage :generic-math
  (:shadow :+ :- :* :/ :expt)
  (:use :cl :ol)
  (:export
   :argument
   :+ :generic-+
   :- :generic--
   :* :generic-*
   :/ :generic-/
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

(define-generic-binary-operation - :none (generic-- 0 argument))
(defmethod generic-- ((a number) (b number))
  (cl:- a b))

(define-generic-binary-operation * 1)
(defmethod generic-* ((a number) (b number))
  (cl:* a b))

(define-generic-binary-operation / :none (generic-- 1 argument))
(defmethod generic-/ ((a number) (b number))
  (cl:/ a b))

(defgeneric expt (base power))
(defmethod expt ((base number) (power number))
  (cl:expt base power))
