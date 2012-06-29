(defpackage :generic-math
  (:nicknames :gm)
  (:shadow :+ :- :* :/ :=
           :expt :sqrt)
  (:use :cl :ol)
  (:export
   :argument
   :+ :generic-+
   :- :generic--
   :* :generic-*
   :/ :generic-/
   := :generic-=
   :zero
   :one
   :simplify
   :expt
   :sqrt
   :->
   :define-generic-binary-operation
   :simplified-p
   :zero-p
   :one-p
   :summing
   :create-binary->-wrappers))

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
     (defun ,name (&rest ,g!arguments)
      (case (length ,g!arguments)
        ((0) ,(if (eq unit :none)
                  `(error "Invalid number of arguments: 0")
                  unit))
        ((1) ,(if single-argument
                  `(let ((argument (first ,g!arguments)))
                     ,single-argument)
                  `(first ,g!arguments)))
        ((2) (apply #',(symb 'generic- name) ,g!arguments))
        (t   (reduce #',(symb 'generic- name) ,g!arguments
                     :from-end ,reduce-right))))))

(define-generic-binary-operation + 0)
(defmethod generic-+ ((a number) (b number))
  (cl:+ a b))

(defgeneric zero (number))
(defmethod zero ((number number))
  0)

(define-generic-binary-operation - :none (generic-- (zero argument) argument))
(defmethod generic-- ((a number) (b number))
  (cl:- a b))

(define-generic-binary-operation * 1)
(defmethod generic-* ((a number) (b number))
  (cl:* a b))

(defgeneric one (number))
(defmethod one ((number number))
  1)

(define-generic-binary-operation / :none (generic-/ (one argument) argument))
(defmethod generic-/ ((a number) (b number))
  (cl:/ a b))

(defgeneric expt (base power))
(defmethod expt ((base number) (power number))
  (cl:expt base power))

(defgeneric simplify (number &key)
  (:documentation "Get the number into a unique, canonical
  representation, such that equality comparison is more efficient.
  Implementations may modify the given NUMBER, but it is expected that
  the simplified version is returned (as the first value)."))

(defgeneric simplified-p (number))

(defmethod simplify (number &key)
  number) ; by default no simplification is done.

(defgeneric generic-= (a b)
  (:documentation "Use this function to implement generic equality.
  But never call this, call instead GM:=, which first simplifies
  everything!"))

(defun = (&rest arguments)
   (case (length arguments)
     ((0) (error "invalid number of arguments: 0"))
     ((1) t)
     ((2)
      (apply #'generic-= (mapcar #'simplify arguments)))
     (t (let ((simple-arguments (mapcar #'simplify arguments)))
          (every #'generic-= simple-arguments (rest simple-arguments))))))

(defmethod generic-= ((a number) (b number))
  (cl:= a b))

(defgeneric sqrt (number)
  (:documentation
   "Find a square root of NUMBER and try to preserve the type of
   number if possible.  Return two values, with the first being the
   result, the second indicating whether type preservation was
   successfull."))

(defmethod sqrt ((number number))
  (values (cl:sqrt number) t))

(defgeneric -> (target-type number &key)
  (:documentation "Transform a NUMBER, if possible to target type,
  which is either a symbol designating a type, or another object."))

(defgeneric zero-p (number)
  (:documentation "Test whether the given number is zero."))

(defmethod zero-p (number)
  (= (zero number) number))

(defmethod zero-p ((number number))
  (cl:zerop number))

(defgeneric one-p (number)
  (:documentation "Test whether the given number is one."))

(defmethod one-p (number)
  (= (one number) number))

(defmacro! summing ((var o!start o!stop &optional below) expr)
  `(let ((,g!sum 0))
     (do ((,var ,g!start (+ 1 ,var)))
         ((,(if below '>= '>)
            ,var ,g!stop)
          ,g!sum)
       (setf ,g!sum
             (+ ,g!sum ,expr)))))

(defmacro create-binary->-wrappers (to from params sides &body generic-functions)
  `(progn
     ,@(when (member :left sides)
        (mapcar
         #`(defmethod ,a1 ((,from ,from) (,to ,to))
             (,a1 (-> ',to ,from ,@params)
                  ,to))
         generic-functions))
     ,@(when (member :right sides)
        (mapcar
         #`(defmethod ,a1 ((,to ,to) (,from ,from))
             (,a1 (-> ',to ,from ,@params)
                  ,to))
         generic-functions))))
