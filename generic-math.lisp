(defpackage :generic-math
  (:nicknames :gm)
  (:shadow :+ :- :* :/ :=
           :expt :^ :sqrt
           :expt-mod)
  (:use :cl :ol :iterate)
  (:export
   #:argument
   #:+ #:generic-+
   #:- #:generic--
   #:* #:generic-*
   #:/ #:generic-/
   #:div
   #:= #:generic-=
   #:zero
   #:one
   #:simplify
   #:expt
   #:sqrt
   #:->
   #:define-generic-binary-operation
   #:simplified-p
   #:zero-p
   #:one-p
   #:create-binary->-wrappers
   #:print-object/tex
   #:print-object/helper
   #:*tex-output-mode*
   #:minus-p
   #:define->-method
   #:declare-commutative
   #:declare-fold-operation
   #:default-simple-type-conversion
   #:define->-method/identity
   #:define->-method/custom
   #:generic-math-object
   #:^
   #:expt-mod
   #:+gm-shadow-imports+
   #:+frac-shadow-imports+
   #:divr
   #:gm-summing
   #:+cl-shadow-imports+
   #:+ol-shadow-imports+))

(in-package :generic-math)

;; some useful constants for shadowing imports
(defparameter +gm-shadow-imports+
  '(:shadowing-import-from :generic-math :+ :- :* :/ := :expt :sqrt :^ :_))

(defparameter +cl-shadow-imports+
  '(:shadowing-import-from :common-lisp :+ :- :* :/ := :expt :sqrt))

(defparameter +ol-shadow-imports+
  '(:shadowing-import-from :ol-utils :^ :_))

(defparameter +frac-shadow-imports+
  '(:shadowing-import-from :fractions :numerator :denominator))


(defalias ^ expt (base exponent))

(defclass generic-math-object ()
  ()
  (:documentation "an abstract root class for all mathematical
  objects (except for primtive types like numbers.)"))

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

;; special behaviour at units
(defmethod generic-+ (a (b (eql 0)))
  a)

(defmethod generic-+ ((b (eql 0)) a)
  a)

(defun zero (number)
  (declare (ignore number))
  0)

(define-generic-binary-operation - :none (generic-- (zero argument) argument))
(defmethod generic-- ((a number) (b number))
  (cl:- a b))

(defmethod generic-- (a (b (eql 0)))
  a)

(define-generic-binary-operation * 1)
(defmethod generic-* ((a number) (b number))
  (cl:* a b))

(defmethod generic-* (a (b (eql 0)))
  0)

(defmethod generic-* ((b (eql 0)) a)
  0)

(defmethod generic-* (a (b (eql 1)))
  a)

(defmethod generic-* ((b (eql 1)) a)
  a)

(defun one (number)
  (declare (ignore number))
  1)

(define-generic-binary-operation / :none (generic-/ (one argument) argument))
(defmethod generic-/ ((a number) (b number))
  (cl:/ a b))

(defmethod generic-/ (a (b (eql 1)))
  a)

(defgeneric div (number modulus)
  (:documentation "The division function for Euclidean rings."))

(defun divr (number modulus)
  "The remainder of the generic division."
  (nth-value 1 (div number modulus)))

(defmethod div ((number number) (modulus number))
  (floor number modulus))

(defmethod div ((number (eql 0)) modulus)
  (when (zero-p modulus)
    (error "Division by 0!"))
  (values 0 0))

(defgeneric expt (base power))
(defmethod expt ((base number) (power number))
  (cl:expt base power))

(defmethod gm:expt (base  (power integer))
  "Generic exponentation algorithm using SQUARE-MULTIPLY.  Feel free
to override this if a better algorithm is available."
  (cond ((zerop power) (one base))
        ((= power 1) base)
        ((= power 2) (generic-* base base))
        ((minusp power) (gm:expt (generic-/ (one base) base) (- power)))
        (t (square-multiply base power #'generic-*))))

(defgeneric simplify (number &key)
  (:documentation "Get the number into a unique, canonical
  representation, such that equality comparison is more efficient.
  Implementations may modify the given NUMBER, but it is expected that
  the simplified version is returned (as the first value)."))

(defgeneric simplified-p (number))

(defmethod simplified-p ((number number))
  t)

(defmethod simplify (number &key)
  number) ; by default no simplification is done.

(defgeneric generic-= (a b)
  (:documentation "Use this function to implement generic equality.
  But never call this, call instead GM:=, which first simplifies
  everything!"))

(defmethod generic-= (a b)
  nil)

(defun simplify-if-necessary (x)
  (if (simplified-p x) x
      (simplify x)))

(defun = (&rest arguments)
   (case (length arguments)
     ((0) (error "invalid number of arguments: 0"))
     ((1) t)
     ((2)
      (apply #'generic-= (mapcar #'simplify-if-necessary arguments)))
     (t (let ((simple-arguments (mapcar #'simplify-if-necessary arguments)))
          (every #'generic-= simple-arguments (rest simple-arguments))))))

(defmethod generic-= ((a number) (b number))
  (cl:= a b))

(defmethod generic-= ((a integer) (b integer))
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

(defmethod -> (target-type number &key)
  (error "Undefined math object conversion path."))

(defgeneric zero-p (number)
  (:documentation "Test whether the given number is zero."))

#|(defmethod zero-p (number)
  (= (zero number) number))|#

(defmethod zero-p ((number number))
  (cl:zerop number))

(defgeneric one-p (number)
  (:documentation "Test whether the given number is one."))

#|(defmethod one-p (number)
  (= (one number) number))|#

(defmethod one-p ((number number))
  (cl:= 1 number))

;; sign extraction is useful for nice representation of stuff
(defgeneric minus-p (number)
  (:documentation
   "Return true if the number is negative, of course this only makes
   sense in an ordered field/domain."))

;; by default, nothing is negative
(defmethod minus-p (number)
  nil)

(defmethod minus-p ((number real))
  (minusp number))


;; treat 0 and 1 special for comparison
(defmethod generic-= ((a (eql 0)) b)
  (if (numberp b)
      (zerop b)
      (zero-p b)))

(defmethod generic-= (b (a (eql 0)))
  (if (numberp b)
      (zerop b)
      (zero-p b)))

(defmethod generic-= ((a (eql 1)) b)
  (if (numberp b)
      (cl:= 1 b)
      (one-p b)))

(defmethod generic-= (b (a (eql 1)))
  (if (numberp b)
      (cl:= 1 b)
      (one-p b)))

;; TODO leverage iterate for this sort of stuff. perhaps even use a
;; macro to simplify things even more. (we probably want a multiplying
;; thingy too)
(defmacro! gm-summing ((var o!start o!stop &optional below) expr)
  `(let ((,g!sum 0))
     (do ((,var ,g!start (cl:+ 1 ,var)))
         ((,(if below '>= '>)
            ,var ,g!stop)
          ,g!sum)
       (setf ,g!sum
             (+ ,g!sum ,expr)))))

(defmacro slots-dest-helper (&body body)
  `(let* ((keywords (mapcar (lambda (x) (if (keywordp (first x))
                                      (first x)
                                      (keyw (first x))))
                           slots))
         (parameters (mapcar (lambda (x) (if (keywordp (first x))
                                        (symb (first x))
                                        (first x)))
                             slots))
         (accessors (mapcar (lambda (slot param) (if (second slot)
                                                (second slot)
                                                param))
                            slots parameters))
         (default-values (mapcar #'third slots)))
     (declare (ignorable keywords))
     ,@body))

(defmacro! define->-method ((to from &rest slots) &rest m-i-parameters)
  "Define -> for conversion FROM TO. The result may inherit SLOTS from
TO, where every slot has the form (keyword-or-parameter &optional accessor
default-value). The M-I-PARAMETERS are then the additional arguments
for make-instance."
  (slots-dest-helper
    `(progn
       (defmethod -> ((,g!target-type (eql ',to)) (,from ,from)
                      &key ,@(mapcar #2`(,a1 ,a2) parameters default-values))
         (make-instance ',to ,@(mapcan #2`(,a1 ,a2) keywords parameters)
                        ,@m-i-parameters))
       (defmethod -> ((,to ,to) (,from ,from) &key)
         (make-instance ',to ,@(mapcan #2`(,a1 (,a2 ,to)) keywords accessors)
                        ,@m-i-parameters)))))

(defmacro! define->-method/custom ((to from &rest slots) &body body)
  "Define -> for conversion FROM TO. We bind SLOTS from TO, where
every slot has the form (keyword-or-parameter &optional accessor
default-value). These can be accessed from the BODY."
  (slots-dest-helper
    `(progn
       (defmethod -> ((,g!target-type (eql ',to)) (,from ,from)
                      &key ,@(mapcar #2`(,a1 ,a2) parameters default-values))
         ,@body)
       (defmethod -> ((,g!target-type ,to) (,from ,from) &key)
         (let ,(mapcar #2`(,a1 (,a2 ,g!target-type)) parameters accessors)
           ,@body)))))

(defmacro! define->-method/identity (to-and-from)
  "Define -> which does nothing if target and source have given type
  TO-AND-FROM."
  `(progn
     (defmethod -> ((,g!target-type (eql ',to-and-from)) (,to-and-from ,to-and-from) &key)
       ,to-and-from)
     (defmethod -> ((,g!target-type ,to-and-from) (,to-and-from ,to-and-from) &key)
       ,to-and-from)))

(defmacro create-binary->-wrappers (to from sides &body generic-functions)
  `(progn
     ,@(when (member :left sides)
        (mapcar
         #`(defmethod ,a1 ((,from ,from) (,to ,to))
             (,a1 (-> ,to ,from)
                  ,to))
         generic-functions))
     ,@(when (member :right sides)
        (mapcar
         #`(defmethod ,a1 ((,to ,to) (,from ,from))
             (,a1 ,to
                  (-> ,to ,from)))
         generic-functions))))

(defmacro declare-commutative (to-left to-right &body generic-functions)
  `(progn
     ,@(mapcar #`(defmethod ,a1 ((,to-right ,to-right) (,to-left ,to-left))
                   (,a1 ,to-left ,to-right))
               generic-functions)))

(defmacro declare-fold-operation (to-left to-right &body generic-functions+units)
  "generic-functions+units has items of the format (generic-- generic-+ 0)"
  `(progn
     ,@(mapcar #`(defmethod ,(first a1) ((,to-right ,to-right) (,to-left ,to-left))
                   (,(second a1) (,(first a1) ,(third a1) ,to-left)
                     ,to-right))
               generic-functions+units)))

(defmacro default-simple-type-conversion (simple complex)
  ;; this makes mostly sense when the simple type is really simple,
  ;; like RATIONAL.
  `(progn
     (create-binary->-wrappers ,complex ,simple (:left)
       generic-+
       generic--
       generic-*
       generic-/)

     (declare-commutative ,simple ,complex
       generic-+
       generic-*)

     (declare-fold-operation ,simple ,complex
       (generic-- generic-+ 0)
       (generic-/ generic-* 1))))

(defgeneric print-object/tex (object stream)
  (:documentation "Generate a string representation suitable for
  insertion into TeX documents."))

(defmethod print-object/tex (object stream)
  ;; by default fall back to standard format call
  (format stream "~A" object))

;; normal numbers
(defmethod print-object/tex ((number integer) stream)
  (format stream "~A" number))

(defmethod print-object/tex ((number rational) stream)
  (format stream "~:[~;-~]\\frac{~A}{~A}" (minusp number)
          (abs (numerator number)) (denominator number)))

;; helper functions to share code for tex / non-tex output
(defparameter *tex-output-mode* t)

(defun print-object/helper (obj stream)
  (if *tex-output-mode*
      (print-object/tex obj stream)
      (typecase obj
        ;; TODO add other types if necessary
        (number (princ obj stream))
        (t (print-object obj stream)))))

;;; TODO compiler macros to replace gm:op with cl:op if all arguments
;;; are standard cl types
