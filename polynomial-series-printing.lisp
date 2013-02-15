(defpackage :polynomial-series-printing
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :generic-math :summing)
  (:use :cl :ol :iterate
        :generic-math
        :polynomials
        :power-series)
  (:export
   #:print-number
   #:print-superscript
   #:print-variable
   #:print-spacer
   #:print-operator
   #:print-ellipsis
   #:with-printer
   #:print-monomial
   #:print-polynomial))

(in-package :polynomial-series-printing)

;; first establish a protocol, so we may use the output logic for
;; different frontends (like repl, tex code or the math-interactor
(progn
  (defgeneric print-number      (printer number))
  (defgeneric print-superscript (printer base exponent))
  (defgeneric print-variable    (printer variable))
  (defgeneric print-spacer      (printer))
  (defgeneric print-operator    (printer operator))
  (defgeneric print-ellipsis    (printer)))

(defmacro with-printer ((printer) &body body)
  `(flet ,(mapcar #`(,a1 (&rest args) (apply #',a1 ,printer args))
                  '(print-number
                    print-superscript
                    print-variable
                    print-spacer
                    print-operator
                    print-ellipsis))
     ,@body))

;;; output of polynomials
(defun print-monomial (printer coefficient exponent)
  (with-printer (printer)
    (if (one-p coefficient)
        (case exponent
          ((0) (print-number coefficient))
          ((1) (print-variable 'X))
          (t   (print-superscript 'X exponent)))
        (progn
          (print-number coefficient)
          (case exponent
            ((0))
            ((1) (print-spacer)
             (print-variable 'X))
            (t   (print-spacer)
                 (print-superscript 'X exponent)))))))

(defun print-polynomial (polynomial printer)
  (iter (for i from 0 to (degree polynomial))
        (for coefficient = (nth-coefficient% polynomial i))
        (for exponent    = (- (degree polynomial) i))
        (for zero-p      = (zero-p coefficient))
        (for minus-p     = (minus-p coefficient))
        (cond ((or (zerop i) zero-p))
              (minus-p
               (print-operator stream '-))
              (t
               (print-operator stream '+)))
        (unless zero-p 
          (print-monomial printer (if (and (< 0 i) minus-p)
                                      (gm:- coefficient)
                                      coefficient)
                          exponent))))

;; implementation for print-object
(defclass repl-printer ()
  ((stream :initarg :stream
           :initform *standard-output*)))

(defmethod print-number ((repl-printer repl-printer) number)
  (print-object number (slot-value repl-printer 'stream)))

(defmethod print-number ((repl-printer repl-printer) (number number))
  (princ number (slot-value repl-printer 'stream)))

(defmethod print-superscript ((repl-printer repl-printer) base exponent)
  (format (slot-value repl-printer 'stream) "~A^~A" base exponent))

(defmethod print-variable ((repl-printer repl-printer) variable)
  (format (slot-value repl-printer 'stream) "~A" variable))

(defmethod print-spacer ((repl-printer repl-printer))
  (princ " " (slot-value repl-printer 'stream)))

(defmethod print-operator ((repl-printer repl-printer) operator)
  (format (slot-value repl-printer 'stream) " ~A " operator))

(defmethod print-ellipsis ((repl-printer repl-printer))
  (princ "..." (slot-value repl-printer 'stream)))

(defmethod print-object ((polynomial polynomial) stream)
  (princ #\[ stream)
  (print-polynomial polynomial (make-instance 'repl-printer :stream stream))  
  (princ #\] stream))

;; implementation for print-object/tex
(defclass tex-printer ()
  ((stream :initarg :stream
           :initform *standard-output*)))

(defmethod print-number ((tex-printer tex-printer) number)
  (print-object/tex number (slot-value tex-printer 'stream)))

(defmethod print-superscript ((tex-printer tex-printer) base exponent)
  (format (slot-value tex-printer 'stream) "{~A}^{~A}" base exponent))

(defmethod print-variable ((tex-printer tex-printer) variable)
  (format (slot-value tex-printer 'stream) "~A" variable))

(defmethod print-spacer ((tex-printer tex-printer))
  (princ " \\, " (slot-value tex-printer 'stream)))

(defmethod print-operator ((tex-printer tex-printer) operator)
  (format (slot-value tex-printer 'stream) " ~A " operator))

(defmethod print-ellipsis ((tex-printer tex-printer))
  (princ "\\dots" (slot-value tex-printer 'stream)))


(defmethod print-object/tex ((polynomial polynomial) stream)
  (print-polynomial polynomial
                    (make-instance 'tex-printer :stream stream)))
