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
   #:print-polynomial
   #:print-additional-terms
   #:print-power-series))

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

(defun print-polynomial (printer polynomial)
  (if (zero-p polynomial)
      (print-number printer 0)
      (iter (for i from 0 to (degree polynomial))
            (for coefficient = (nth-coefficient% polynomial i))
            (for exponent    = (- (degree polynomial) i))
            (for zero-p      = (zero-p coefficient))
            (for minus-p     = (minus-p coefficient))
            (cond ((or (zerop i) zero-p))
                  (minus-p
                   (print-operator printer '-))
                  (t
                   (print-operator printer '+)))
            (unless zero-p 
              (print-monomial printer (if (and (< 0 i) minus-p)
                                          (gm:- coefficient)
                                          coefficient)
                              exponent)))))

;;; output of power series
(defparameter print-additional-terms 5)

(defun print-power-series (printer series)
  (with-printer (printer)
    (if (zero-p series)
        (print-number 0)
        (progn
          (iter (for i from 0 to (+ print-additional-terms
                                    (max 0 (degree series))))
                (for coefficient = (nth-coefficient% series i))
                (for exponent    = (- (degree series) i))
                (for zero-p      = (zero-p coefficient))
                (for minus-p     = (minus-p coefficient))
                (cond ((or (zerop i) zero-p))
                      (minus-p
                       (print-operator '-))
                      (t
                       (print-operator '+)))
                (unless zero-p
                  (print-monomial printer (if (and (< 0 i) minus-p)
                                              (gm:- coefficient)
                                              coefficient)
                                  exponent)))
          ;; now add the ellipsis
          (print-operator '+)
          (print-ellipsis)))))

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
  (print-polynomial (make-instance 'repl-printer :stream stream) polynomial)  
  (princ #\] stream))

(defmethod print-object ((series power-series) stream)
  (princ #\[ stream)
  (print-power-series (make-instance 'repl-printer :stream stream) series)
  (princ #\] stream))

(defmethod print-object ((series constant-series) stream)
  (let ((print-additional-terms 0))
    (princ #\[ stream)
    (print-power-series (make-instance 'repl-printer :stream stream) series)
    (princ #\] stream)))

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
  (print-polynomial (make-instance 'tex-printer :stream stream) polynomial))

(defmethod print-object/tex ((series power-series) stream)
  (print-power-series (make-instance 'tex-printer :stream stream) series))

(defmethod print-object/tex ((series constant-series) stream)
  (let ((print-additional-terms 0))
    (print-power-series (make-instance 'tex-printer :stream stream) series)))

;; TODO very few coefficients in power-series with low degree
;; TODO use the variable name of the polynomial
