(defpackage :polynomials
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :generic-math :summing)
  (:use :cl :ol :generic-math
        :iterate)
  (:export
   :degree
   :nth-coefficient%
   :nth-coefficient
   :polynomial
   :coefficients
   :make-polynomial
   :constant-coefficient
   :format-monomial/tex
   :format-monomial
   :spacer
   :monomial-form
   :leading-coefficient))

(in-package :polynomials)

(defclass polynomial ()
  ((coefficients :initform (vector 0)
                 :initarg :coefficients
                 :reader coefficients)
   #|(var :initform 'x
   :accessor var)|#)
  (:documentation "Model a polynomial in VAR, with the leading
  coefficient the first entry of COEFFICIENTS."))

;; TODO unify polynomial interface with power series interface
(defmethod degree ((polynomial polynomial))
  (1- (length (coefficients polynomial))))

(defmethod leading-coefficient ((polynomial polynomial))
  (if (simplified-p polynomial)
      (nth-coefficient% polynomial 0)
      (error "Trying to take leading-coefficient of non-simplified polynomial.")))

(defmethod nth-coefficient% ((polynomial polynomial) n)
  (aref (coefficients polynomial) n))

(defmethod nth-coefficient ((polynomial polynomial) n)
  (let ((d (degree polynomial)))
    (nth-coefficient% polynomial (- d n))))

(defun constant-coefficient (polynomial)
  "This is just an abbreviation for (nth-coefficient p 0)"
  (nth-coefficient polynomial 0))

(defmethod simplified-p ((polynomial polynomial))
  (or (zerop (degree polynomial))
      (not (zero-p
            (nth-coefficient% polynomial 0)))))

(defun make-polynomial (lk &rest coefficients)
  (make-instance 'polynomial :coefficients (list->array (list* lk coefficients))))

(defmethod zero ((number polynomial))
  (make-polynomial 0))

(defmethod zero ((number (eql 'polynomial)))
  (make-polynomial 0))

(defmethod one ((number polynomial))
  (make-polynomial 1))

(defmethod one ((number (eql 'polynomial)))
  (make-polynomial 1))

(defmethod simplify ((polynomial polynomial) &key)
  "Remove all leading zeros from the coefficients.  If all
  coefficients are zero, keep the last zero."
  (with-slots (coefficients) polynomial
    (let* ((deg (- (length coefficients) 1))
           (nz (or (position-if-not #'zero-p coefficients
                                    :end deg)
                   deg)))
      (setf coefficients
            (subseq coefficients nz))
      (values polynomial nz))))

;;; arithmetic of polynomials
(defmethod generic-* ((poly-a polynomial) (poly-b polynomial))
  "Multiply two polynomials."
  (let ((array-a (coefficients poly-a))
        (array-b (coefficients poly-b))
        (deg-a   (degree poly-a))
        (deg-b   (degree poly-b)))
    (make-instance 'polynomial
                   :coefficients
                   (make-nlazy-array
                       (:index-var n
                                   :default-value 0
                                   :finite (+ deg-a deg-b 1))
                     (summing (i (max 0 (- n deg-b))
                                 (min n deg-a))
                              (gm:* (aref array-a i)
                                    (aref array-b (- n i))))))))

(defmethod generic-* ((poly-b polynomial) (int integer))
  (generic-* int poly-b))

(defmethod generic-* ((int integer) (poly-b polynomial))
  (make-instance 'polynomial
                 :coefficients
                 (map 'vector (lambda (x) (gm:* int x)) (coefficients poly-b))))

(defmethod generic-+ ((poly-a polynomial) (poly-b polynomial))
  "Add two polynomials together.  Implicitly simplify."
  (if (> (degree poly-a) (degree poly-b))
      (generic-+ poly-b poly-a)
      ;; now poly-b has the higher degree
      (let ((coeff-a (coefficients poly-a))
            (coeff-b (coefficients poly-b))
            (d (- (degree poly-b) (degree poly-a))))
        (simplify
         (make-instance 'polynomial
                        :coefficients
                        (make-nlazy-array (:index-var n :default-value 0
                                                      :finite (+ (degree poly-b) 1))
                          (if (< n d)
                              (aref coeff-b n)
                              (gm:+ (aref coeff-b n)
                                    (aref coeff-a (- n d))))))))))

(defmethod generic-- ((poly-a polynomial) (poly-b polynomial))
  (generic-+ poly-a (generic-* -1 poly-b)))

(defmethod generic-/ ((poly-numer polynomial) (poly-denom polynomial))
  "This actually implements polynomial division with a remainder.
Keep this in mind when using."
  (unless (simplified-p poly-denom)
    (error "Cannot divide by the POLY-DENOM ~A unless it is
    normalised, i.e. the first coefficient is non-zero." poly-denom))
  (when (zero-p poly-denom)
    (error "Cannot divide by ZERO."))
  (let ((a0 (nth-coefficient% poly-denom 0))
        (an (coefficients poly-denom))
        (cn (coefficients poly-numer))
        (deg-a (degree poly-denom)) 
        (deg (- (degree poly-numer)
                (degree poly-denom))))
    (if (minusp deg)
        (values (zero 'polynomial) poly-numer)
        (let ((result
               (make-instance 'polynomial
                              :coefficients
                              (make-nlazy-array (:start ((gm:/ (aref cn 0) a0))
                                                        :index-var n
                                                        :finite (+ deg 1)
                                                        :default-value 0)
                                (gm:/ (gm:- (aref cn n)
                                            (summing (i 1 (min n deg-a))
                                                     (gm:* (aref an i)
                                                           (aref this (- n i)))))
                                      a0)))))
          (values result ;(gm:- poly-numer (gm:* poly-denom result))
                  )))))

;;; comparison
(defmethod generic-= ((poly-a polynomial) (poly-b polynomial))
  "Compare two polynomials for equality, assuming both are already
  simplified."
  (let ((d (degree poly-a)))
    (and (= d (degree poly-b))
         (iter (for a in-vector (coefficients poly-a))
               (for b in-vector (coefficients poly-b) )
               (always (gm:= a b))))))

;;; output of polynomials


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
                                      (- coefficient)
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



;; TODO make the polynomial rendering code more general (i.e. we want
;; to reuse it in the math-interactor
