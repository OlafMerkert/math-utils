(defpackage :polynomial-series-printing
  (:nicknames :pspr)
  (:shadowing-import-from :cl :+ :- :* :/ := :expt :sqrt)
  (:shadowing-import-from :generic-math :summing)
  (:use :cl :ol :iterate
        :generic-math
        :polynomials
        :power-series)
  (:export
   #:print-superscript
   #:format-monomial
   #:format-polynomial
   #:print-additional-terms
   #:format-power-series
   #:string-printer
   #:tex-printer
   #:printer
   #:implement-printer-method
   #:define-printer-method
   #:print-sum+ellipsis
   #:*current-printer*
   #:format-monomial/all
   #:format-power-series/all
   #:format-polynomial/all))

(in-package :polynomial-series-printing)

(defparameter print-additional-terms 5)

;;; four entries in the list: coefficient, exponent, whether coeff is
;;; one, and whether it had sign swapped.
(defun all-coeffs (coefficients degree)
  (iter (for i downfrom degree)
        (for c in-vector coefficients)
        (collect (list c i nil nil))))

(defun clean-coeffs (coefficients degree)
  (iter (for i downfrom degree)
        (for c in-vector coefficients)
        (for m = (and (not (first-iteration-p))
                      (minus-p c)))
        (for cc = (if m (gm:- c) c))
        (unless (zero-p c)
          (collect (list cc i (one-p cc) m)))))

;;; TODO some inconsistence with print-superscript, which does not get
;;; called with results of print-math-object
(defun format-monomial (var coeff deg one neg)
  (declare (ignorable neg))
  (cond ((and one (zerop deg))
         (print-math-object 1))
        ((zerop deg)
         (print-math-object coeff))
        ((and one (= deg 1))
         (print-math-object var))
        ((= deg 1)
         (print-product
          (print-math-object coeff)
          (print-math-object var)))
        (one
         (print-superscript var deg))
        (t
         (print-product
          (print-math-object coeff)
          (print-superscript var deg)))))

(defun format-monomial/all (var coeff deg)
  (print-product
   (print-math-object coeff)
   (print-superscript var deg)))

(defun format-polynomial (polynomial)
  (apply #'print-sum
         (mapcar (lambda (x) (list (apply #'format-monomial
                                     (var polynomial)
                                     x)
                              (if (fourth x)
                                  '-
                                  '+)))
                 (clean-coeffs (coefficients polynomial) (degree polynomial)))))

(defun format-polynomial/all (polynomial)
  (apply #'print-sum
         (mapcar (lambda (x) (list (apply #'format-monomial/all
                                     (var polynomial)
                                     x)
                              (if (fourth x)
                                  '-
                                  '+)))
                 (all-coeffs (coefficients polynomial) (degree polynomial)))))

(defun format-power-series (power-series)
  (apply #'print-sum+ellipsis
         (mapcar (lambda (x) (list (apply #'format-monomial
                                     'X
                                     x)
                              (if (fourth x)
                                  '-
                                  '+)))
                 (clean-coeffs (lazy-array-take (coefficients power-series)
                                                (+ (degree power-series) print-additional-terms)
                                                nil)
                               (degree power-series)))))

(defun format-power-series/all (power-series)
  (apply #'print-sum+ellipsis
         (mapcar (lambda (x) (list (apply #'format-monomial/all
                                     'X
                                     x)
                              (if (fourth x)
                                  '-
                                  '+)))
                 (all-coeffs (lazy-array-take (coefficients power-series)
                                              (+ (degree power-series) print-additional-terms)
                                              nil)
                             (degree power-series)))))

(defparameter *current-printer* nil)

(defmacro define-printer-method (name args)
  (let ((impl-name (symb name '-implementation))
        (args% (args->names args)))
    `(progn
       (defgeneric ,impl-name (printer ,@args%))
       (defmethod ,impl-name ((printer (eql nil)) ,@args%))
       (defun ,name ,args
         (,impl-name *current-printer* ,@args%)))))

(defmacro implement-printer-method (name printer-type args &body body)
  ;; PRINTER is anaphoric
  `(defmethod ,(symb+ :pspr name '-implementation) ((printer (eql ',printer-type)) ,@args)
     ,@body))

(define-printer-method print-math-object  (object))
(define-printer-method print-sum          (&rest summands-with-sign))
(define-printer-method print-sum+ellipsis (&rest summands-with-sign))
(define-printer-method print-product      (&rest factors))
(define-printer-method print-superscript  (base exponent))

;;; implementation of printer interface for the repl
(implement-printer-method print-math-object string-printer (object)
  (format nil "~A" object))

(implement-printer-method print-sum string-printer (summands-with-sign)
  (with-output-to-string (stream)
    (iter (for sws in summands-with-sign)
          (for sum = (first sws))
          (for sign = (second sws))
          (if (first-iteration-p)
              (unless (eql '+ sign)
                (princ sign stream))
              (format stream " ~A " sign))
          (princ sum stream))))

(implement-printer-method print-sum+ellipsis string-printer (summands-with-sign)
  (concatenate 'string (print-sum-implementation printer summands-with-sign)
               " + ..."))

(implement-printer-method print-product string-printer (factors)
  (format nil "~{~A~^ ~}" factors))

(implement-printer-method print-superscript string-printer (base exponent)
  (format nil "~A^~A" base exponent))

(defmethod print-object ((polynomial polynomial) stream)
  (let ((*current-printer* 'string-printer))
    (princ #\[ stream)
    (princ (format-polynomial polynomial) stream)
    (princ #\] stream)))

(defmethod print-object ((series power-series) stream)
    (let ((*current-printer* 'string-printer))
      (princ #\[ stream)
      (princ (format-power-series series) stream)
      (princ #\] stream)))

(defmethod print-object ((series constant-series) stream)
  (let ((print-additional-terms 1)
        (*current-printer* 'string-printer))
    (princ #\[ stream)
    (princ (format-power-series series) stream)
    (princ #\] stream)))

;; implementation for print-object/tex
(implement-printer-method print-math-object tex-printer (object)
  (with-output-to-string (stream)
    (print-object/tex object stream)))

;; this one is identical to the string printer
(implement-printer-method print-sum tex-printer (summands-with-sign)
  (with-output-to-string (stream)
    (iter (for sws in summands-with-sign)
          (for sum = (first sws))
          (for sign = (second sws))
          (if (first-iteration-p)
              (unless (eql '+ sign)
                (princ sign stream))
              (format stream " ~A " sign))
          (princ sum stream))))

(implement-printer-method print-sum+ellipsis tex-printer (summands-with-sign)
  (concatenate 'string (print-sum-implementation printer summands-with-sign)
               " + \\dots"))

(implement-printer-method print-product tex-printer (factors)
  (format nil "~{~A~^ \\, ~}" factors))

(implement-printer-method print-superscript tex-printer (base exponent)
  (format nil "{~A}^{~A}" base exponent))

(defmethod print-object/tex ((polynomial polynomial) stream)
  (let ((*current-printer* 'tex-printer))
    (princ #\[ stream)
    (princ (format-polynomial polynomial) stream)
    (princ #\] stream)))

(defmethod print-object/tex ((series power-series) stream)
    (let ((*current-printer* 'tex-printer))
      (princ #\[ stream)
      (format-power-series series)
      (princ #\] stream)))

(defmethod print-object/tex ((series constant-series) stream)
  (let ((print-additional-terms 1)
        (*current-printer* 'tex-printer))
    (princ #\[ stream)
    (format-power-series series)
    (princ #\] stream)))

;; TODO very few coefficients in power-series with low degree
;; TODO use the variable name of the polynomial

;;; TODO how about abstracting math formatting a bit more, and
;;; providing a general library that takes care of that?
