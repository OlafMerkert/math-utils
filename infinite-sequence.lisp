(defpackage :infinite-sequence
  (:nicknames :infseq)
  (:shadow #:length)
  (:use :cl :ol :iterate
        :infinite-math)
  (:export))

(in-package :infinite-sequence)

(defclass infinite+-sequence ()
  ((start :initarg :start
          :initform 0
          :reader start)
   (end :initarg :end
        :initform infinity+
        :reader end)
   (data+ :initform (make-array 100 :initial-element +uncalculated+
                                :adjustable t)
          :reader data+)
   (generating-function :initarg :generating-function
                        :initform nil
                        :accessor generating-function))
  (:documentation "An infinite sequence on integers, from an arbitrary
  integer N to +infinity."))

;; (defclass infinite-sequence ()
;;   ()
;;   (:documentation "An infinite sequence on integers, covering all
;;   integers"))

(defmethod finite-sequence-p ((seq infinite+-sequence))
  (numberp (length seq)))

(defgeneric length (sequence))

(defmethod length ((iseq infinite+-sequence))
  (with-slots (start end) iseq
    (gm:- end start)))

(defmacro with-iseq (&body body)
  "Bind slots `start' and `data+'. Moreover, transform the sequence
index `n' to the array index `i'."
  `(with-slots (start data+) iseq
     (let ((i (- n start)))
       ,@body)))

(defun ensure-array-size (array size)
  "For an adjustable `array', make sure that it can hold at least
`size' elements."
  (if (< (cl:length array) size)
      (adjust-array array (+ size 10) :initial-element +uncalculated+)
      array))

(defun sref (iseq n)
  "Access element at position `n' in a sequence. Compute previously
uncalculated values."
  (with-iseq
   (ensure-array-size data+ i)
   (let ((value (aref data+ i)))
     (if (eq value +uncalculated+)
         (compute-value iseq n)
         value))))

;;; TODO provide different update strategies: sequential filling, as
;;; needed and maybe others

;;; TODO make the sequence interface compatible with arrays and lists.

(defun compute-value (iseq n)
  "Fill position `n' in the sequence `iseq' using the
`generating-function'."
  (with-iseq 
   (setf (aref data+ i)
         (funcall (generating-function iseq) n))))

(defun set-sref (iseq n value)
  "Manually set position `n' in the sequence `iseq'."
  (with-iseq
    (ensure-array-size (data+ iseq) i)
    (setf (aref data+ i)
          value)))

(defsetf sref (iseq n) (value)
  `(set-sref ,iseq ,n ,value))

;;; TODO do we actually want mutability??
