(defpackage :infinite-sequence
  (:nicknames :infseq)
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

(defmacro with-iseq (&body body)
  `(with-slots (start data+) iseq
     (let ((i (- n start)))
       ,@body)))

(defun ensure-array-size (array size)
  (if (< (length array) size)
      (adjust-array array (+ size 10) :initial-element +uncalculated+)
      array))

(defun sref (iseq n)
  (with-iseq 
   (ensure-array-size data+ i)
   (let ((value (aref data+ i)))
     (if (eq value +uncalculated+)
         (compute-value iseq n)
         value))))

(defun compute-value (iseq n)
  (with-iseq 
   (setf (aref data+ i)
         (funcall (generating-function iseq) n))))

(defun set-sref (iseq n value)
  (with-iseq
    (ensure-array-size (data+ iseq) i)
    (setf (aref data+ i)
          value)))

(defsetf sref (iseq n) (value)
  `(set-sref ,iseq ,n ,value))

;;; TODO do we actually want mutability??
