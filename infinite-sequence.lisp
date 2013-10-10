(defpackage :infinite-sequence
  (:nicknames :infseq)
  (:shadow #:length)
  (:use :cl :ol :iterate
        :infinite-math)
  (:export))

(in-package :infinite-sequence)

;;; general (infinite) sequence API
(defgeneric finite-sequence-p (sequence))
(defgeneric length (sequence))

(defgeneric sref (sequence n))
(defgeneric set-sref (sequence n value))

(defsetf sref (iseq n) (value)
  `(set-sref ,iseq ,n ,value))

;;; singly infinite sequences
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
   ;; for sequential filling, we need to keep track of the minimal
   ;; non-filled entry in the data array. This should refer to actual
   ;; array indices, not sequence indices.
   (minimal-uncalculated :initarg :fill-strategy
                         :initform nil
                         :accessor minimal-uncalculated)
   (generating-function :initarg :generating-function
                        :initform nil
                        :accessor generating-function)
   ;; the `generating-function' is expected to take two arguments,
   ;; namely the `infinite+-sequence' which must be treated immutable,
   ;; and an index `n'. Moreover, for sequential filling, only lower
   ;; indices may be accessed, and for as-needed filling, be careful
   ;; to avoid circular dependencies.
   )
  (:documentation "An infinite sequence on integers, from an arbitrary
  integer N to +infinity."))



(defmethod initialize-instance :after ((iseq infinite+-sequence) &key)
  (with-slots ((fs minimal-uncalculated) data+) iseq
    (cond ((or (null fs) (eq fs :as-needed))
           ;; by default, we don't track minimal filled and just fill
           ;; as needed.
           )
          ((integerp fs)
           ;; we are given an explicit index where only indices below
           ;; have been filled, and we use sequential filling
           )
          ((eq fs :sequential)
           ;; we determine the first uncalculated index automatically
           ;; and use sequential filling.
           (setf fs (position +uncalculated+ data+))))))

(declaim (inline sequential-filling-p))

(defun sequential-filling-p (iseq)
  (integerp (minimal-uncalculated iseq)))

;; (defclass infinite-sequence ()
;;   ()
;;   (:documentation "An infinite sequence on integers, covering all
;;   integers"))

(defmethod length ((iseq infinite+-sequence))
  (with-slots (start end) iseq
    (gm:- end start)))

(defmethod finite-sequence-p ((seq infinite+-sequence))
  (numberp (length seq)))

(defmacro with-iseq (&body body)
  "Bind slots `start' and `data+'. Moreover, transform the sequence
index `n' to the array index `i'."
  `(with-slots (start data+) iseq
     (let ((i (- n start)))
       ,@body)))

(defparameter array-size-step 10
  "By how much we extend array-size when an array is too small.")

(defun ensure-array-size (array size)
  "For an adjustable `array', make sure that it can hold at least
`size' elements."
  (if (< (cl:length array) size)
      (adjust-array array (+ size array-size-step) :initial-element +uncalculated+)
      array))

(declaim (inline in-range))

(defun in-range (iseq n)
  "Make sure the given index is valid for the sequence."
  (and (integerp n)
       (i<= (start iseq) n)
       (i< n (end iseq))))

(define-condition index-out-of-range ()
  ((start :initarg :start :reader start)
   (index :initarg :index :reader index)
   (end   :initarg :end   :reader end)))

(defmacro when-in-range (&body body)
  `(if (in-range iseq n)
       (progn ,@body)
       (error 'index-out-of-range :index n :start (start iseq) :end (end iseq))))


(defmethod sref ((iseq infinite+-sequence) n)
  "Access element at position `n' in a sequence. Compute previously
uncalculated values."
  (when-in-range
    (with-iseq
      (ensure-array-size data+ i)
      (let ((value (aref data+ i)))
        (if (eq value +uncalculated+)
            (compute-value iseq n)
            value)))))

;;; TODO provide different update strategies: sequential filling, as
;;; needed and maybe others

;;; TODO make the sequence interface compatible with arrays and lists.

(defun compute-value (iseq n)
  "Fill position `n' in the sequence `iseq' using the
`generating-function'."
  (if (sequential-filling-p iseq)
      (with-slots (start data+ generating-function (fs minimal-uncalculated)) iseq
        (let ((sequence-offset start)) ; special variable
          (iter (for j from (+ start fs) to n)
                (for k from fs)
                ;; here we make use of the compatibility layer that
                ;; allows transparent use of the array
                (setf (aref data+ k)
                      (funcall generating-function data+ j)))))
      ;; TODO track circular dependencies for as-needed filling
      (with-iseq
        (setf (aref data+ i)
              (funcall (generating-function iseq) iseq n)))))

(defmethod set-sref ((iseq infinite+-sequence) n value)
  "Manually set position `n' in the sequence `iseq'."
  (when-in-range
    (with-iseq
      (ensure-array-size (data+ iseq) i)
      (setf (aref data+ i)
            value))))

;;; TODO do we actually want mutability??

;;; here comes compatibility functions for ordinary lisp sequences
(defmethod finite-sequence-p ((sequence sequence))
  t)

(defmethod length ((sequence sequence))
  (cl:length sequence))

(defparameter sequence-offset 0)

(defmethod sref ((array array) n)
  (aref array (- n sequence-offset)))

(defmethod set-sref ((array array) n value)
  (setf (aref array (- n sequence-offset)) value))

(defmethod sref ((list list) n)
  (nth (- n sequence-offset) list))

(defmethod set-sref ((list list) n value)
  (setf (nth (- n sequence-offset) list) value))
