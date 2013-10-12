(defpackage :infinite-sequence
  (:nicknames :ins)
  (:shadow #:length)
  (:use :cl :ol :iterate
        :infinite-math)
  (:export
   #:finite-sequence-p
   #:sequence-length
   #:sref
   #:subsequence
   #:map-sequence
   #:bind-seq
   #:seq->array
   #:infinite+-sequence
   #:infinite-sequence/standard-value
   #:data
   #:standard-value
   #:generating-function
   #:infinite-sequence
   #:start
   #:end
   #:indirect-sequence
   #:refer-to
   #:index-transform
   #:finseq
   #:inf+seq
   #:infinite--sequence
   #:inf-seq
   #:map-sequences/or
   #:map-sequences/and))

(in-package :infinite-sequence)

;;; general (infinite) sequence API
(defgeneric finite-sequence-p (sequence))
(defgeneric length (sequence))

(declaim (inline sequence-length))
(defun sequence-length (sequence)
  (length sequence))

(defgeneric sref (sequence n))
(defgeneric set-sref (sequence n value))

(defsetf sref (iseq n) (value)
  `(set-sref ,iseq ,n ,value))

;; support special keyword `:start' and `:end' instead of integer
;; indices. As the end is often infinite, we do an additional check.
(defmethod sref (sequence (n (eql :start)))
  (sref sequence (start sequence)))
(defmethod set-sref (sequence (n (eql :start)) value)
  (set-sref sequence (start sequence) value))

(define-condition infinite-index ()
  ((index :initarg :index
          :initform nil
          :accessor index)))

(defmethod sref (sequence (n (eql :end)))
  (with-slots (end) sequence
    (if (infinite-p end)
        (error 'infinite-index :index end)
        (sref sequence end))))
(defmethod set-sref (sequence (n (eql :end)) value)
  (with-slots (end) sequence
    (if (infinite-p end)
        (error 'infinite-index :index end)
        (set-sref sequence end value))))

(defgeneric subsequence (sequence start &optional end))

(defgeneric map-sequence (function sequence))

(defmacro! bind-seq (sequences o!index &body body)
  "For `sequences' given by symbols A, write A instead of (sref A index) -- this is intended only for read access."
  `(let ,(mapcar #`(,a1 (sref ,a1 ,g!index)) sequences)
     ,@body))

(defgeneric seq->array (sequence))

;;; singly infinite sequences
(defclass infinite-sequence ()
  ((start :initarg :start
          :initform 0
          :reader start)
   (end :initarg :end
        :initform infinity+
        :reader end))
  (:documentation "base class for infinite sequences."))

(defclass infinite+-sequence (infinite-sequence)
  ((data :initarg :data
          :initform (make-array 100 :initial-element +uncalculated+
                                :adjustable t)
          :reader data)
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
  integer N to +infinity. The infinity is realised using a generating
  function."))

(defmethod initialize-instance :after ((iseq infinite+-sequence) &key)
  (with-slots ((fs minimal-uncalculated) data) iseq
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
           (setf fs (or (position +uncalculated+ data)
                        (length data)))))
    (if (length=0 data)
        (setf data (make-array 100 :initial-element +uncalculated+
                               :adjustable t))
        (ensure-adjustable-array data))))

(declaim (inline sequential-filling-p))

(defun sequential-filling-p (iseq)
  (integerp (minimal-uncalculated iseq)))

;; (defclass infinite-sequence ()
;;   ()
;;   (:documentation "An infinite sequence on integers, covering all
;;   integers"))

(defmethod length ((iseq infinite-sequence))
  (with-slots (start end) iseq
    (gm:- end start)))

(defmethod finite-sequence-p ((seq infinite-sequence))
  (numberp (length seq)))

(defmacro with-iseq (&body body)
  "Bind slots `start' and `data'. Moreover, transform the sequence
index `n' to the array index `i'."
  `(with-slots (start data) iseq
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

(defmethod sref ((iseq infinite+-sequence) (n integer))
  "Access element at position `n' in a sequence. Compute previously
uncalculated values."
  (when-in-range
    (with-iseq
      (ensure-array-size data i)
      (let ((value (aref data i)))
        (if (eq value +uncalculated+)
            (compute-value iseq n)
            value)))))

(defun compute-value (iseq n)
  "Fill position `n' in the sequence `iseq' using the
`generating-function'."
  (with-iseq
    (if (sequential-filling-p iseq)
        (with-slots (generating-function (fs minimal-uncalculated)) iseq
          (let ((sequence-offset start)) ; special variable
            (iter (for j from (+ start fs) to n)
                  (for k from fs)
                  ;; here we make use of the compatibility layer that
                  ;; allows transparent use of the array
                  (setf (aref data k)
                        (funcall generating-function data j)))
            (setf fs (+ i 1))
            (aref data i)))
        ;; TODO track circular dependencies for as-needed filling
        (setf (aref data i)
              (funcall (generating-function iseq) iseq n)))))

(defmethod set-sref ((iseq infinite+-sequence) (n integer) value)
  "Manually set position `n' in the sequence `iseq'."
  (when-in-range
    (with-iseq
      (ensure-array-size (data iseq) i)
      (setf (aref data i)
            value))))

;;; TODO do we actually want mutability??
;;; todo track mutations and allow automatic recomputing

;;; important mapping and slicing functions
(defmethod map-sequence (function (iseq infinite+-sequence))
  (make-instance 'infinite+-sequence
                 :fill-strategy (if (sequential-filling-p iseq)
                                    0)
                 :start (start iseq)
                 :end (end iseq)
                 :generating-function (ilambda (iseq2 n)
                                        (funcall function (sref iseq n)))))



;;; todo helper functions to quickly transform arrays into infinite
;;; sequences and so on.

(defun compute-all (iseq)
  "For finite sequences, compute all elements of the sequence."
  (if (sequential-filling-p iseq)
      (sref iseq (- (end iseq) 1))
      (iter (for i from (start iseq) below (end iseq))
            (sref iseq i))))

(define-condition infinite-length-not-supported () ())

(defmethod seq->array ((iseq infinite+-sequence))
  "Return the array corresponding to a finite sequence."
  (if (finite-sequence-p iseq)
      (progn
        (compute-all iseq)
        (subseq (data iseq) 0 (- (end iseq) (start iseq))))
      (error 'infinite-length-not-supported)))

(defun array->iseq (array &key (start 0))
  "Generate a finite sequence from an `array'."
  (make-instance 'infinite+-sequence
                 :start start
                 :end (+ start (length array))
                 :data array))

(defun list->iseq (list &key (start 0))
  "Generate a finite sequence from a `list'."
  (make-instance 'infinite+-sequence
                 :start start
                 :end (+ start (length list))
                 :data (coerce list 'vector)))

;;; todo special indirect infinite sequences (useful for slicing and
;;; shifting?) that allow sharing content by reusing the backing array
(defclass indirect-sequence (infinite-sequence)
  ((refer-to :initarg :refer-to
             :accessor refer-to)
   (index-transform :initarg :index-transform
                    :initform #'identity
                    :accessor index-transform))
  (:documentation "Transparently access elements of sequence
  `refer-to' after transforming the index with `index-transform'.
  Careful with indices that are out of range."))

(defmethod initialize-instance :after ((iseq indirect-sequence) &key)
  ;; make sure refer-to does not hit an indirect-sequence
  (let (pre-refer-to)
    (setf (index-transform iseq)
          (apply #'compose/red
                 (iter (for seq first iseq then (refer-to seq))
                       (while (typep seq 'indirect-sequence))
                       (setf pre-refer-to seq)
                       (collect (index-transform seq) at start)))
          (refer-to iseq)
          (refer-to pre-refer-to))))

(defmethod sref ((iseq indirect-sequence) (n integer))
  (sref (refer-to iseq)
        (funcall (index-transform iseq) n)))

(defmethod set-sref ((iseq indirect-sequence) (n integer) value)
  (set-sref (refer-to iseq)
            (funcall (index-transform iseq) n)
            value))

(defmethod subsequence ((iseq infinite-sequence) start &optional (end infinity+))
  (make-instance 'indirect-sequence
                 :start (imax start (start iseq))
                 :end (imin end (end iseq))
                 :refer-to iseq))

(defun shift (iseq offset)
  "Shift all indices of the sequence `iseq' by `offset'."
  (make-instance 'indirect-sequence
                 :start (gm:+ (start iseq) offset)
                 :end (gm:+ (end iseq) offset)
                 :refer-to iseq
                 :index-transform (lambda (n) (- n offset))))

(define-condition doubly-infinite-not-supported () ())

(defmethod map-sequence (function (iseq indirect-sequence))
  (with-slots (start end refer-to index-transform) iseq
    (cond ((not (infinite-p start))
           (make-instance 'infinite+-sequence
                          :start start
                          :end end
                          :generating-function
                          (ilambda (iseq2 n)
                            (funcall function
                                     (sref refer-to (funcall index-transform n))))))
          ((not (infinite-p end))
           (make-instance 'infinite--sequence
                          :start start
                          :end end
                          :generating-function
                          (ilambda (iseq2 n)
                            (funcall function
                                     (sref refer-to (funcall index-transform n))))))
          (t (error 'doubly-infinite-not-supported)))))


(defmethod seq->array ((iseq indirect-sequence))
  (if (finite-sequence-p iseq)
      (iter (for i from (start iseq) below (end iseq))
            (collect (sref iseq i) result-type vector))
      (error 'infinite-length-not-supported)))

;;; infinite sequence with standard value outside its range.
(defclass infinite-sequence/standard-value (infinite-sequence)
  ((data :initarg :data
         :initform #()
         :accessor data)
   (standard-value :initarg :standard-value
                   :initform nil
                   :accessor standard-value))
  (:documentation "A doubly infinite sequence where only finitely many
  values are explicitly specified, for the rest we return a standard
  value. The `end' slot is automatically set to `start' + (length
  `data'). "))

(defmethod initialize-instance :after ((iseq infinite-sequence/standard-value) &key)
  ;; adjust end
  (with-slots (start end data) iseq
    (setf end (+ start (length data)))))


(defmethod finite-sequence-p ((iseq infinite-sequence/standard-value))
  t)

(defmethod length ((iseq infinite-sequence/standard-value))
  (- (end iseq) (start iseq)))

(defmethod sref ((iseq infinite-sequence/standard-value) (n integer))
  (if (in-range iseq n)
      (aref (data iseq) (- n (start iseq)))
      (standard-value iseq)))

(defmethod set-sref ((iseq infinite-sequence/standard-value) (n integer) value)
  (when-in-range
    (setf (aref (data iseq) (- n (start iseq))) value)))

(defmethod map-sequence (function (iseq infinite-sequence/standard-value))
  (make-instance 'infinite-sequence/standard-value
                 :start (start iseq)
                 :end (end iseq)
                 :data (map 'vector function (data iseq))
                 :standard-value (funcall function (standard-value iseq))))

(defmethod seq->array ((iseq infinite-sequence/standard-value))
  (data iseq))

(defun list->iseq/sv (list &key (start 0) standard-value)
  (make-instance 'infinite-sequence/standard-value
                 :start start
                 :end (+ start (length list))
                 :data (coerce list 'vector)
                 :standard-value standard-value ))

(defun array->iseq/sv (array &key (start 0) standard-value)
  (make-instance 'infinite-sequence/standard-value
                 :start start
                 :end (+ start (length array))
                 :data array
                 :standard-value standard-value ))


;;; here come compatibility functions for ordinary lisp sequences
(defmethod finite-sequence-p ((sequence sequence))
  t)

(defmethod length ((sequence sequence))
  (cl:length sequence))

(defparameter sequence-offset 0)

(defmethod start ((sequence sequence))
  sequence-offset)

(defmethod end ((sequence sequence))
  (+ sequence-offset (cl:length sequence)))

(defmethod sref ((array array) (n integer))
  (aref array (- n sequence-offset)))
(defmethod set-sref ((array array) (n integer) value)
  (setf (aref array (- n sequence-offset)) value))

(defmethod sref ((list list) (n integer))
  (nth (- n sequence-offset) list))
(defmethod set-sref ((list list) (n integer) value)
  (setf (nth (- n sequence-offset) list) value))

(defmethod subsequence ((sequence sequence) start &optional end)
  (subseq sequence start end))

(defmethod map-sequence (function (list list))
  (map 'list function list))

(defmethod map-sequence (function (vector vector))
  (map 'vector function vector))

(defmethod seq->array ((list list))
  (coerce list 'array))

(defmethod seq->array ((array array))
  array)

;;; a helper macro to port from lazy-array
#|(defmacro make-lazy-array ((&key start (index-var 'index) finite default-value) &body fill-form)
  (if (or finite default-value)
      `(make-instance 'infinite-sequence/standard-value
                      :standard-value ,default-value
                      :end (or finite infinite-math:infinity+)
                      :data (vector ,@start))
      `(make-instance 'infinite+-sequence
                      :fill-strategy :sequential
                      :data (vector ,@start)
                      :generating-function (lambda (this ,index-var)
                                             ,@(subst-if 'sref
                                                         (lambda (x) (member x '(aref lazy-aref)))
                                                         fill-form)))))|#
;;; shorthand notation for most frequent uses
(defmacro inf+seq (start (index-var) &body generating-expression)
  `(make-instance 'infinite+-sequence
                  :fill-strategy :sequential
                  :data ,start
                  :generating-function
                  (ilambda (this ,index-var)
                    ,@generating-expression)))

(defmacro finseq (start standard-value)
  `(make-instance 'infinite-sequence/standard-value
                  :standard-value ,standard-value
                  :data ,start))

;;; todo sequences towards -infinity
(defmacro inf-seq (start (index-var) &body generating-expression)
  `(make-instance 'infinite--sequence
                  :fill-strategy :sequential
                  :data ,start
                  :generating-function
                  (ilambda (this ,index-var)
                    ,@generating-expression)))

(defclass infinite--sequence (indirect-sequence)
  ((start :initform infinity-)
   (end :initform 0)
   (index-transform :initform #'-)
   (refer-to :initform nil))
  (:documentation "A singly infinite series going towards -oo."))

(defmethod initialize-instance :after ((iseq infinite--sequence)
                                       &key fill-strategy generating-function data)
  (with-slots (start end) iseq
    (setf (slot-value iseq 'refer-to)
          (make-instance 'infinite+-sequence
                         :start (gm:- end)
                         :end (gm:- start)
                         :fill-strategy (pass-symbol (- fill-strategy))
                         :data (reverse data)
                         :generating-function (lambda (iseq2 n)
                                                (funcall generating-function iseq2 (- n)))))))

;;; very powerful mapping function
(defun map-sequences/or (function &rest sequences)
  "Given a `function' that takes between 1 and number of `sequences'
arguments, build a new sequence, which applies the function on the
available elements of each index. This means, we are using the maximal
range available (so we take the union, not the intersection of the
domains of definition). However, all sequences should either be
bounded from below or from above."
  (let ((start (reduce #'imin sequences :key #'start))
        (end   (reduce #'imax sequences :key #'end)))
    (labels ((get-ref (seq i)
               (handler-case (sref seq i)
                 (index-out-of-range () +uncalculated+)))
             (get-refs (i)
               (remove +uncalculated+
                       (mapcar (lambda (seq) (get-ref seq i)) sequences)))
             (call-fun (iseq i)
               (declare (ignore iseq))
               (aif (get-refs i)
                    (apply function it)
                    (error 'index-out-of-range :start start :index i :end end))))
      (declare (inline get-refs))
     (cond ((not (infinite-p start)) ;; bounded below
            (make-instance 'infinite+-sequence
                           :start start
                           :end end
                           :generating-function #'call-fun))
           ((not (infinite-p end)) ;; bounded above
            (make-instance 'infinite--sequence
                           :start start
                           :end end
                           :generating-function #'call-fun))
           (t (error 'doubly-infinite-not-supported))))))

(defun map-sequences/and (function &rest sequences)
  "Given a `function' that takes as many arguments as there are
  `sequences', apply the `function' on the elements at same indices
  wherever all are defined (so we take the intersection of the domains
  of intersection). This does not work if all `sequences' are doubly
  infinite."
  (let ((start (reduce #'imax sequences :key #'start))
        (end   (reduce #'imin sequences :key #'end)))
    (labels ((get-refs (i)
               (mapcar (lambda (seq) (sref seq i)) sequences))
             (call-fun (iseq i)
               (declare (ignore iseq))
               (aif (get-refs i)
                    (apply function it)
                    (error 'index-out-of-range :start start :index i :end end))))
      (declare (inline get-refs))
     (cond ((not (infinite-p start)) ;; bounded below
            (make-instance 'infinite+-sequence
                           :start start
                           :end end
                           :generating-function #'call-fun))
           ((not (infinite-p end)) ;; bounded above
            (make-instance 'infinite--sequence
                           :start start
                           :end end
                           :generating-function #'call-fun))
           (t (error 'doubly-infinite-not-supported))))))

;;; todo perhaps add additional behaviour in case all sequences have
;;; finite support.
