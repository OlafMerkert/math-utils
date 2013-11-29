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
   #:inf+seq
   #:infinite--sequence
   #:inf-seq
   #:map-sequences/or
   #:map-sequences/and
   #:strip-if
   #:shift
   #:seq->iseq/sv
   #:flambda))

(in-package :infinite-sequence)

;;; general (infinite) sequence API
(defgeneric finite-sequence-p (sequence))
(defgeneric length (sequence))
;; use this shadowing definition only in this package, for the rest of
;; the world define
(declaim (inline sequence-length))
(defun sequence-length (sequence)
  (length sequence))

(defgeneric sref (sequence n))
(defgeneric set-sref (sequence n value))

(defsetf sref (iseq n) (value)
  `(set-sref ,iseq ,n ,value))

;; support special keyword `:start' and `:end' instead of integer
;; indices. As these can be infinite, we do an additional check.
(define-condition infinite-index ()
  ((index :initarg :index
          :initform nil
          :accessor index)))

(defmethod sref (sequence (n (eql :start)))
  (with-slots (start) sequence
    (if (infinite-p start)
        (error 'infinite-index :index start)
        (sref sequence start))))

(defmethod set-sref (sequence (n (eql :start)) value)
  (with-slots (start) sequence
    (if (infinite-p start)
        (error 'infinite-index :index start)
        (set-sref sequence start value))))

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

;; for reasons of symmetry, both `start' and `end' are supposed to be
;; inclusive (unless they are infinite)

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
   ;; first a function to retrieve entries of the very same sequence,
   ;; and an index `n'. Be careful to avoid infinite loops when
   ;; accessing other entries in the `generating-function'. In
   ;; sequential filling mode, this might give errors or return
   ;; `+uncalculated+', but otherwise this can create circular
   ;; dependencies which are not caught at the moment.
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
    (cond ((length=0 data)
           (setf data (make-array 100 :initial-element +uncalculated+
                                  :adjustable t)))
          ((arrayp data)
           (ensure-adjustable-array data))
          ((listp data)
           (setf data (make-array (length data) :adjustable t
                                  :initial-contents data)))
          (t (error "invalid data in infinite+-sequence: ~A" data)))))

(declaim (inline sequential-filling-p))

(defun sequential-filling-p (iseq)
  (integerp (minimal-uncalculated iseq)))

;; (defclass infinite-sequence ()
;;   ()
;;   (:documentation "An infinite sequence on integers, covering all
;;   integers"))

(defmethod length ((iseq infinite-sequence))
  (with-slots (start end) iseq
    (gm:+ 1 (gm:- end start))))

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
`size'+1 elements (i.e. we can get at the index `size')."
  (if (>= size (cl:length array))
      (adjust-array array (+ size array-size-step) :initial-element +uncalculated+)
      array))

(declaim (inline in-range))
(defun in-range (iseq n)
  "Make sure the given index is valid for the sequence."
  (and (integerp n)
       (i<= (start iseq) n)
       (i<= n (end iseq))))

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
          (progn
            (iter (for j from (+ start fs))
                  (for k from fs to i)
                  ;; here we make use of the compatibility layer that
                  ;; allows transparent use of the array
                  (setf (aref data k)
                        (funcall generating-function
                                 (lambda (index)
                                   (aref data (- index start)))
                                 j)))
            (setf fs (+ i 1))
            (aref data i)))
        ;; TODO track circular dependencies for as-needed filling
        (setf (aref data i)
              (funcall (generating-function iseq)
                       (lambda (index)
                         (sref iseq index))
                       n)))))

;; todo perhaps move
(defmacro! flambda ((f-arg &rest args) &body body)
  "With the first argument a function, make it possible to avoid using
apply or funcall in code by locally binding a function `f-arg'."
  `(lambda (,g!f-arg ,@args)
     (flet ((,f-arg (&rest ,g!args) (apply ,g!f-arg ,g!args)))
       ,@body)))

(defmethod set-sref ((iseq infinite+-sequence) (n integer) value)
  "Manually set position `n' in the sequence `iseq'."
  (when-in-range
    (with-iseq
      (ensure-array-size (data iseq) i)
      (setf (aref data i)
            value))))

;;; todo do we actually want mutability??
;;; todo track mutations and allow automatic recomputing

;;; important mapping and slicing functions

(defmethod map-sequence (function (iseq infinite+-sequence))
  ;; don't use sequential filling for mapped sequences, this avoids
  ;; computations that may never be needed. If you really want this,
  ;; you can easily get it manually.
  (make-instance 'infinite+-sequence
                 :start (start iseq)
                 :end (end iseq)
                 :generating-function (ilambda (this n)
                                        (funcall function (sref iseq n)))))


;;; helper functions to quickly transform arrays into infinite
;;; sequences and so on.

(defun compute-all (iseq)
  "For finite sequences, compute all elements of the sequence."
  (if (sequential-filling-p iseq)
      (sref iseq (end iseq))
      (iter (for i from (start iseq) to (end iseq))
            (sref iseq i))))

(define-condition infinite-length-not-supported () ())

(defmethod seq->array ((iseq infinite+-sequence))
  "Return the array corresponding to a finite sequence."
  (if (finite-sequence-p iseq)
      (progn
        (compute-all iseq)
        (subseq (data iseq) 0 (length iseq)))
      (error 'infinite-length-not-supported)))

(defun array->iseq (array &key (start 0))
  "Generate a finite sequence from an `array'."
  (make-instance 'infinite+-sequence
                 :start start
                 :end (+ start (length array) -1)
                 :data array))

(defun list->iseq (list &key (start 0))
  "Generate a finite sequence from a `list'."
  (make-instance 'infinite+-sequence
                 :start start
                 :end (+ start (length list) -1)
                 :data (coerce list 'vector)))

;;; special indirect infinite sequences (useful for slicing and
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
  ;; no in-range test here, because `indirect-sequence' are also used
  ;; in conjunction with `infinite-sequence/standard-value'
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
                          (ilambda (this n)
                            (funcall function
                                     (sref refer-to (funcall index-transform n))))))
          ((not (infinite-p end))
           (make-instance 'infinite--sequence
                          :start start
                          :end end
                          :generating-function
                          (ilambda (this n)
                            (funcall function
                                     (sref refer-to (funcall index-transform n))))))
          (t (error 'doubly-infinite-not-supported)))))


(defmethod seq->array ((iseq indirect-sequence))
  (if (finite-sequence-p iseq)
      (iter (for i from (start iseq) to (end iseq))
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

;; todo see how well this standard-value thing works out, and whether
;; considering it as a finite sequence is useful
(defmethod initialize-instance :after ((iseq infinite-sequence/standard-value) &key)
  ;; adjust `start' or `end' if infinite
  (with-slots (start end data) iseq
    (cond ((and (infinite-p start) (infinite-p end))
           (setf start 0
                 end (- (length data) 1)))
          ((infinite-p end)
           (setf end (+ start (length data) -1)))
          ((infinite-p start)
           (setf start (- end (length data) -1))))))

(defmethod finite-sequence-p ((iseq infinite-sequence/standard-value))
  t)

(defmethod length ((iseq infinite-sequence/standard-value))
  (+ 1 (- (end iseq) (start iseq))))

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

;; todo for a finite subsequence, how about filling up non-specified
;; entries? would be useful for printing. On the other hand, maybe
;; se->array and array->iseq/sv should be exactly inverse to each
;; other.
(defmethod seq->array ((iseq infinite-sequence/standard-value))
  (data iseq))

(defun seq->iseq/sv (seq &key (start 0) (end infinity+) standard-value)
  (make-instance 'infinite-sequence/standard-value
                 :start start
                 :end end
                 :data (coerce seq 'vector)
                 :standard-value standard-value ))

(defun array->iseq/sv (array &key (start 0) standard-value)
  (make-instance 'infinite-sequence/standard-value
                 :start start
                 :end (+ start (length array) -1)
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
  (+ sequence-offset (cl:length sequence) -1))

(defmethod sref ((array array) (n integer))
  (aref array (- n sequence-offset)))
(defmethod set-sref ((array array) (n integer) value)
  (setf (aref array (- n sequence-offset)) value))

(defmethod sref ((list list) (n integer))
  (nth (- n sequence-offset) list))
(defmethod set-sref ((list list) (n integer) value)
  (setf (nth (- n sequence-offset) list) value))

(defmethod subsequence ((sequence sequence) start &optional end)
  (subseq sequence start (+ 1 end)))

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
(defmacro inf+seq (initial-data (index-var &optional (start 0)) &body generating-expression)
  `(make-instance 'infinite+-sequence
                  :fill-strategy :sequential
                  :data ,initial-data
                  :start ,start
                  :generating-function
                  (flambda (this ,index-var)
                    ,@generating-expression)))

;;; sequences towards -infinity
(defmacro inf-seq (initial-data (index-var &optional (end 0)) &body generating-expression)
  `(make-instance 'infinite--sequence
                  :fill-strategy :sequential
                  :data ,initial-data
                  :end ,end
                  :generating-function
                  (flambda (this ,index-var)
                    ,@generating-expression)))

(defun index- (n &optional special)
  "Special index transform that transparently deals with `special'
  bounds `:start' and `:end' when taking (- n)."
  (cond ((eq n infinity+) infinity-)
        ((eq n infinity-) infinity+)
        ((eq special :start)
         ;; this is the inclusive lower bound, it should become a
         ;; inclusive upper bound, so we go one up
         (cl:- n))
        ((eq special :end)
         ;; this is the inclusive upper bound, it should become an
         ;; inclusive lower bound
         (cl:- n))
        (t (cl:- n))))

;; todo maybe move
(defun compose2 (fun1 fun2)
  (lambda (&rest args) (funcall fun1 (apply fun2 args))))

(defclass infinite--sequence (indirect-sequence)
  ((start :initform infinity-)
   (end :initform 0)
   (index-transform :initform #'index-)
   (refer-to :initform nil))
  (:documentation "A singly infinite series going towards -oo."))

(defmethod initialize-instance :after ((iseq infinite--sequence)
                                       &key fill-strategy generating-function data)
  (with-slots (start end) iseq
    (setf (slot-value iseq 'refer-to)
          (make-instance 'infinite+-sequence
                         :start (index- end :end)
                         :end (index- start :start)
                         :fill-strategy (pass-symbol (- fill-strategy))
                         ;; no need to reverse, that is done by the
                         ;; index-transform
                         :data data
                         :generating-function
                         (lambda (access-function n)
                           (funcall generating-function
                                    (compose2 access-function #'-)
                                    (- n)))))))

;; todo write some tests to make sure all of this stuff works properly

;;; very powerful mapping function
(defun map-sequences/or (function default &rest sequences)
  "Given a `function' that takes as many arguments ass there are
`sequences' arguments, build a new sequence, which applies the
function on the available elements of each index. Unavailable elements
are \"substituted\" by `default'. This means, we are using the maximal
range available (so we take the union, not the intersection of the
domains of definition). However, all sequences should either be
bounded from below or from above.

 Special treatment happens when `default' is `+uncalculated', then
`function' should also handle a lesser number of arguments, and the
`default' value is removed from the arguments list."
  (let ((start (reduce #'imin sequences :key #'start))
        (end   (reduce #'imax sequences :key #'end)))
    (labels ((get-ref (seq i)
               (handler-case (sref seq i)
                 (index-out-of-range () default)))
             (get-refs (i)
               (if (eq default +uncalculated+)
                   (remove +uncalculated+
                           #1=(mapcar (lambda (seq) (get-ref seq i)) sequences))
                   #1#))
             (call-fun (access-function i)
               (declare (ignore access-function))
               (aif (get-refs i)
                    (apply function it)
                    (error 'index-out-of-range :start start :index i :end end))))
      (declare (inline get-refs))
      (cond ((and (not (infinite-p start)) ; doubly bounded
                  (not (infinite-p end)))
             (make-instance 'infinite-sequence/standard-value
                            :start start :end end
                            :standard-value (aif (find-if (clambda typep x! 'infinite-sequence/standard-value) sequences)
                                                 (standard-value it)
                                                 (if (eq default +uncalculated+)(error "Cannot find suitable default value for conjunction of finite sequences.")
                                                     default))
                            
                            
                            :data (iter (for i from start to end)
                                        (collect (call-fun nil i) result-type vector))))
             
            ((not (infinite-p start))   ; bounded below

             (make-instance 'infinite+-sequence
                            :start start :end end
                            :generating-function #'call-fun))
            ((not (infinite-p end))     ; bounded above
             (make-instance 'infinite--sequence
                            :start start :end end
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
             (call-fun (access-function i)
               (declare (ignore access-function))
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

(defparameter limit 1000)
(defgeneric strip-if (test sequence &key from limit))

(defmethod strip-if (test (iseq infinite-sequence) &key (from :start) (limit limit))
  (ecase from
    (:start
     (iter (for i from (start iseq))
           (for j from 0 to limit)
           (while (i<= i (end iseq)))
           (while (funcall test (sref iseq i)))
           (finally (return (values (subsequence iseq i) j)))))
    (:end
     (iter (for i downfrom (end iseq) )
           (for j from 0 to limit)
           (while (i<= (start iseq) i))
           (while (funcall test (sref iseq i)))
           (finally (return (values (subsequence iseq infinity- i) j)))))))

(defmethod strip-if (test (seq sequence) &key (from :start) (limit limit))
  (ecase from
    (:start
     (let* ((highbound (min (length seq) limit))
            (pos (or (position-if-not test seq :end highbound) highbound)))
       (values (subseq seq pos) pos)))
    (:end
     (let* ((lowbound (max 0 (- (length seq) limit)))
            (pos (+ 1 (or (position-if-not test seq :from-end t :start lowbound)
                          lowbound))))
       (values (subseq seq 0 pos) (- (length seq) pos))))))


;; todo for infinite sequences, the `this' parameter for the
;; generating function does no longer make sense, because it is only
;; useful when we know about all necessary index offsets and
;; transforms. This is particularly problematic for series toward
;; infinity-.

;;; sometimes, it is more useful to have just very simple sequences
;;; that start just from 0 and go to infinity+. Together with the
;;; sequence mapping function we have so far, and indirect sequences,
;;; we get something slightly nicer than the `lazy-array'
