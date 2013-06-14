(defpackage :elliptic-curve-weierstrass
  (:nicknames :ec-ws)
  (:shadow :zero)
  (:shadowing-import-from :generic-math
                          :+ :* :/ :expt :- :=)
  (:use :cl :ol :iterate)
  (:export
   #:elliptic-curve-weierstrass
   #:elliptic-curve
   #:ws-a
   #:ws-b
   #:discriminant
   #:j-invariant
   #:point-2
   #:x
   #:y
   #:ec-point-ws
   #:on-curve-p
   #:curve
   #:ec-point-infinity
   #:infinite-point
   #:ec-ws-iso
   #:lutz-nagell-test
   #:ec-rational-p
   #:ec-rational-torsion-p
   #:ec-torsion-p))

;;; TODO use operations from generic math

(in-package :elliptic-curve-weierstrass)

;;; TODO move this to a better place.
(declaim (inline ^))
(defun ^ (base power)
  "an alias for EXPT."
  (expt base power))

(defclass elliptic-curve ()
  ()
  (:documentation "an elliptic curve base class."))

(defclass elliptic-curve-weierstrass (elliptic-curve)
  ((a :initarg :a
         :initform 0
         :accessor ws-a)
   (b :initarg :b
         :initform 0
         :accessor ws-b))
  (:documentation "an affine elliptic curve respresented in weierstrass-form"))

(defmethod discriminant ((curve elliptic-curve-weierstrass) &key (simple nil))
  (with-slots (a b) curve
    (* (if simple 1 -16)
       (+ (* 4 (^ a 3))
          (* 27 (^ b 2))))))

(defmethod j-invariant ((curve elliptic-curve-weierstrass))
  (with-slots (a) curve
    (* 1728 4 (^ a 3) (/ (discriminant curve :simple t)))))

(defclass point-2 (gm:generic-math-object)
  ((x :initarg :x
         :initform 0
         :accessor x)
   (y :initarg :y
         :initform 0
         :accessor y))
  (:documentation "a tupel, with coordinates named X and Y."))

(defclass ec-point-ws (point-2)
  ((curve :initarg :curve
         :initform nil
         :accessor curve))
  (:documentation "a point on an elliptic curve in weierstrass form."))

(defmethod on-curve-p ((point ec-point-ws))
  (with-slots (x y curve) point
    (with-slots (a b) curve
      (= (^ y 2)
         (+ (^ x 3) (* a x) b)))))

(defclass ec-point-infinity (gm:generic-math-object)
  ((curve :initarg :curve
         :initform nil
         :accessor curve))
  (:documentation "The infinite point of an elliptic curve."))

(defmethod infinite-point ((curve elliptic-curve-weierstrass))
  (make-instance 'ec-point-infinity :curve curve))

(defmethod gm:zero-p ((point ec-point-infinity))
  t)

(defmethod gm:zero-p ((point ec-point-ws))
  nil)

(defmethod gm:generic-+ ((p1 ec-point-infinity) (p2 ec-point-infinity))
  p1)

(defmethod gm:generic-+ ((p1 ec-point-infinity) (p2 ec-point-ws))
  p2)

(defmethod gm:generic-+ ((p2 ec-point-ws) (p1 ec-point-infinity))
  p2)

(defmethod gm:generic-+ ((p1 ec-point-ws) (p2 ec-point-ws))
  (with-slots ((x1 x) (y1 y) curve) p1
    (with-slots ((x2 x) (y2 y) (curve2 curve)) p2
      (cond ((not (eq curve curve2))
             (error "points on different curves ~A and ~A" curve curve2))
            ((not (= x1 x2))
             (ec-ws-+ curve x1 y1 x2 y2))
            ((= y1 y2)
             (ec-ws-2times curve x1 y1))
            (t (infinite-point curve))))))

(declaim (inline  ec-ws-+%  ec-ws-2times  ec-ws-+))

;; TODO check these algorithms // might have some mistake

(defun ec-ws-+% (curve m y1 x1 &optional (x2 x1))
  (let ((c (- y1 (* m x1))))
   (make-instance 'ec-point-ws
                  :curve curve
                  :x (- (^ m 2) x1 x2)
                  :y (- (* m (+ x1 x2)) (^ m 3) c))))

(defun ec-ws-2times (curve x1 y1)
  (with-slots (a b) curve
    (ec-ws-+% curve
              (/ (+ (* 3 (^ x1 2)) a) 2 y1)
              y1
              x1)))

(defun ec-ws-+ (curve x1 y1 x2 y2)
  (ec-ws-+% curve
            (/ (- y2 y1) (- x2 x1))
            y1
            x1 x2))

;; inverse of points
(defmethod zero ((point ec-point-ws))
  (infinite-point (curve point)))

(defmethod zero ((point ec-point-infinity))
  point)

(defmethod gm:generic-- ((p1 ec-point-infinity) (p2 ec-point-infinity))
  p1)

(defmethod gm:generic-- ((p1 ec-point-ws) (p2 ec-point-infinity))
  p1)

(progn
  (defmethod gm:generic-- ((p1 ec-point-infinity) (p2 ec-point-ws))
   #1=(make-instance 'ec-point-ws
                     :curve (curve p2)
                     :x (x p2)
                     :y (- (y p2))))

 (defmethod gm:generic-- ((p1 ec-point-ws) (p2 ec-point-ws))
   (gm:generic-+ p1 #1#)))


;; scalar multiplication

(defmethod gm:generic-* ((n integer) (point ec-point-infinity))
  point)

(defmethod gm:generic-* ((n integer) (point ec-point-ws))
  (if (minusp n)
      (gm:generic-* (cl:- n) (gm:- point))
      (square-multiply point n #'gm:generic-+
                       ;; custom squaring
                       (lambda (point)
                         (ec-ws-2times (curve point) (x point) (y point))))))

;; reparametrise to integer parameters
(defun ec-ws-iso (curve c)
  (let ((new-curve (make-instance 'elliptic-curve-weierstrass
                         :a (* (^ c 4) (ws-a curve))
                         :b (* (^ c 6) (ws-b curve)))))
   (values (lambda (point)
             (unless (eq (curve point) curve)
               (error "Point does not lie on the source curve."))
             (typecase point
               (ec-point-infinity (infinite-point new-curve))
               (ec-point-ws (make-instance 'ec-point-ws
                                           :curve new-curve
                                           :x (* (^ c 2) (x point))
                                           :y (* (^ c 3) (y point))))))
           new-curve)))

;; determine the desired c
(defun eliminate-denominators (a b)
  "find integer c s.t. c^4 a and c^6 b are both integers"
  (let* ((factors-a (nt:factorise (denominator a) :singletons))
         (factors-b (nt:factorise (denominator b) :singletons))
         (primes (union (mapcar #'car factors-a)
                        (mapcar #'car factors-b))))
    (reduce #'* primes :key
            (lambda (p)
              (^ p
                 (ceiling
                  (max 0
                       (/ (or (assoc1 p factors-a) 0) 4)
                       (/ (or (assoc1 p factors-b) 0) 6))))))))

(defun lutz-nagell-test (curve point)
  (or (typep point 'ec-point-infinity)
      (multiple-value-bind (iso curve)
          (ec-ws-iso curve
                     (eliminate-denominators (ws-a curve) (ws-b curve)))
        (let ((point (funcall iso point)))
          (cond ((not (and (integerp (x point)) (integerp (y point))))
                 nil)
                ((not (nt:divides-p (^ (y point) 2)
                                    (discriminant curve)))
                 nil)
                (t t))))))

;; TODO move to a more general place
(defun order-find (group-element multiplication identity-test &optional (order-bound))
  (iter (for n from 1 to order-bound)
        (for g initially group-element then (funcall multiplication g group-element))
        (when (funcall identity-test g)
          (return n))
        (finally (return nil))))

(defun ec-rational-p (point)
  (and (rationalp (x point))
       (rationalp (y point))
       (rationalp (ws-a (curve point)))
       (rationalp (ws-b (curve point)))))

(defun ec-rational-torsion-p (point)
  (when (lutz-nagell-test (curve point) point)
    (order-find point #'gm:+ #'gm:zero-p 12)))

(defun ec-torsion-p (point &optional (order-bound 40))
  (order-find point #'gm:+ #'gm:zero-p order-bound))
