(defpackage :elliptic-curve-weierstrass
  (:nicknames :ec-ws)
  (:shadowing-import-from :generic-math
                          :+ :* :/ :expt :- :=)
  (:use :cl :ol )
  (:export))

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

(defclass point-2 ()
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

(defclass ec-point-infinity ()
  ((curve :initarg :curve
         :initform nil
         :accessor curve))
  (:documentation "The infinite point of an elliptic curve."))

(defmethod infinite-point ((curve elliptic-curve-weierstrass))
  (make-instance 'ec-point-infinity :curve curve))

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

(defun ec-ws-+% (curve lambda nu x1 &optional (x2 x1))
  (make-instance 'ec-point-ws
                 :curve curve
                 :x (- (^ lambda 2) x1 x2)
                 :y (- (* lambda (+ x1 x2) ) (^ lambda 3) nu)))

(defun ec-ws-2times (curve x1 y1)
  (with-slots (a b) curve
    (ec-ws-+% curve
              (/ (+ (* 3 (^ x1 2)) a) 2 y1)
              (/ (+ (- (^ x1 3) (* a x1) (* 2 b)))
                 2 y1)
              x1)))

(defun ec-ws-+ (curve x1 y1 x2 y2)
  (ec-ws-+% curve
            (/ (- y2 y1) (- x2 x1))
            (/ (- (* y1 x2) (* y2 x1))
               (- x2 x1))
            x1 x2))

;; inverse of points
(defmethod gm:zero ((point ec-point-ws))
  (infinite-point (curve point)))

(defmethod gm:zero ((point ec-point-infinity))
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

