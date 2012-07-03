(defsystem math-utils
  :serial t
  :depends-on (ol-utils cl-utilities)
  :components ((:file "generic-math")
               (:file "number-theory-basic")
               (:file "finite-fields")
               (:file "polynomials")
               (:file "power-series")))
