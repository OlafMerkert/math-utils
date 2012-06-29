(defsystem math-utils
  :serial t
  :depends-on (ol-utils)
  :components ((:file "generic-math")
               (:file "number-theory-basic")
               (:file "finite-fields")
               (:file "power-series")))
