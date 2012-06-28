(defsystem math-utils
  :serial t
  :depends (ol-utils)
  :components ((:file "generic-math")
               (:file "number-theory-basic")
               (:file "finite-fields")))
