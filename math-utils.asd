(defsystem math-utils
  :depends-on (ol-utils cl-utilities iterate)
  :components ((:file "generic-math")
               (:module "number-theory"
                        :components ((:file "packages")
                                     (:file "basic"         :depends-on ("packages"))
                                     (:file "primes"        :depends-on ("packages"))
                                     (:file "factorisation" :depends-on ("packages"))
                                     (:file "multiplicative-functions")))
               (:file "finite-fields" :depends-on ("generic-math"
                                                   "number-theory"))
               (:file "localisations" :depends-on ("generic-math"
                                                   "number-theory"
                                                   "finite-fields"))
               (:file "polynomials"   :depends-on ("generic-math"))
               (:file "power-series"  :depends-on ("generic-math"
                                                   "polynomials"))))
