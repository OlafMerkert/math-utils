(defsystem math-utils
  :depends-on (ol-utils cl-utilities iterate)
  :components ((:file "generic-math")
               (:file "infinite-math" :depends-on ("generic-math"))
               (:file "elementary-combinatorics")
               (:module "number-theory"
                        :depends-on ("generic-math")
                        :components ((:file "packages")
                                     (:file "basic"         :depends-on ("packages"))
                                     (:file "primes"        :depends-on ("packages"))
                                     (:file "factorisation" :depends-on ("packages"))
                                     (:file "multiplicative-functions" :depends-on ("packages"))))
               (:file "finite-fields" :depends-on ("generic-math"
                                                   "number-theory"))
               (:file "localisations" :depends-on ("generic-math"
                                                   "number-theory"
                                                   "finite-fields"))
               (:file "polynomials"   :depends-on ("generic-math" "finite-fields"))
               (:file "power-series"  :depends-on ("generic-math"
                                                   "polynomials"
                                                   "finite-fields"))
               (:file "polynomial-series-printing" :depends-on ("generic-math"
                                                                "polynomials"
                                                                "power-series"))
               (:module "elliptic-curves" :depends-on ("generic-math" "number-theory") 
                        :components ((:file "weierstrass")))
               (:file "valuations" :depends-on ("infinite-math"
                                                "number-theory"
                                                "polynomials" "power-series"))
               (:file "valuations-coeff" :depends-on ("infinite-math"
                                                      "valuations"
                                                      "polynomials" "power-series"))))
