(defsystem math-utils-tests
  :depends-on (ol-utils math-utils fiveam)
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "generic-math-tests")
                                     (:file "infinite-sequence-tests")
                                     (:file "number-theory-tests")
                                     (:file "polynomial-tests")
                                     (:file "mpolynomial-tests")))))
