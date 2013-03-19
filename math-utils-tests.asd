(defsystem math-utils-tests
  :depends-on (ol-utils math-utils fiveam)
  :serial t
  :components ((:module "tests"
                        :serial t
                        :components ((:file "packages")
                                     (:file "number-theory-tests")))))
