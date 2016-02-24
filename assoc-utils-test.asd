#|
  This file is a part of assoc-utils project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage assoc-utils-test-asd
  (:use :cl :asdf))
(in-package :assoc-utils-test-asd)

(defsystem assoc-utils-test
  :author "Eitaro Fukamachi"
  :license "Public Domain"
  :depends-on (:assoc-utils
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "assoc-utils"))))
  :description "Test system for assoc-utils"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
