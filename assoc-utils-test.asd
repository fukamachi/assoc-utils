#|
  This file is a part of assoc-utils project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem assoc-utils-test
  :author "Eitaro Fukamachi"
  :license "Public Domain"
  :depends-on (:assoc-utils
               :rove)
  :components ((:module "t"
                :components
                ((:file "assoc-utils"))))
  :description "Test system for assoc-utils"
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
