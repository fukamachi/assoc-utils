#|
  This file is a part of assoc-utils project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Utilities for manipulating association lists

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage assoc-utils-asd
  (:use :cl :asdf))
(in-package :assoc-utils-asd)

(defsystem assoc-utils
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "Public Domain"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "assoc-utils"))))
  :description "Utilities for manipulating association lists"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op assoc-utils-test))))
