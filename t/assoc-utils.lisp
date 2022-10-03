(in-package :cl-user)
(defpackage assoc-utils-test
  (:use :cl
        :assoc-utils
        :prove))
(in-package :assoc-utils-test)

(plan 10)

(subtest "aget"
  (is-error (aget 1 1) 'error)
  (is (aget nil 1) nil)
  (is (aget nil 1 "dflv") "dflv")
  (is-error (aget (cons 1 2) 1) 'error)
  (is-error (aget '((1 . 2) 3) #\x) 'error)
  (is (aget '(("name" . "Eitaro")) "name") "Eitaro"))

(subtest "(setf aget)"
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (setf (aget alist "name") "Fukamachi")
    (is (aget alist "name") "Fukamachi")
    (setf (aget alist "address") "Japan")
    (is (aget alist "address") "Japan")))

(subtest "(incf aget)"
  (let (alist)
    (incf (aget alist "value" 0))
    (is (aget alist "value") 1)))

(subtest "remove-from-alist & delete-from-alist"
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (is (remove-from-alist alist "name")
        '(("email" . "e.arrows@gmail.com")))
    (is alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
  (let ((alist '(("name" . "Eitaro")
                 ("email" . "e.arrows@gmail.com")
                 ("address" . "Japan"))))
    (is (delete-from-alist alist "email")
        '(("name" . "Eitaro") ("address" . "Japan")))
    (setf alist '(("name" . "Eitaro") ("address" . "Japan")))
    (is (delete-from-alist alist "name")
        '(("address" . "Japan"))))
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (remove-from-alistf alist "name")
    (is alist '(("email" . "e.arrows@gmail.com"))))

  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (delete-from-alistf alist "name")
    (is alist '(("email" . "e.arrows@gmail.com")))))

(subtest "remove-values-from-alist & delete-values-from-alist"
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (is (remove-values-from-alist alist "Eitaro")
        '(("email" . "e.arrows@gmail.com")))
    (is alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (delete-values-from-alist alist "Eitaro")
    (is alist '(("email" . "e.arrows@gmail.com")))))

(subtest "alist-plist & plist-alist"
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")))
        (plist '(:name "Eitaro" :email "e.arrows@gmail.com")))
    (is (alist-plist alist) plist)
    (is (plist-alist plist) alist)))

(subtest "alist-hash & hash-alist"
  (let* ((alist (copy-seq '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
         (hash (alist-hash alist)))
    (is (hash-table-count hash) 2)
    (is (gethash "name" hash) "Eitaro")
    (is (gethash "email" hash) "e.arrows@gmail.com")
    (is (sort (hash-alist hash) #'string< :key #'car)
        (sort alist #'string< :key #'car))))

(subtest "alist-keys & alist-values"
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (is (alist-keys alist) '("name" "email"))
    (is (alist-values alist) '("Eitaro" "e.arrows@gmail.com"))))

(subtest "alistp"
  (ok (alistp '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
  (ok (not (alistp '((("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))))
  (ok (alistp '()))
  (ok (not (alistp 1)))
  (ok (not (alistp (cons 1 2))))

  (is-type '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")) 'alist))

(subtest "alist="
  (ok (alist= '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))
              '(("email" . "e.arrows@gmail.com") ("name" . "Eitaro"))))
  (ok (not (alist= '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))
                   '(("name" . "Eitaro")))))
  (ok (not (alist= '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))
                   '())))
  (ok (alist= nil nil)))

(finalize)
