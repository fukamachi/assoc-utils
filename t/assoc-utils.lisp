(in-package :cl-user)
(defpackage assoc-utils-test
  (:use :cl
        :assoc-utils
        :rove))
(in-package :assoc-utils-test)

(deftest aget
  (ok (signals (aget 1 1)))
  (ok (eql (aget nil 1) nil))
  (ok (string= (aget nil 1 "dflv") "dflv"))
  (ok (signals (aget (cons 1 2) 1)))
  (ok (signals (aget (list (cons 1 2) 3) #\x)))
  (ok (string= (aget (list (cons "name" "Eitaro")) "name") "Eitaro")))

(deftest setf-aget
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (setf (aget alist "name") "Fukamachi")
    (ok (string= (aget alist "name") "Fukamachi"))
    (setf (aget alist "address") "Japan")
    (ok (string= (aget alist "address") "Japan"))))

(deftest incf-aget
  (let (alist2)
    (incf (aget alist2 "value" 0))
    (ok (= (aget alist2 "value") 1))))

(deftest remove-from-alist
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (ok (equalp (remove-from-alist alist "name")
		'(("email" . "e.arrows@gmail.com"))))
    (ok (equalp alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))))

(deftest delete-from-alist
  (let ((alist '(("name" . "Eitaro")
                 ("email" . "e.arrows@gmail.com")
                 ("address" . "Japan"))))
    (ok (equalp (delete-from-alist alist "email")
		'(("name" . "Eitaro") ("address" . "Japan"))))
    (setf alist '(("name" . "Eitaro") ("address" . "Japan")))
    (ok (equalp (delete-from-alist alist "name")
		'(("address" . "Japan"))))))

(deftest remove-from-alistf
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (remove-from-alistf alist "name")
    (ok (equalp alist '(("email" . "e.arrows@gmail.com"))))))

(deftest delete-from-alistf
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (delete-from-alistf alist "name")
    (ok (equalp alist '(("email" . "e.arrows@gmail.com"))))))

(let ((alist (list (cons "name" "Eitaro") (cons "email" "e.arrows@gmail.com")))
      (plist (list :name "Eitaro" :email "e.arrows@gmail.com")))
  (deftest alist-plist
    (ok (equalp plist (alist-plist alist))))
  (deftest plist-alist
    (ok (equalp alist (plist-alist plist)))))

(deftest association-list-hash-table
  (testing "alist-hash & hash-alist"
    (let* ((alist (copy-seq (list (cons "name" "Eitaro") (cons "email" "e.arrows@gmail.com"))))
	   (hash (alist-hash alist)))
      (ok (= (hash-table-count hash) 2))
      (ok (string= (gethash "name" hash) "Eitaro"))
      (ok (string= (gethash "email" hash) "e.arrows@gmail.com"))
      (ok (equalp (sort (hash-alist hash) #'string< :key #'car)
		  (sort alist #'string< :key #'car))))))

(deftest alist-keys
  (let ((alist '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
    (ok (equalp (alist-keys alist) '("name" "email")))))

(deftest alist-values
  (let ((alist (list (cons "name" "Eitaro") (cons "email" "e.arrows@gmail.com"))))
    (ok (equalp (alist-values alist) (list "Eitaro" "e.arrows@gmail.com")))))

(deftest alistp
  (ok (alistp '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))
  (ok (not (alistp '((("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))))))
  (ok (alistp '()))
  (ok (not (alistp 1)))
  (ok (not (alistp (cons 1 2))))

  (ok (typep '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")) 'alist)))

(deftest alist=
  (ok (alist= (list (cons "name" "Eitaro") (cons "email" "e.arrows@gmail.com"))
              (list (cons "email" "e.arrows@gmail.com") (cons "name" "Eitaro"))))
  (ok (not (alist= '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))
                   '(("name" . "Eitaro")))))
  (ok (not (alist= '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))
                   '())))
  (ok (alist= nil nil)))
