(in-package :cl-user)
(defpackage assoc-utils
  (:use #:cl)
  (:export #:*assoc-test*
           #:aget
           #:remove-from-alist
           #:remove-from-alistf
           #:delete-from-alist
           #:delete-from-alistf
           #:alist-plist
           #:plist-alist
           #:alist-hash
           #:hash-alist
           #:alist-keys
           #:alist-values
           #:alistp
           #:alist))
(in-package :assoc-utils)

(defvar *assoc-test* #'equal)

(defun aget (alist key &optional default)
  (let ((kv (assoc key alist :test *assoc-test*)))
    (if kv
        (cdr kv)
        default)))

(defun %aput (alist key value)
  (let ((kv (find key alist :key #'car :test *assoc-test*)))
    (if kv
        (progn
          (rplacd kv value)
          alist)
        (cons (cons key value) alist))))

(define-setf-expander aget (alist key &environment env)
  (multiple-value-bind (dummies vals newvals setter getter)
      (get-setf-expansion alist env)
    (let ((newval (first newvals)))
      (values dummies
              vals
              newvals
              `(let ((,newval (%aput ,alist ,key ,newval)))
                 ,setter
                 ,newval)
              `(aget ,getter ,key)))))

(defun remove-from-alist (alist &rest keys)
  (remove-if
   (lambda (kv)
     (find (car kv) keys :test *assoc-test*))
   alist))

(define-modify-macro remove-from-alistf (&rest keys) remove-from-alist)

(defun delete-from-alist (alist &rest keys)
  (let ((alist
          (member-if-not (lambda (kv)
                           (find (car kv) keys :test *assoc-test*))
                         alist)))
    (mapl (lambda (tree)
            (let ((kv (second tree)))
              (when (find (car kv) keys :test *assoc-test*)
                (rplacd tree (cddr tree)))))
          alist)))

(define-modify-macro delete-from-alistf (&rest keys) delete-from-alist)

(defun alist-plist (alist)
  (mapcan (lambda (kv)
            (list (intern (string-upcase (car kv)) :keyword)
                  (cdr kv)))
          alist))

(defun plist-alist (plist)
  (loop for (k v) on plist by #'cddr
        collect (cons (string-downcase k) v)))

(defun alist-hash (alist)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (kv alist hash)
      (setf (gethash (car kv) hash) (cdr kv)))))

(defun hash-alist (hash)
  (loop for key being the hash-keys of hash
          using (hash-value value)
        collect (cons key value)))

(defun alist-keys (alist)
  (mapcar #'car alist))

(defun alist-values (alist)
  (mapcar #'cdr alist))

(defun alistp (value)
  (or (null value)
      (and (consp value)
           (mapl (lambda (tree)
                   (unless (and (consp (first tree))
                                (listp (rest tree)))
                     (return-from alistp nil)))
                 value)
           t)))

(deftype alist () '(satisfies alistp))
