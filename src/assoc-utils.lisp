(in-package :cl-user)
(defpackage assoc-utils
  (:use #:cl)
  (:export #:*assoc-test*
           #:aget
           #:remove-from-alist
           #:remove-from-alistf
           #:delete-from-alist
           #:delete-from-alistf
           #:remove-values-from-alist
           #:delete-values-from-alist
           #:alist-plist
           #:plist-alist
           #:alist-hash
           #:hash-alist
           #:alist-keys
           #:alist-values
           #:alistp
           #:alist
           #:alist=))
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

(define-setf-expander aget (alist key &optional default &environment env)
  (multiple-value-bind (dummies vals newvals setter getter)
      (get-setf-expansion alist env)
    (let ((newval (first newvals)))
      (values dummies
              vals
              newvals
              `(let ((,newval (%aput ,alist ,key ,newval)))
                 ,setter
                 ,newval)
              `(aget ,getter ,key ,default)))))

(defun remove-from-alist (alist &rest keys)
  (remove-if
   (lambda (kv)
     (find (car kv) keys :test *assoc-test*))
   alist))

(define-modify-macro delete-from-alist  (&rest keys) remove-from-alist)
(define-modify-macro delete-from-alistf (&rest keys) remove-from-alist)
(define-modify-macro remove-from-alistf (&rest keys) remove-from-alist)

(defun remove-values-from-alist (alist &rest values)
  (remove-if
   (lambda (kv)
     (find (cdr kv) values :test *assoc-test*))
   alist))

(define-modify-macro delete-values-from-alist (&rest values) remove-values-from-alist)

(defun alist-plist (alist)
  (mapcan (lambda (kv)
            (list (intern (string-upcase (car kv)) :keyword)
                  (cdr kv)))
          alist))

(defun plist-alist (plist)
  (loop for (k v) on plist by #'cddr
        collect (cons (string-downcase k) v)))

(defun alist-hash (alist &key recursivep)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (kv alist hash)
      (setf (gethash (car kv) hash)
            (if (and recursivep (alistp (cdr kv)))
                (alist-hash (cdr kv) :recursivep t)
                (cdr kv))))))

(defun hash-alist (hash &key recursivep)
  (labels ((rec-conv (value)
             (typecase value
               (hash-table (hash-alist value :recursivep t))
               (cons (mapcar #'rec-conv value))
               (otherwise value))))
    (loop for key being the hash-keys of hash
            using (hash-value value)
          if recursivep
            collect (cons key
                          (rec-conv value))
          else
            collect (cons key value))))

(defun alist-keys (alist)
  (mapcar #'car alist))

(defun alist-values (alist)
  (mapcar #'cdr alist))

(defun alistp (value)
  (or (null value)
      (and (consp value)
           (mapl (lambda (tree)
                   (unless (and (consp (first tree))
                                (not (consp (first (first tree))))
                                (listp (rest tree)))
                     (return-from alistp nil)))
                 value)
           t)))

(deftype alist () '(satisfies alistp))

(defun alist= (alist1 alist2)
  (check-type alist1 alist)
  (check-type alist2 alist)
  (equalp (sort (copy-seq alist1) #'string< :key #'car)
          (sort (copy-seq alist2) #'string< :key #'car)))
