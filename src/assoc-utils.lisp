(in-package :cl-user)
(defpackage assoc-utils
  (:use #:cl)
  (:export #:*assoc-test*
           #:aget
	   #:with-keys
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

(defmacro with-keys (keys alist &body body)
  "The macro `with-keys` is the alist equivalent of `with-slots`.

(with-keys
   (\"name\" (loc \"location\") (time \"time\" 2024))
    (list (cons \"name\" \"eitaro\") (cons \"location\" \"vienna\"))
  (declare (string name))
  (setf loc (string-upcase loc))
  (format nil \"Hi, ~a in ~a around ~a!\" name loc time))
;; => \"Hi, eitaro in VIENNA around 2024!\"

The first parameter is a list of keys that `with-keys` will reference in the alist
provided in the second parameter. `With-keys` will attempt to convert each
key into a symbol, binding the alist value to it during body execution.

If you don't want `with-keys` to guess the symbol for a key, supply a list -
`(symbol key)` - in place of the key, as in `(loc \"location\")` above.
If the key is a number, you have to supply a symbol name since common lisp
symbols can not consist of only numbers.

If you want to supply a default value, you have to supply a list -
`(symbol key default)` - in place of the key, as in `(time \"time\" 2024)`.
q
Code and documentation adapted from cl-hash-util."
  (let ((alist-symbol (gensym)))
    (list* (quote let*)
	   (list* (list alist-symbol alist)
		  (mapcar
		   (lambda (entry)
		     (let ((symbol (if (listp entry)
				       (car entry)
				       (intern (string-upcase (format nil "~A" entry)))))
			   (key (if (listp entry)
				    (second entry)
				    entry))
			   (default (when (listp entry)
				      (third entry))))
		       (list symbol (list (quote aget)
					  alist-symbol
					  (if (and (symbolp key) (not (keywordp key)))
					      (quote key)
					      key)
					  default))))
		   keys))
	   body)))

(defun remove-from-alist (alist &rest keys)
  (remove-if
   (lambda (kv)
     (find (car kv) keys :test *assoc-test*))
   alist))

(define-modify-macro delete-from-alist  (&rest keys) remove-from-alist)
(define-modify-macro delete-from-alistf (&rest keys) remove-from-alist)
(define-modify-macro remove-from-alistf (&rest keys) remove-from-alist)

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
