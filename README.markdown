# Assoc-Utils

Utilities for manipulating association lists.

## Usage

### aget

```common-lisp
(defvar *person*
  '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")))

(aget *person* "name")
;=> "Eitaro"

(aget *person* "address")
;=> NIL

(aget *person* "address" "Tokyo, Japan")
;=> "Tokyo, Japan"

(setf (aget *person* "name") "Eitaro Fukamachi")

*person*
;=> (("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))
```

### remove-from-alist & delete-from-alist

```common-lisp
(defvar *person*
  '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")))

(remove-from-alist *person* "name")
;=> (("email" . "e.arrows@gmail.com"))

;; Destructive version
(delete-from-alist *person* "name")
;=> (("email" . "e.arrows@gmail.com"))
```

### alist-plist & plist-alist

```common-lisp
(defvar *person*
  '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")))

(alist-plist *person*)
;=> (:NAME "Eitaro" :EMAIL "e.arrows@gmail.com")

(plist-alist '(:name "Eitaro" :email "e.arrows@gmail.com"))
;=> (("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))
```

### alist-keys & alist-values

```common-lisp
(defvar *person*
  '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")))

(alist-keys *person*)
;=> ("name" "email")

(alist-values *person*)
;=> ("Eitaro" "e.arrows@gmail.com")
```

### alistp

```common-lisp
(alistp '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")))
;=> T

(alistp 1)
;=> NIL

(alistp nil)
;=> T

;; Type: alist is also available
(typep '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")) 'alist)
;=> T
```

## Installation

```common-lisp
(ql:quickload :assoc-utils)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the Public Domain License.
