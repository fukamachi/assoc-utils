# Assoc-Utils

[![Build Status](https://travis-ci.org/fukamachi/assoc-utils.svg?branch=master)](https://travis-ci.org/fukamachi/assoc-utils)
[![Coverage Status](https://coveralls.io/repos/fukamachi/assoc-utils/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/assoc-utils)
[![Quicklisp dist](http://quickdocs.org/badge/assoc-utils.svg)](http://quickdocs.org/assoc-utils/)

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
;=> (("name" . "Eitaro Fukamachi") ("email" . "e.arrows@gmail.com"))
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

### remove-values-from-alist & delete-values-from-alist

```common-lisp
(defparameter *person*
  '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")))

(remove-values-from-alist *person* "Eitaro")
;=> (("email" . "e.arrows@gmail.com"))

*person*
;=> (("name" . "Eitaro") ("email" . "e.arrows@gmail.com")))

;; Destructive version
(delete-values-from-alist *person* "e.arrows@gmail.com")
;=> (("name" . "Eitaro"))

*person*
;=> (("name" . "Eitaro"))
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

### alist-hash & hash-alist

```common-lisp
(defvar *person*
  '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com")))

(alist-hash *person*)
;=> #<HASH-TABLE :TEST EQUAL :COUNT 2 {1004329443}>

(hash-alist *)
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

### alist=

```common-lisp
(alist= '(("name" . "Eitaro") ("email" . "e.arrows@gmail.com"))
        '(("email" . "e.arrows@gmail.com") ("name" . "Eitaro")))
;=> T
```

## Installation

```common-lisp
(ql:quickload :assoc-utils)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## License

Assoc-Utils is free and unencumbered software released into the public domain.
