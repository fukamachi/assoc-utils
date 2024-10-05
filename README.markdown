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

### alist-get

```common-lisp
(defvar *data*
  '((:VERSION . "0.6")
    (:GENERATOR . "openstreetmap-cgimap 2.0.1 (1992 spike-08.openstreetmap.org)")
    (:COPYRIGHT . "OpenStreetMap and contributors")
    (:ATTRIBUTION . "http://www.openstreetmap.org/copyright")
    (:LICENSE . "http://opendatacommons.org/licenses/odbl/1-0/")
    (:ELEMENTS
     ((:TYPE . "node") (:ID . 1) (:LAT . 42.79572) (:LON . 13.569003)
      (:TIMESTAMP . "2024-09-13T11:52:01Z") (:VERSION . 39)
      (:CHANGESET . 156568263) (:USER . "SomeoneElse_Revert") (:UID . 1778799)
      (:TAGS (:|COMMUNICATION:MICROWAVE| . "yes") (:|COMMUNICATION:RADIO| . "fm")
       (:DESCRIPTION . "Radio Subasio") (:FREQUENCY . "105.5 MHz")
       (:MAN--MADE . "mast") (:NAME . "Monte Piselli - San Giacomo")
       (:NOTE . "This is the very first node on OpenStreetMap.")
       (:|TOWER:CONSTRUCTION| . "lattice") (:|TOWER:TYPE| . "communication"))))))

(alist-get *data* '(:elements 0 :tags :note))
;=> "This is the very first node on OpenStreetMap."
```

### with-keys

The macro `with-keys` is the alist equivalent of [`with-slots`](https://novaspec.org/cl/f_with-slots).

```common-lisp
(with-keys
   ("name" (loc "location") (time "time" 2024))
    (list (cons "name" "eitaro") (cons "location" "vienna"))
  (declare (string name))
  (setf loc (string-upcase loc))
  (format nil "Hi, ~a in ~a around ~a!" name loc time))
;; => "Hi, eitaro in VIENNA around 2024!"
```

The first parameter is a list of keys that `with-keys` will reference in the alist
provided in the second parameter. `With-keys` will attempt to convert each
key into a symbol, binding the alist value to it during body execution.

If you don't want `with-keys` to guess the symbol for a key, supply a list -
`(symbol key)` - in place of the key, as in `(loc "location")` above.
If the key is a number, you have to supply a symbol name since common lisp
symbols can not consist of only numbers.

If you want to supply a default value, you have to supply a list -
`(symbol key default)` - in place of the key, as in `(time "time" 2024)`.

Code and documentation adapted from [cl-hash-util](https://github.com/orthecreedence/cl-hash-util).

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
