; -*- mode:common-lisp -*-

(in-package #:asdf)

(defsystem :cl-wepay
  :name "cl-wepay"
  :description "A common lisp interface to the WePay API"
  :author "Marc Battyani"
  :components ((:file "package")
               (:file "specials" :depends-on ("package"))
	       (:file "wepay" :depends-on ("specials"))
               (:file "wepay-api" :depends-on ("wepay")))
  :depends-on (:drakma :cl-html-parse :cl-json :named-readtables :optima :fare-quasiquote-optima))
