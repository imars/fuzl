;;;; cl-fuzl.asd
;;;;
;;;; Copyright (c) 2016 Ian Marshall

(asdf:defsystem #:cl-fuzl
  :description "Common Lisp Fuzzy Logic compiler."
  :author "Ian Marshall"
  :license ""
  :serial t
  :components ((:file "package")
	       (:file "utils")
               (:file "cl-fuzl")))

