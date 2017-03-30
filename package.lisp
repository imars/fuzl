;;;; package.lisp
;;;;
;;;; Copyright (c) 2016 Ian Marshall

(defpackage #:cl-fuzl
  (:use #:cl)
  (:export #:triangle
	   #:triangle-fuzzy
	   #:triangle-a
	   #:triangle-b
	   #:triangle-c
	   #:tri
	   #:tri-defuzzy-area
	   #:tri-fuzzy-left
	   #:tri-fuzzy
	   #:tri-fuzzy-right
	   #:defmfset
	   #:mfset
	   #:div
	   #:drawset
	   #:drawset%
	   #:stream-fuzzy-data
	   #:save-fuzzy-data
	   #:trace-graph
	   #:with-mfset
	   #:with-mfsets
	   #:reset-mfsets
	   #:&
	   #:v
	   #:!
	   #:->
	   #:defrules))

