;;;; cl-fuzl.lisp
;;;;
;;;; Copyright (c) 2016 Ian Marshall

(in-package #:cl-fuzl)
(defparameter *debug* t)
#|
;; Example:

(defparameter in
  (mfset inner 
	 (tri neg nil -5.0 0.0)
	 (tri zero -5.0 0.0 5.0)
	 (tri pos 0.0 5.0 nil)))
(defparameter out 
  (mfset outer
	 (tri small 0.0 1.0 5.0)
	 (tri medium 1.0 5.0 10.0)
	 (tri large 5.0 10.0 30.0)))

(fuzzify-inner 3.3 in)
(fuzzify-outer 13.0 out)
(defuzzify-outer out)

(with-mfset in inner
	    (print (list inner.neg
			 inner.zero
			 inner.small)))

(with-mfsets ((in inner)
	      (out outer))
  (print inner.zero)
  (print outer.medium))

(with-mfsets ((in inner)
	      (out outer))()
	      (fuzzify-inner -3.3 in)
	      (-> (& in.neg in.zero) out.small
		  (v in.pos in.zero) out.medium)
	      (defuzzify-outer out))
|#

(defmacro dash (a b &optional (sep "-"))
  `(with-input-from-string
       (s (concatenate 'string ,a ,sep ,b))
     (read s)))

;; Membership function
(defstruct (mfun (:type vector)
		 :named
		 (:constructor mf()))
  (fuzzy 0.0d0))

;; TNorm structure and functions
(defstruct (triangle
	     (:include mfun) 
	     (:type vector)
	     :named
	     (:constructor tri (a b c)))
  ;;(f 0.0d0)
  a b c)
#|
(f 0.0d0 :type double-float)
(a 0.0d0 :type double-float)
(b 0.0d0 :type double-float)
(c 0.0d0 :type double-float))
|#
(defstruct (trapezoid
	     (:include mfun)
	     (:type vector)
	     (:constructor trap (a b c d)))
  a b c d)
(defstruct (gaussian
	     (:include mfun)
	     (:type vector)
	     (:constructor gauss (a c)))
  c a)

;; TNorm math
(declaim (inline tri-fuzzy-left))
(defun tri-fuzzy-left (u tri)
  (declare (inline triangle-b triangle-c triangle-fuzzy))
  (proclaim '(inline tri-fuzzy-left))
  (setf (triangle-fuzzy tri)
	(cond
	  ((<= u (triangle-b tri)) 1.0d0)
	  ((>= u (triangle-c tri)) 0.0d0)
	  (t (/ (- (triangle-c tri) u)(- (triangle-c tri) (triangle-b tri)))))))
(declaim (notinline tri-fuzzy-left))
(declaim (inline tri-fuzzy))
(defun tri-fuzzy (u tri)
  (declare (inline triangle-a triangle-b triangle-c triangle-fuzzy))
  (proclaim '(inline tri-fuzzy))
  (setf (triangle-fuzzy tri)
	(cond  
	  ((<= u (triangle-a tri)) 0.0d0)
	  ((>= u (triangle-c tri)) 0.0d0)
	  ((>= u (triangle-b tri)) 
	   (/ (- (triangle-c tri) u)(- (triangle-c tri) (triangle-b tri))))
	  (t (/ (- u (triangle-a tri))(- (triangle-b tri) (triangle-a tri)))))))
(declaim (notinline tri-fuzzy-right))
(declaim (inline tri-fuzzy-right))
(defun tri-fuzzy-right (u tri)
  (declare (inline triangle-a triangle-b triangle-fuzzy))
  (proclaim '(inline tri-fuzzy-right))
  (setf (triangle-fuzzy tri)
	(cond
	  ((<= u (triangle-a tri)) 0.0d0)
	  ((>= u (triangle-b tri)) 1.0d0)
	  (t (/ (- u (triangle-a tri))(- (triangle-b tri) (triangle-a tri)))))))
(declaim (notinline tri-fuzzy-right))
(declaim (inline tri-defuzzy-area))
(defun tri-defuzzy-area (tri)
  (declare (inline triangle-a triangle-b triangle-c triangle-fuzzy))
  (proclaim '(inline tri-defuzzy-area))
  (sb-int:with-float-traps-masked (:inexact)
    (let ((defuz
	   (+ (* (/ (- (triangle-b tri)
		       (triangle-a tri)) 2.0d0)
		 (triangle-fuzzy tri))
	      (* (/ (- (triangle-c tri)
		       (triangle-b tri)) 2.0d0)
		 (triangle-fuzzy tri)))))
      (setf (mfun-fuzzy tri) 0.0d0)
      defuz)))
(declaim (notinline tri-defuzzy-area))

(defun fuzzifier (name ud members)
  "Build named triangle fuzzifers over a particular universe of discourse."
  (let ((funi 0))
    (mapcar (lambda (mem)
	      (let* ((mf (cadr mem))
		     (mf-type (car mf)))
		(incf funi)
		(cond
		  ((null (cadr mf))
		   `(,(dash (symbol-name mf-type)
			    "fuzzy-left") ,ud (aref ,name ,funi)))
		  ((null (car (last mf)))
		   `(,(dash (symbol-name mf-type)
			    "fuzzy-right") ,ud
		      (aref ,name ,funi)))
		  (t
		   `(,(dash (symbol-name mf-type)
			    "fuzzy") ,ud (aref ,name ,funi))))))
	    members)))

(defun defuzzifier (name members)
  "Build a named triangle set defuzzifer 
from a description of its member functions."
  (let ((numlet ())
	(numerator ())
	(denominator ())
	(defuni 1)
	(denom (gensym)))
    (mapc (lambda (mem)
	    (let* ((mf (cadr mem))
		   (mf-type (car mf)))
	      (push `(,(dash "m" (write-to-string defuni))
		       (,(dash (symbol-name mf-type)
			       "defuzzy-area")
			 (aref ,name ,defuni))) numlet)
	      (push `(* (triangle-b (aref ,name ,defuni))
			,(dash "m" (write-to-string defuni)))
		    numerator)
	      (push (dash "m" (write-to-string defuni))
		    denominator)
	      (incf defuni)))
	  members)
    (push '+ numerator)
    (push '+ denominator)
    `(let* (,@numlet
	    (,denom ,denominator))
       (if (> ,denom 0.0)
	   (/ ,numerator ,denom)))))

(defun struct-compiler-macros (struct-name members)
  "Build compiler macros so that membership functions can be inlined 
   when expanded in defmacro calls."
  (let ((ref-index 0))
    (mapcar (lambda (mem)
	      (incf ref-index)
	      `(define-compiler-macro
		   ,(dash (symbol-name struct-name)
			  (symbol-name (car mem))) (struct)
		 `(aref ,struct ,,ref-index)))
	    members)))

(defmacro mfset (name &rest mfs)
  "Create a membership function set."
  (let* ((ud (gensym))
	 (members 
	  (sort (mapcar (lambda (mf)
			  (list (second mf) `(,(first mf) ,@(cddr mf))))
			mfs)
		(lambda (l r)
		  (if (or (null l) (null r)) t
		      (< l r)))
		:key (lambda (x)
		       (third (second x)))))
	 (mfuns (mapcar (lambda (mf)
			  (dash (symbol-name name)
				(symbol-name (car mf)))) members))
	 (fuzzify (fuzzifier name ud members))
	 (defuzzify (defuzzifier name members)))
    (declare (ignorable mfuns))
    (let ((maker (dash "make" (symbol-name name))))
      `(progn
	 (locally
	     (declare #+sbcl(sb-ext:muffle-conditions
			     sb-kernel:redefinition-warning))
	   (handler-bind
	       (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
	     ;;(declaim (inline ,@mfuns))
	     ,@(struct-compiler-macros name members)
	     (defstruct (,name (:type vector) :named)
	       ,@members)
	     ;;(proclaim '(inline ,@mfuns))
	     (defun ,(dash "members" (symbol-name name)) ()
	       ',members)
	     (defun ,(dash "fuzzify" (symbol-name name)) (,ud ,name)
	       ;;(declare (inline ,@mfuns))
	       ,@fuzzify
	       ,name)
	     (defun ,(dash "defuzzify" (symbol-name name)) (,name)
	       ;;(declare (inline ,@mfuns))
	       ,defuzzify)
	     (sb-ext:gc)
	     (,maker)))))))

(defmacro defmfset (name &rest mfs)
  "Define  a membership function set."
  (let* ((ud (gensym))
	 (members 
	  (sort (mapcar (lambda (mf)
			  (list (second mf) `(,(first mf) ,@(cddr mf))))
			mfs)
		(lambda (l r)
		  (if (or (null l) (null r)) t
		      (< l r)))
		:key (lambda (x)
		       (third (second x)))))
	 (mfuns (mapcar (lambda (mf)
			  (dash (symbol-name name)
				(symbol-name (car mf)))) members))
	 (fuzzify (fuzzifier name ud members))
	 (defuzzify (defuzzifier name members)))
    (declare (ignorable mfuns))
    `(progn
	 (locally
	     (declare #+sbcl(sb-ext:muffle-conditions
			     sb-kernel:redefinition-warning))
	   (handler-bind
	       (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
	     ;;(declaim (inline ,@mfuns))
	     ,@(struct-compiler-macros name members)
	     (defstruct (,name (:type vector) :named)
	       ,@members)
	     ;;(proclaim '(inline ,@mfuns))
	     (defun ,(dash "members" (symbol-name name)) ()
	       ',members)
	     (defun ,(dash "fuzzify" (symbol-name name)) (,ud ,name)
	       ;;(declare (inline ,@mfuns))
	       ,@fuzzify
	       ,name)
	     (defun ,(dash "defuzzify" (symbol-name name)) (,name)
	       ;;(declare (inline ,@mfuns))
	       ,defuzzify))))))

(defun reset-mfsets (&rest mfsets)
  "Set all fuzzy values of passed mfset structures to 0.0d0."
  (dolist (mfset mfsets)
    (dotimes (mf (- (length mfset) 1))
      (setf (mfun-fuzzy (aref mfset (+ mf 1))) 0.0d0))))

#|
;; Example
(with-mfset in inner
	    (print inner.zero))
|#
(defun mfs-let (mfsetsym mfs-type)
  (let* ((mfstype (symbol-name mfs-type))
	 (mfuns (funcall (dash "members" mfstype)))
	 (mems (mapcar (lambda (mf)
			 `(,(dash mfstype (symbol-name (car mf)) ".")
			    (,(dash mfstype
				    (symbol-name (car mf))) ,mfsetsym)))
		       mfuns)))
    mems))
(defmacro with-mfset (mfset mfs-type &rest body)
  (let ((mfs (gensym)))
    `(let ((,mfs ,mfset))
       (symbol-macrolet ,(mfs-let mfs mfs-type)
	 ,@body))))

#|
;; Example

(with-mfsets ((in inner)
	      (out outer))
  (print inner.zero)
  (print outer.medium))
|#

;; AND over fuzzified predicates
(defmacro & (&rest pred)
  `(min ,@pred))

;; OR over fuzzified predicates
(defmacro v (&rest pred)
  `(max ,@pred))

;; NOT for a single predicate
(defmacro ! (pred)
  `(- 1 ,pred))

(defun compile-rules (name
		      optimized-assignment-table
		      optimized-if-table
		      predicate-lets)
  (let ((compiled-rules (make-hash-table :test 'equal))
	(compiled-code ()))

    ;; Compile and layout code
    (if *debug* (format t "~%Compiling rule assignments."))
    (maphash (lambda (key value)
	       (let ((opt-pred (gethash value optimized-if-table)))
		 (if opt-pred
		     (setf (gethash key compiled-rules) opt-pred))))
	     optimized-assignment-table)
    (if *debug* (format t "~%Compiling rule code."))
    (unwind-protect
	 (maphash (lambda (key value)
		    (push `(if (> ,value 0.0d0)
			       (setq ,key ,value))
			  compiled-code))
		  compiled-rules)
      (progn
	(if *debug* (format t "~%~a completed." name))
	(clrhash compiled-rules)
	(setq compiled-rules nil)))
    (sb-ext:gc)
    (values
     (append `(let* )
	     `(,predicate-lets)
	     compiled-code)
     predicate-lets
     compiled-code)))

(defun rules (prans &optional name)
  (if name
      (format t "~%~%Compiling ~a rules." name))  
  (let ((precompiled-rules (make-hash-table :test 'equal))
	(optimized-preds (make-hash-table :test 'equal))
	(optimized-assignment (make-hash-table :test 'equal))
	(optimized-if (make-hash-table :test 'equal)))
	;;(compiled-rules (make-hash-table :test 'equal)))
	;;(compiled-code ()))
    (if *debug* (format t "~%Precompiling predicates."))
    (dotimes (p (/ (length prans) 2)) ;; Pre-compile predicates
      (let ((pred (nth (* p 2) prans))
	    (ante (nth (+ 1 (* p 2)) prans)))
	(if (not (gethash ante precompiled-rules))
	    (setf (gethash ante precompiled-rules)
		  `(,pred))
	    (let ((preds (gethash ante precompiled-rules)))
	      (push `,pred preds)
	      (setf (gethash ante precompiled-rules) preds))))
      (sb-ext:gc))
    (if *debug* (format t "~%Optimizing predicates"))
    (maphash (lambda (key value) ;; Optimize predicates
	       (declare (ignore key))
	       (if (or (> (length value) 1) (and (listp (car value))
						 (> (length (car value)) 1)))
		   (setf (gethash value optimized-preds) (gensym)))) 
	     precompiled-rules)
    (if *debug* (format t "~%Optimizing assignments."))
    (flet ((optimize-assignment (ant op pred)
	     (if (not (gethash ant optimized-assignment))
		 (setf (gethash ant optimized-assignment)
		       `(,(or op (car pred))))
		 (let ((ante (gethash ant optimized-assignment)))
		   (setf (gethash ant optimized-assignment)
			 (append ante `(,(or op (car pred)))))))))
      (maphash (lambda (key value) ;; Compile optimized assigments
		 (let ((opt-pred (gethash value optimized-preds)))
		   (if (and (not (atom key)) (> (length key) 1))
		       (mapc (lambda (ant)
			       (optimize-assignment ant opt-pred value))
			     key)
		       (optimize-assignment key opt-pred value))))
	       precompiled-rules))
    (clrhash precompiled-rules)
    (setq precompiled-rules nil)
    (maphash (lambda (key value)
	       (declare (ignore key))
	       (if (not (gethash value optimized-if))
		   (setf (gethash value optimized-if) (gensym))))
	     optimized-assignment)
    (let ((pred-lets ()))

      ;; Layout optimized predicates  
      (if *debug* (format t "~%Laying out predicates."))
      (maphash (lambda (key value)
		 (if (> (length key) 1)
		     (push `(,value (max ,@key)) pred-lets)
		     (push `(,value ,@key) pred-lets)))
	       optimized-if)
      (maphash (lambda (key value)
		 (if (> (length key) 1)
		     (push `(,value (max ,@key)) pred-lets)
		     (push `(,value ,@key) pred-lets)))
	       optimized-preds)
      (clrhash optimized-preds)
      (setq optimized-preds nil)

      (multiple-value-bind (ccc pl cc)
	  (compile-rules name optimized-assignment optimized-if pred-lets)
	(values ccc pl cc))

      #|
      ;; Compile and layout code
      (if *debug* (format t "~%Compiling rule assignments."))
      (maphash (lambda (key value)
		 (let ((opt-pred (gethash value optimized-if)))
		   (if opt-pred
		       (setf (gethash key compiled-rules) opt-pred))))
	       optimized-assignment)
      (clrhash optimized-assignment) 
      (setq optimized-assignment nil)
      (clrhash optimized-if)
      (setq optimized-if nil)      
      (if *debug* (format t "~%Compiling rule code."))
      (unwind-protect
	   (maphash (lambda (key value)
		      (push `(if (> ,value 0.0d0)
				 (setq ,key ,value))
			    compiled-code))
		    compiled-rules)
	(progn
	  (if *debug* (format t "~%~a completed." name))
	  (clrhash compiled-rules)
	  (setq compiled-rules nil)))
      (sb-ext:gc)
      (values
       (append `(let* )
	       `(,pred-lets)
	       compiled-code)
       pred-lets
       compiled-code)
      |#
      )))

;;(defmacro -> (&rest preds-antes)
;;  `(let ()
;;     ,(rules `,preds-antes)))

(defmacro -> (&rest preds-antes)
  (multiple-value-bind (if-code opt-set compile-code)
      (rules preds-antes)
    (declare (ignore opt-set compile-code))
    `(let ()
       ,if-code)))


#|
(macroexpand '(-> pr.front (thrustl1.more thrustr1.more
			    thrustl2.less thrustr2.less)
	       pr.zero (thrustl1.hover thrustr2.hover
			thrustl2.hover thrustr1.hover)
	       pr.zero (thrustl1.descend thrustr2.descend
			thrustl2.descend thrustr1.descend)
	       (& pr.back rr.left) (thrustl1.less thrustr2.less
				    thrustl2.less thrustr2.less)
	       (v pr.back rr.zero) (thrustl1.less thrustr2.less
				    thrustl2.less thrustr2.less)
	       rr.left (thrustl1.more thrustl2.more)
	       rr.zero (thrustl1.hover thrustl2.hover)
	       rr.right (thrustl1.less thrustl2.less)
	       (& rr.right rr.zero) (thrustl1.less thrustl2.less)
	       pr.zero thrust1l.hover))

;; Example
(with-mfsets ((in inner)
	      (out outer))()
	      (fuzzify-inner -3.3 in)
	      (-> (& inner.neg inner.zero) outer.small
		  (v inner.pos inner.zero) outer.medium)
	      (defuzzify-outer out))(with-rules ((in inner)
						 (out outer))()
|#
(defun mf-macros (members mfstype sym var &key (key 'triangle-fuzzy))
  (mapcar (lambda (mf)
	    `(,(dash var (symbol-name (car mf)) ".")
	       (,key
		(,(dash mfstype
			(symbol-name (car mf))) ,sym))))
	  members))
(defun mf-lets (mfsets &key (key 'triangle-fuzzy))
  (let* ((macro-lets ())
	 (var-lets
	  (mapcar (lambda (mfs)
		    (let* ((set-sym (gensym))
			   (var (car mfs))
			   (mfstype (symbol-name (second mfs)))
			   (funs (funcall (dash "members" mfstype)))
			   (mfs (mf-macros funs mfstype set-sym
					   (symbol-name var) :key key)))
		      (mapc (lambda (mf)
			      (push mf macro-lets)) mfs)
		      `(,set-sym ,var)))
		  mfsets))
	 (mfuns
	  (mapcar (lambda (mf)
		    (car (cadr (cadr mf))))
		  (reverse macro-lets))))
    (values var-lets macro-lets mfuns)))
(defmacro with-mfsets (mfsets &rest body)
  (multiple-value-bind (vars macros mfuns)
      (mf-lets mfsets :key 'triangle-fuzzy)
    (declare (ignore mfuns))
    `(let ,vars
       (declare (ignorable ,@(mapcar #'car vars)))
       ;;(declare (inline triangle-f ,@mfuns)
       ;;	(ignorable ,@(mapcar #'car vars)))
       (symbol-macrolet ,macros
	 ,@body))))

(defmacro defrules (name mfsets &rest body)
  (multiple-value-bind (vars macros mfuns)
      (mf-lets mfsets :key 'triangle-fuzzy)
    (declare (ignore mfuns))
    (multiple-value-bind (if-code opt-set compiled-code)
	(rules body name)
      (declare (ignore if-code))
      `(let ,vars
	 (declare (ignorable ,@(mapcar #'car vars)))
	 (symbol-macrolet ,macros
	   (let (,@(mapcar (lambda (sym)
			     `(,(car sym) 0.0d0))
			   opt-set))
	     (defun ,name ()
	       ,@(mapcar (lambda (opts)
			   `(setq ,@opts))
			 opt-set)
	       ,@compiled-code)))))))
	     
