;;;; cl-fuzl.lisp
;;;;
;;;; Copyright (c) 2016 Ian Marshall

(in-package :cl-fuzl)

#|
(defmacro remove-symbols (prefix members)
  `(progn
     (mapc (lambda (mem)
	     (let ((sym (find-symbol (concatenate 'string
						  (symbol-name ',prefix) "-"
						  (symbol-name mem)))))
	       (if sym (unintern sym))))
	   ',members)
     (let* ((pre (symbol-name ',prefix))
	    (make (find-symbol (concatenate 'string "MAKE-" pre)))
	    (fuzzify (find-symbol (concatenate 'string "FUZZIFY-" pre)))
	    (defuzzify (find-symbol (concatenate 'string "DEFUZZIFY-" pre))))
       (if make (unintern make))
       (if fuzzify (unintern fuzzify))
       (if defuzzify (unintern defuzzify)))))
|#

(defun div (nu de)
  (multiple-value-bind (q rem)
      (truncate (/ nu de))
    (declare (ignore rem))
    q))

(defun drawset (mfset &key (char #\*)
			(rows 10)
			(columns 80)
			(start 1)
			end)
  "Draw a textual picture of the passed triangle mfset."
  (let ((pix (make-array `(,rows ,(+ 1 columns))
			 :element-type 'character
			 :initial-element #\space)))
    (let* ((len (length mfset))
	   (lastmf (if end (aref mfset end)
		       (aref mfset (- len 1))))
	   (minu (or (aref (aref mfset start) 4)
		     (aref (aref mfset start) 5)))
	   (maxu (or (aref lastmf (- (length lastmf) 1))
		     (aref lastmf (- (length lastmf) 2))))
	   (ud (- maxu minu))
	   (pw (/ ud columns))
	   (negf (abs (div minu pw))))
      
      (setq end (or end (- len 1)))  
      (flet ((plot (y x)
	       ;(print (list y x))
	       (if (and (> x 0) (< x columns) (< y rows))
		   (setf (aref pix (- (- rows 1) y) x) char)))
	     (pixelx (rx)
	       (+ (div rx pw) negf)))
	(loop :for mfi :upfrom start :to end
	   :do
	   (let ((mf (aref mfset mfi)))
	     (dotimes (side 2)
	       (let* ((x1 (pixelx
			   (if (eql 0 side)
			       (or (triangle-a mf)
				   (- (triangle-b mf) 0.0000001d0))
			       (or (triangle-c mf)
				   (+ (triangle-b mf) 0.0000001d0)))))
		      (x2 (pixelx (triangle-b mf)))
		      (y1 0)
		      (y2 (- rows 1))
		      (dist-x (abs (- x1 x2)))
		      (dist-y (abs (- y1 y2)))
		      (steep (> dist-y dist-x)))
		 (when steep
		   (psetf x1 y1 y1 x1
			  x2 y2 y2 x2))
		 (when (> x1 x2)
		   (psetf x1 x2 x2 x1
			  y1 y2 y2 y1))
		 (let* ((delta-x (abs (- x2 x1)))
			(delta-y (abs (- y1 y2)))
			(error (floor delta-x 2))
			(y-step (if (< y1 y2) 1 -1))
			(y y1))
		   (loop 
		      :for x :upfrom x1 :to x2
		      :do (if steep
			      (plot x y)
			      (plot y x))
		      (setf error (- error delta-y))
		      (when (< error 0)
			(incf y y-step)
			(incf error delta-x))))))))))
    (dotimes (row rows)
      (let ((raster (make-string-output-stream)))
	(dotimes (col columns)
	  (write-char (aref pix row col) raster))
	(print (get-output-stream-string raster))))))

(defun drawset% (mfset &key (char #\*)
			(rows 10)
			(columns 80)
			(start 1)
			end)
  "Draw a textual picture of the passed triangle mfset."
  (let ((pix (make-array `(,rows ,(+ 1 columns))
			 :element-type 'character
			 :initial-element #\space)))
    (let* ();(len (length mfset))
	   ;(lastmf (if end (aref mfset end)
	;	       (aref mfset (- len 1))))
	 ;  (minu (or (aref (aref mfset start) 4)
	;	     (aref (aref mfset start) 5)))
	   ;(maxu (or (aref lastmf (- (length lastmf) 1))
	;	     (aref lastmf (- (length lastmf) 2))))
	   ;(ud (- maxu minu))
	   ;(pw (/ ud columns)))
	   ;(negf (abs (div minu pw))))
      (flet ((plot (x y)
	       (print (list x y))
	       (if (and (> x 0) (< x columns) (> y rows))
		   (setf (aref pix (- (- rows 1) y) x) char))))
	
	))
    (dotimes (row rows)
      (let ((raster (make-string-output-stream)))
	(dotimes (col columns)
	  (write-char (aref pix row col) raster))
	(print (get-output-stream-string raster))))))



(defun trace-graph (&key
		      (minu -30.0)
		      (maxu 30.0)
		      (chars '(#\*))
		      (columns 80)
		      values)
  (let ((pix (make-array `(,columns)
			 :element-type 'character
			 :initial-element #\space)))
    (let* ((ud (- maxu minu))
	   (pw (/ ud columns))
	   (negf (abs (div minu pw)))
	   (i 0))
      (dolist (x values)
	(let ((p (+ (div x pw) negf)))
	  (if (and (> p 0.0d0)(< p columns))
	      (setf (aref pix p) (nth i chars)))
	  (incf i))))			
   (let ((raster (make-string-output-stream)))
     (dotimes (col columns)
       (write-char (aref pix col) raster))
     (print (get-output-stream-string raster)))))

(let ((fuzzy-stream (make-string-output-stream)))
  (defun stream-fuzzy-data (&rest mfsets)
    "Save passed mfset fuzzy values to a stream."
    (dolist (mfset mfsets)
      (if (not (numberp mfset))
	  (dotimes (mf (- (length mfset) 1))
	    (format fuzzy-stream "~,20f" (mfun-fuzzy (aref mfset (+ mf 1))))
	    (write-char #\Tab fuzzy-stream))
	  (progn
	    (format fuzzy-stream "~,20f" mfset)
	    (write-char #\Tab fuzzy-stream))))
    (terpri fuzzy-stream))
  (defun stream-mfset-data (&rest mfsets)
    "Save passed mfset fuzzy values to a stream."
    (let ((mfun nil))
      (dolist (mfset mfsets)
	(dotimes (mf (- (length mfset) 1))
	  (setq mfun (aref mfset (+ mf 1)))
	  (dotimes (i (- (length mfun) 1))
	    (princ (aref mfun i) fuzzy-stream)
	    (prin1 " " fuzzy-stream)))))
    (terpri fuzzy-stream))
  (defun save-fuzzy-data (members filename &optional (stream fuzzy-stream))
    (with-open-file (s (merge-pathnames filename)
		       :direction :output
		       :if-exists :supersede)
      (if members
	  (let ((heading-stream (make-string-output-stream)))
	    (dolist (mem members)
	      (if (not (stringp mem))
		  (progn
		    (dolist (mfhead (funcall mem))
		      (prin1 (car mfhead) heading-stream)
		      (write-char #\Tab heading-stream)))
		  (progn
		    (princ mem heading-stream)
		    (write-char #\Tab heading-stream))))
	    (princ (get-output-stream-string heading-stream) s)
	    (terpri s)))
      (princ (get-output-stream-string stream) s))
    (values)))

(defun slidemf (a b c by times)
	  (let ((mfs ()))
	    (dotimes (i times)
	      (push (list 'tri
			  (concatenate 'string "strong+"
				       (write-to-string i))
			  (+ a (* i by)) (+ b (* i by)) (+ c (* i by))) mfs))
	    (reverse mfs)))

(defun slidemf- (a b c by times)
	  (let ((mfs ()))
	    (dotimes (i times)
	      (push (list 'tri
			  (concatenate 'string "strong+"
				       (write-to-string i))
			  (- a (* i by)) (- b (* i by)) (- c (* i by))) mfs))
	    (reverse mfs)))
