;;; -*- Mode:LISP;  Package: OBJ; Base:10; Readtable:CL; Syntax: Common-Lisp -*-
;;;
;;; ******************************************************************************
;;; Copyright (c) 1984, 1985 Gary L. Drescher.  All rights reserved.
;;; Licensed to and distributed by Lisp Machine, Inc.
;;;
;;; Use and copying of this software and preparation of derivative works based
;;; upon this software are permitted.  Any distribution of this software or
;;; derivative works must comply with all applicable United States export control
;;; laws.
;;;
;;; This software is made available AS IS, and no warranty is made about the
;;; software, its performance, or its conformity to any specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send her name
;;; and post office or electronic mail address to:
;;;   ObjectLISP Coordinator
;;;   c/o User Interface Group
;;;   Lisp Machine, Inc.
;;;   1000 Massachusetts Ave.
;;;   Cambridge, Ma 02138
;;;
;;; Suggestions, comments, and requests for improvements are also welcome.
;;; ******************************************************************************

(in-package "OBJ")

#+:SBCL (IMPORT 'SB-CLTL2:COMPILER-LET)
#+CCL (IMPORT 'CCL:COMPILER-LET)

(defmacro stickon (thing list list-end)
  `(if (null ,list-end)
       (setq ,list (setq ,list-end (ncons ,thing)))
       (rplacd ,list-end (setq ,list-end (ncons ,thing)))))

(defmacro stickon* (new-list list list-end)
  `(let ((new ,new-list))
     (unless (null new)
       (if (null ,list-end)
	   (setq ,list new)
	   (rplacd ,list-end new))
       (setq ,list-end (last new)))))

(defvar *nloop-collections)

(defmacro collect (form &optional (collection 'standard-nloop-collection))
  `(stickon ,form ,collection ,(cdr (assq collection *nloop-collections))))

(defmacro collect* (form &optional (collection 'standard-nloop-collection))
  `(stickon* ,form ,collection ,(cdr (assq collection *nloop-collections))))

;(collecting vars)
;(with var init)
;(for var init repeat)
;(for-on var list)
;(for-in var list)
;(collect form collection)
;(collect* form collection)
;(finally forms)
;(stop)
;(stop-if test)
;(stop-unless test)
;(inc-til var start limit inc)
;(inc-nottil var start limit inc)
;(dec-til var start limit inc)
;(dec-nottil var start limit inc)

;(defun foo (list)
;  (nloop (for-on l1 list) (for x (car l1)) (print x) (collect (1+ x))))

(defmacro nloop (&body body)
  (let (finally finally? collections aux-collections
	clauses clauses-end vars vars-end)
   (flet ((return-form ()	; Return >without< bypassing finally stuff.
	    (if (null finally?) '(return nil) '(return-from do-nloop t))))
    (do ((body body)
	 (clause (car body) (car body)))
	((null body))
      (pop body)
      (case (car clause)
	((finally collecting collect collect*)
	 (setq finally? t)
	 (return))))
    (do ((clause (car body) (car body)))
	((null body)
	 (setq aux-collections (mapcar #'(lambda (ignore)
					   (declare (ignore ignore))
					   (gensym))
				       collections))
	 (when (memq 'standard-nloop-collection collections)
	   (if (null finally)
	       (setq finally (ncons 'standard-nloop-collection))
	       (rplacd (last finally) (ncons 'standard-nloop-collection))))
	 `(compiler-let ((*nloop-collections
			   (mapcar #'(lambda (var aux) (cons var aux))
				   ',collections ',aux-collections)))
	    (let (,@collections ,@aux-collections)
	      ,(if (null finally?)
		   `(do* ,vars (nil)
		      ,@clauses)
		   `(let* ,vars
		      (do () ((block do-nloop ,@clauses nil)
			      ,@finally)
			))))))
      (pop body)
      (if (not (consp clause))
	  (stickon clause clauses clauses-end)
	  (case (car clause)
	    (collecting
	     (setq collections
		   (if (memq t (cdr clause))
		       `(standard-nloop-collection ,@(remq t (cdr clause)))
		       (cdr clause))))
	    (collect
	     (let ((var (or (caddr clause) 'standard-nloop-collection)))
	       (unless (memq var collections)
		 (push var collections)))
	     (stickon clause clauses clauses-end))
	    (collect*
	     (let ((var (or (caddr clause) 'standard-nloop-collection)))
	       (unless (memq var collections)
		 (push var collections)))
	     (stickon clause clauses clauses-end))
	    (finally
	      (setq finally (append (cdr clause) finally)))
	    (with
	     (stickon `(,(cadr clause) ,(caddr clause)) vars vars-end))
	    (for
	     (cond ((null (cdddr clause))
		    (stickon (cadr clause) vars vars-end)
		    (stickon `(setq ,(cadr clause) ,(caddr clause))
			     clauses clauses-end))
		   (t
		    (stickon `(,(cadr clause) 'nloop-uninit-token) vars vars-end)
		    (stickon `(if (eq ,(cadr clause) 'nloop-uninit-token)
				  (setq ,(cadr clause) ,(caddr clause))
				  (setq ,(cadr clause) ,(cadddr clause)))
			     clauses clauses-end))))
	    (for-in				; No check for duplicate popping!
	     (let ((listvar (gensym)))
	       (stickon `(,listvar ,(caddr clause)) vars vars-end)
	       (stickon (cadr clause) vars vars-end)
	       (stickon `(if (null ,listvar)
			     ,(return-form)
			     (setq ,(cadr clause) (pop ,listvar)))
			clauses clauses-end)))
	    (for-on				; No check for duplicate popping!
	     (let ((listvar (gensym)))
	       (stickon `(,listvar ,(caddr clause)) vars vars-end)
	       (stickon (cadr clause) vars vars-end)
	       (stickon `(setq ,(cadr clause) ,listvar) clauses clauses-end)
	       (stickon `(if (null ,listvar)
			     ,(return-form)
			     (setq ,listvar (cdr ,listvar)))
			clauses clauses-end)))
	    (stop-if
	     (stickon `(if ,(cadr clause)
			   ,(return-form))
		      clauses clauses-end))
	    (stop-unless
	     (stickon `(or ,(cadr clause)
			   ,(return-form))
		      clauses clauses-end))
	    (inc-til (pop clause)
	     (let ((var (pop clause))
		   (start (pop clause))
		   (limit (pop clause))
		   (inc (or (pop clause) 1))
		   (limitvar (gensym)) (incvar (gensym)))
	       (stickon `(,var nil) vars vars-end)
	       (stickon `(,limitvar ,limit) vars vars-end)
	       (stickon `(,incvar ,inc) vars vars-end)
	       (stickon `(cond ((null ,var) (setq ,var ,start))
			       ((< ,var ,limitvar) (incf ,var ,incvar))
			       (t ,(return-form)))
			clauses clauses-end)))
	    (inc-nottil (pop clause)
	     (let ((var (pop clause))
		   (start (pop clause))
		   (limit (pop clause))
		   (inc (or (pop clause) 1))
		   (limitvar (gensym)) (incvar (gensym)))
	       (stickon `(,var nil) vars vars-end)
	       (stickon `(,limitvar ,limit) vars vars-end)
	       (stickon `(,incvar ,inc) vars vars-end)
	       (stickon `(if (null ,var)
			     (setq ,var ,start)
			     (incf ,var ,incvar))
			clauses clauses-end)
	       (stickon `(if (not (< ,var ,limitvar))
			     ,(return-form))
			clauses clauses-end)))
	    (dec-til (pop clause)
	     (let ((var (pop clause))
		   (start (pop clause))
		   (limit (pop clause))
		   (inc (or (pop clause) 1))
		   (limitvar (gensym)) (incvar (gensym)))
	       (stickon `(,var nil) vars vars-end)
	       (stickon `(,limitvar ,limit) vars vars-end)
	       (stickon `(,incvar ,inc) vars vars-end)
	       (stickon `(cond ((null ,var) (setq ,var ,start))
			       ((> ,var ,limitvar) (decf ,var ,incvar))
			       (t ,(return-form)))
			clauses clauses-end)))
	    (dec-nottil (pop clause)
	     (let ((var (pop clause))
		   (start (pop clause))
		   (limit (pop clause))
		   (inc (or (pop clause) 1))
		   (limitvar (gensym)) (incvar (gensym)))
	       (stickon `(,var nil) vars vars-end)
	       (stickon `(,limitvar ,limit) vars vars-end)
	       (stickon `(,incvar ,inc) vars vars-end)
	       (stickon `(if (null ,var)
			     (setq ,var ,start)
			   (decf ,var ,incvar))
			clauses clauses-end)
	       (stickon `(if (not (> ,var ,limitvar))
			     ,(return-form))
			clauses clauses-end)))
	    (otherwise (stickon clause clauses clauses-end))))))))
