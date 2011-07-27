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

;;;; Obl interface to commonlisp codewalker

(defun walkover (form)
  (if (not *walkover?)
      form
      (walk-toplevel-form form #'var-ref-intercept #'var-set-intercept
			  #'application-intercept)))

(defun var-ref-intercept (sym)
  (cond ((eq sym '*shadows) sym)
	((eq (symbol-package sym) *obf-pkg)  sym)
	(t (qsym-ref-form sym))))

(defun var-set-intercept (sym val)
  (qsym-set-form sym val))

(defun application-intercept (tail? fcn-args &optional exceptions-only?
			      &aux (fcn (car fcn-args))  (args (cdr fcn-args)))
  tail?
  (labels ((replace-access-fcn (form)
	     (cdr (assq (cadr form)
			'((symbol-value #'get-sym-val) (set . #'set-sym-val)
			  (#-symbolics symeval
			   #+symbolics zl:symeval . #'get-sym-val)
			  (symbol-function #'get-sym-fcn) (fset . #'set-sym-fcn)
			  (#-symbolics fsymeval
			   #+symbolics zl:fsymeval . #'get-sym-fcn)))))
	   (special-case-access-fcn ()
	     (if (quoted-symbol? (car args))
		 (let ((new (replace-access-fcn (car args))))
		   (if new (setq args (cons new (cdr args))
				 fcn-args (cons fcn args)))))))
    (cond; ((eq fcn *shadowed-fcn-sym)
	 ;  (shadowed-funcall-form args))
	  ((symbolp fcn)
	   (case fcn
	     ((symbol-value #-symbolics symeval #+symbolics zl:symeval)
	      (symbol-ref-form (car args)))
	     ((symbol-function #-symbolics fsymeval #+symbolics zl:fsymeval)
	      (symbol-fref-form (car args)))
	     (set
	      (symbol-set-form (car args) (cadr args)))
	     (fset
	      (symbol-fset-form (car args) (cadr args)))
	     (FUNCTION
	      (or (replace-access-fcn fcn-args)
		  (IF (UNSHADOWABLE-FUNCALL-FORM? FCN-ARGS)
		      FCN-ARGS
		      (SUBST-LAMBDA-FOR-FCNQUOTE FCN-ARGS))))
	     (funcall
	      (special-case-access-fcn)
	      (if (unshadowable-funcall-form? (car args))
		  fcn-args
		  (fcncall-form 'funcall (car args) (cdr args))))
	     ((apply #+symbolics zl:apply)
	      (special-case-access-fcn)
	      (if (unshadowable-funcall-form? (car args))
		  fcn-args
		  (fcncall-form 'apply (car args) (cdr args))))
	     ((lexpr-funcall #+symbolics zl:lexpr-funcall)
	      (special-case-access-fcn)
	      (if (unshadowable-funcall-form? (car args))
		  fcn-args
		  (fcncall-form 'apply (car args) (cdr args))))
	     (call
	      (special-case-access-fcn)
	      (if (unshadowable-funcall-form? (car args))
		  fcn-args
		  (fcncall-form 'call (car args) (cdr args))))
	     (t (if (or exceptions-only? (unshadowable-fcncall-form? fcn))
		    fcn-args
		    (fcncall-form 'funcall (list 'quote fcn) args)))))
	  ((or exceptions-only? (unshadowable-fcncall-form? fcn))
	   fcn-args)
	  (t (fcncall-form 'funcall fcn args)))))

(defparameter *unshadowable-pkgs
	#+lambda
	`(,(pkg-find-package 'global)
	  ,(pkg-find-package 'system)
	  ,(pkg-find-package 'si)
	  ,(pkg-find-package 'dbg)
	  ,(pkg-find-package 'obj) ,(pkg-find-package 'obf)
	  ,(pkg-find-package 'compiler))
	#+symbolics
	`(,(pkg-find-package 'global)
	  ,(pkg-find-package 'common-lisp)
	  ,(pkg-find-package 'common-lisp-global)
	  ,(pkg-find-package 'system) ,(pkg-find-package 'zetalisp-system)
	  ,(pkg-find-package 'si)
	  ,(pkg-find-package 'dbg)
	  ,(pkg-find-package 'obj) ,(pkg-find-package 'obf)
	  ,(pkg-find-package 'compiler))
	#-(or lambda symbolics)
	`(,(pkg-find-package :cl)
	  ,(pkg-find-package 'obj) ,(pkg-find-package 'obf)))

(defparameter obj-pkg (pkg-find-package 'obj))
(defparameter obf-pkg (pkg-find-package 'obf))

(defun subst-lambda-for-fcnquote (form)
  `(function (lambda (&rest args) (apply ,form args))))

(defun unshadowable-funcall-form? (fcn-form)
  (unshadowable-aux fcn-form nil))

(defun unshadowable-fcncall-form? (fcn-form)
  (unshadowable-aux fcn-form t))

; FCNCALL? is nonnull iff the application was a normal (<fcn-form> . <args>),
;  rather than a (FUNCALL/APPLY/etc <fcn-form> ...).
; 1st return value is null iff FCN-FORM might possibly yield a shadowable
;  symbol at runtime.
; 2nd return value is the SYM to be FSYMEVAL'ed, if this can be determined now.
;  This is for use by the lambda/symbolics compiler-interceptor.
(defun unshadowable-aux (fcn-form fcncall? &aux sym)
  (values (or ;; Nonsymbol in fcncall's function position:
	      (and fcncall? (not (symbolp fcn-form)))
	      ;; Obl-interceptor, eg (FUNCALL (GET-SYM-FCN 'FOO) ...):
	      (and (consp fcn-form) (symbolp (car fcn-form))
		   (OR (eq (symbol-package (car fcn-form)) obj-pkg)
		       (EQ (SYMBOL-PACKAGE (CAR FCN-FORM)) OBF-PKG)))
	      ;; Constant lambda expression:
	      (and (consp fcn-form)
		   (or (eq (car fcn-form) 'lambda)
		       (and (memq (car fcn-form) '(function quote))
			    (consp (cadr fcn-form))
			    (eq (car (cadr fcn-form)) 'lambda))))
	      ;; Constant-symbol, unshadowable:
	      (progn
	       (cond (fcncall?
		      (setq sym fcn-form))
		     ((quoted-symbol? fcn-form)
		      (setq sym (cadr fcn-form))))
	       (and (not (null sym))
		    (pkg-unshadowable? sym))))
	  sym))

(defun quoted-symbol? (form)
  (and (consp form) (or (eq (car form) 'quote) (eq (car form) 'function))
       (consp (cdr form)) (symbolp (cadr form)) (null (cddr form))))

(defun pkg-unshadowable? (sym)
  (and (memq (symbol-package sym) *unshadowable-pkgs)
       (not (memq sym '(exist shadowed-exist print-self)))))
