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

;;;; Commonlisp codewalker
; Identifies and transforms free variable and function references.
; Ignores flow of control.

(defparameter *bound-vars nil)	; Locally bound variables, lexical & special.
(defparameter *bound-fcns nil)	; Locally bound functions (lexical).
(defparameter *specials nil)	; Locally special variables (from declarations).

(defparameter *free-var-ref-transform nil)
(defparameter *free-var-set-transform nil)
(defparameter *free-fcncall-transform nil)

(defun transform-free-var-ref (sym)
  (iff (null *free-var-ref-transform) sym (funcall *free-var-ref-transform sym)))

(defun transform-free-var-set (sym val)
 (iff (null *free-var-set-transform)
     (list 'setq sym val)
     (funcall *free-var-set-transform sym val)))

(defun transform-free-fcncall (tail? fcn-args)
 (iff (null *free-fcncall-transform)
     fcn-args
   (funcall *free-fcncall-transform tail? fcn-args)))

(defun free-var-ref? (sym)
 (not (memq sym *bound-vars)))

(defun free-fcn-ref? (sym args)
  (and (not (null sym))
       (not (memq sym *bound-fcns))
       ;; Not a funcall/apply/call of a lexically bound fcn-name:
       (not (and (memq sym '(funcall apply call))
		 (CONSP (CAR ARGS))
		 (MEMQ (CAR (CAR ARGS)) '(QUOTE FUNCTION))
		 (NOT (FREE-FCN-REF? (CADR (CAR ARGS)) (CDR ARGS)))))))

(defmacro push-local (sym locals)
  `(push ,sym ,locals))

(defmacro append-local (syms locals)
  `(setf ,locals (append ,syms ,locals)))

;ALWAYS Bind *BOUND-VARS and *SPECIALS around this.
(defun walk-any-dcls (body)
  (mapc #'(lambda (form)
	    (when (and (consp form) (eq (pop form) 'declare))
	      (mapc #'(lambda (dcl)
			(when (and (consp dcl) (eq (pop dcl) 'special))
			  (mapc #'(lambda (sym)
				    (when (symbolp sym)
				      (push sym *specials)))
				dcl)))
		    form)))
	body))

#|
progn block catch throw progv let let* flet if go tagbody setq labels the declare
quote return-from macrolet multiple-value-call multiple-value-prog1 unwind-protect

!!! macrolet

!!!(compiler-let ((x (foo y))  (y z)) x y z...)
-> (compiler-let ((x (foo (obl-sym y))) (y [z may or may not be special))
     (obl-sym x) (obl-sym y) [z may or may not be special])
but if z (init-form) is in the compiler-env, it >can't< be lexical;
 whereas if it's in the runtime-env, it >might< be.  different semantics for
 different times.

 presumption of compiletime may get global/obj variables at evaltime, bypassing
  lexical bindings.  this >might< be what's intended tho; would be more
  consistent.
 presumption of evaltime may get global variables at compiletime, bypassing
  compiletime object-bingings.  this is ok if we document it.
probly presumption of compiletime is better.

|#

(defparameter *obl-walking? nil)

(defun walk-toplevel-form (form &optional *free-var-ref-transform
			                  *free-var-set-transform
					  *free-fcncall-transform
			        &aux (*obl-walking? t))
  (walk-form t form))

(defparameter *walkers
  '((setq . walk-setq) (quote . walk-quote) (cond . walk-cond)
    (progv . walk-progv) (lambda . walk-lambda) (let . walk-let)
    (let* . walk-let*) (flet . walk-flet) (labels . walk-labels)
    (compiler-let . walk-compiler-let) (declare . walk-quote)
    (return-from . walk-return-from) (go . walk-quote)
    (function . walk-function) (progn . walk-tail) (block . walk-block)
    (catch . walk-notail) (throw . walk-notail) (if . walk-if)
    (and . walk-tail) (or . walk-tail) (multiple-value-call . walk-notail)
    (multiple-value-prog1 . walk-notail) (unwind-protect . walk-notail)
    (eval-when . walk-eval-when)  (DELAYED-EXPAND . walk-quote)
    (tagbody . walk-tagbody)
    #+symbolics (zl:eval-when . walk-eval-when)

    ;; Plus some extras (not commonlisp special forms):
    (do . walk-do) (do* . walk-do*)
    (defun . walk-defun) #+symbolics (zl:defun . walk-defun)
    (multiple-value-bind . walk-mvb)
    (multiple-value-setq . walk-mvsetq)
    (prog . walk-prog) (prog* . walk-prog*)
    (return . walk-tail)
    #+lambda (variable-boundp . walk-quote)
    #+lambda (variable-makunbound . walk-quote)
    #+lambda (variable-location . walk-quote)
    #+symbolics (zl:variable-boundp . walk-quote)
    #+symbolics (zl:variable-makunbound . walk-quote)
    #+symbolics (zl:variable-location . walk-quote)
    ))

(defun walk-form (tail? form &optional quote-atoms? &aux walker)
  ;; !!! Should catch errors.
  (cond ((constantp form) form)
	((and (symbolp form) (free-var-ref? form) (not quote-atoms?))
	 (transform-free-var-ref form))
	((symbolp form) form)
	((not (consp form)) form)
	(t (let ((fcn-form (car form))  (arg-forms (cdr form)))
	     (if (and (symbolp fcn-form)
		      (setq walker (cdr (assq fcn-form *walkers))))
		 (walk-special-form tail? form walker)
	         ;; Not special form.  Try macroexpand.
	         (nloop
		   (for old-form form)
		   (setq form (macroexpand form)
			 fcn-form (car form)  arg-forms (cdr form))
		   (if (and (eq form old-form)
			    ;; If it wasn't a macro, try user transform:
			    (symbolp fcn-form)
			    (free-fcn-ref? fcn-form arg-forms))
		       (setq form (transform-free-fcncall tail? form)))
		   ;; Keep expanding & transforming until it's stable.
		   (stop-if (eq old-form form))
		   (finally
		     (cond ((symbolp fcn-form)
			    (if (special-form-p fcn-form)
				(if (setq walker (cdr (assq fcn-form *walkers)))
				    (walk-special-form tail? form walker)
				    (error "~%Non-CommonLisp special form: ~s"
					   fcn-form))
			        (walk-fcncall tail? form)))
			   (t (cons (walk-form nil fcn-form)
				    (walk-forms nil arg-forms)))))))))))

(defun walk-special-form (tail? fcn-args walker)
  (funcall walker tail? fcn-args))

(defun walk-forms (tail? forms &optional quote-atoms?)
  (maplist #'(lambda (forms &aux (form (pop forms)))
	       (walk-form (and tail? (null forms))
			  form quote-atoms?))
	   forms))

(defun walk-setq (tail? form) tail?
  (cond ((null (cddr form)) form)
	((null (cdddr form))
	 (iff (free-var-ref? (cadr form))
	     (transform-free-var-set (cadr form) (walk-form nil (caddr form)))
	     (list 'setq (cadr form) (walk-form nil (caddr form)))))
	(t (cons 'progn
		 (nloop (with f (cdr form))
			(stop-unless (consp f))
			(for sym (pop f))
			(collect
			  (iff (null f)
			      (list 'setq sym)
			      (iff (free-var-ref? sym)
				  (transform-free-var-set
				    sym (walk-form nil (pop f)))
				  (list 'setq sym (walk-form nil (pop f)))))))))))

(defun walk-quote (tail? form) tail?
  form)

(defun walk-the (tail? forms)
  (list* 'the (cadr forms) (walk-forms tail? (cddr forms))))

(defun walk-function (tail? forms) tail?
  (iff (and (symbolp (cadr forms)) (null (cddr forms)))
      forms
      (list* 'function (walk-form nil (cadr forms))
	     (cddr forms))))

(defun walk-tail (tail? forms)
  (cons (car forms) (walk-forms tail? (cdr forms))))

(defun walk-notail (tail? forms) tail?
  (cons (car forms) (walk-forms nil (cdr forms))))

(defun walk-block (tail? forms) tail?
  (list* 'block (cadr forms) (walk-forms tail? (cddr forms))))

(defun walk-fcncall (tail? forms) tail?
  (cons (car forms) (walk-forms nil (cdr forms))))

(defun walk-if (tail? forms)
  (list* 'if (walk-form nil (cadr forms))
	     (walk-form tail? (caddr forms))
	     (walk-forms tail? (cdddr forms))))

(defun walk-cond (tail? forms)
  (cons 'cond (maplist #'(lambda (clauses &aux (clause (pop clauses)))
			   (iff (consp clause)
			       (walk-forms (and tail? (null clauses))
					   clause)
			       (walk-form nil clause)))
		       (cdr forms))))

(defun walk-progv (tail? forms)	; (No dcls allowed in PROGV.)
  (let ((fcn (pop forms)) (vars (pop forms)) (vals (pop forms)))
    fcn
    (list* 'progv vars (walk-form nil vals)
	   (walk-forms tail? forms))))

(defun walk-eval-when (tail? forms)
  (let ((fcn (pop forms)) (sits (pop forms)))
    fcn
    (list* 'eval-when sits (walk-forms tail? forms))))

(defun walk-tagbody (tail? forms)	; (No dcls allowed in TAGBODY.)
  (list* 'tagbody (walk-forms tail? (cdr forms) t)))

(defun walk-prog (tail? forms)
  (walk-let tail? forms t))

(defun walk-prog* (tail? forms)
  (walk-let* tail? forms t))

(defun walk-mvb (tail? forms &aux (*bound-vars *bound-vars) *specials)
  (let ((fcn (pop forms)) (vars (pop forms)) (source (pop forms)))
    fcn
    (walk-any-dcls forms)
    (list* 'multiple-value-bind vars (walk-form nil source)
	   (progn (append-local vars *bound-vars)
		  (walk-forms tail? forms)))))

(defun walk-mvsetq (tail? forms) tail?
  (let ((fcn (pop forms)) (vars (pop forms)) (form (pop forms)))
    fcn
    (list* 'multiple-value-setq vars (walk-form nil form)
	   (walk-forms nil forms))))

(defun walk-defun (tail? forms) tail?
  (let ((fcn (pop forms)) (name (pop forms)) (args (pop forms)))
    (cons fcn (walk-definition name args forms))))

(defun walk-lambda (tail? forms) tail?
  (let ((fcn (pop forms)) (args (pop forms)))
    (walk-definition fcn args forms)))

(defun walk-definition (def args body &aux (*bound-vars *bound-vars) *specials)
  (walk-any-dcls body)
  (list* def (walk-arglist args) (walk-forms t body)))

; This pushes onto *BOUND-VARS, bind that before calling this.
(defun walk-arglist (args)
  (iff (not (consp args))
      args
      (mapcar #'(lambda (arg)
		  (cond ((symbolp arg)
			 (unless (memq arg lambda-list-keywords)
			   (push-local arg *bound-vars))
			 arg)
			((consp arg)
			 (prog1
			   (list* (car arg)	  ;; <var>
				  (walk-form nil (cadr arg)) ;; <init-form>
				  (cddr arg))	       	       ;; ( <svar> )
			   (cond ((symbolp (car arg))	       ;; Var
				  (push-local (car arg) *bound-vars))
				 ((and (consp (car arg));; (( <key> <var> )... )
				       (not (null (cdr (car arg))))
				       (symbolp (cadr (car arg))))
				  (push-local (cadr (car arg))
						       *bound-vars)))
			   (or (null (cddr arg)) (not (symbolp (caddr arg)))
			       (push-local (caddr arg) *bound-vars))))
			(t arg)))
              args)))


(defun walk-do (tail? forms) (walk-let tail? forms t t))
(defun walk-do* (tail? forms) (walk-let* tail? forms t t))

(defun bind-vars-parallel (var-forms &optional do?
			   &aux (new-locals *bound-vars) do-vars)
  (if do? (setq do-vars (nconc (mapcar #'(lambda (var-form)
					   (cond ((symbolp var-form) var-form)
						 ((symbolp (car var-form))
						  (car var-form))))
				       var-forms)
			       *bound-vars)))
  (values
    (mapcar #'(lambda (var-form &aux sym)
		(iff (and (symbolp var-form) (not (null var-form)))
		    (setq sym var-form
			  var-form nil)
		    (setq sym (pop var-form)))
		(push-local sym new-locals)
		(if (null var-form) sym
		    (list* sym (if (not do?) (walk-forms nil var-form)
				 (list* (walk-form nil (car var-form))
					(let ((*bound-vars do-vars))
					  (walk-forms nil (cdr var-form))))))))
	    var-forms)
    new-locals))

(defun walk-let (tail? forms &optional quote-atoms? do?
		 &aux (*bound-vars *bound-vars) *specials)
  (let ((fcn (pop forms)) (var-forms (pop forms)))
    (walk-any-dcls forms)
    (list* fcn
	   (multiple-value-bind (vforms new-locals)
	       (bind-vars-parallel var-forms do?)
	     (list* vforms
		    (let ((*bound-vars new-locals))
		      (if (not do?)
			  (walk-forms tail? forms quote-atoms?)
			  (list* (walk-forms tail? (car forms)) ;; Endtest form
				 (walk-forms nil (cdr forms) t)))))))))

(defun bind-vars-sequential (var-forms &optional do? &aux do-vars)
  (if do? (setq do-vars (nconc (mapcar #'(lambda (var-form)
					   (cond ((symbolp var-form) var-form)
						 ((symbolp (car var-form))
						  (car var-form))))
				       var-forms)
			       *bound-vars)))
  (mapcar #'(lambda (var-form &aux sym)
	      (iff (and (symbolp var-form) (not (null var-form)))
		  (setq sym var-form
			var-form nil)
		  (setq sym (pop var-form)))
	      (prog1
		(if (null var-form) sym
		  (list* sym (if (not do?) (walk-forms nil var-form)
			       (list* (walk-form nil (car var-form))
				      (let ((*bound-vars do-vars))
					(walk-forms nil (cdr var-form)))))))
		(push-local sym *bound-vars)))
	  var-forms))

(defun walk-let* (tail? forms &optional quote-atoms? do?
		  &aux (*bound-vars *bound-vars) *specials)
  (let ((fcn (pop forms)) (var-forms (pop forms)))
    (walk-any-dcls forms)
    (list* fcn (bind-vars-sequential var-forms do?)
	   (if (not do?)
	       (walk-forms tail? forms quote-atoms?)
	       (list* (walk-forms tail? (car forms)) ;;Endtest form
		      (walk-forms nil (cdr forms) t))))))

(defun walk-flet (tail? forms &aux (new-flocals *bound-fcns)
		                   (*bound-vars *bound-vars) *specials)
  (let ((fcn (pop forms)) (fcn-forms (pop forms)))
    fcn
    (walk-any-dcls forms)
    (list* 'flet
	   (mapcar #'(lambda (def)
		       (iff (and (consp def) (symbolp (car def)))
			   (push (car def) new-flocals))
		       (iff (and (consp def) (consp (cdr def)))
			   (walk-definition (car def) (cadr def)
					    (cddr def))
			   def))
		   fcn-forms)
	   (let ((*bound-fcns new-flocals))
	     (walk-forms tail? forms)))))

(defun walk-labels (tail? forms &aux (*bound-fcns *bound-fcns)
		                     (*bound-vars *bound-vars) *specials)
  (let ((fcn (pop forms)) (fcn-forms (pop forms)))
    fcn
    (walk-any-dcls forms)
    (mapc #'(lambda (def)
	      (iff (and (consp def) (symbolp (car def)))
		  (push (car def) *bound-fcns)))
	  fcn-forms)
    (list* 'labels
	   (mapcar #'(lambda (def)
		       (iff (and (consp def) (consp (cdr def)))
			   (walk-definition (car def) (cadr def)
					    (cddr def))
			   def))
		   fcn-forms)
	   (walk-forms tail? forms))))

(defun walk-compiler-let (tail? forms)
       ;; (No declarations allowed.)
  (let ((fcn (pop forms)) (var-forms (pop forms)))
    fcn
    (list* 'compiler-let
	   (mapcar #'(lambda (var-form &aux sym)
		       (iff (and (symbolp var-form) (not (null var-form)))
			   (setq sym var-form
				 var-form nil)
			   (setq sym (pop var-form)))
		       (list* sym (walk-form nil (car var-form))
			      (cdr var-form)))
		   var-forms)
	   (walk-forms tail? forms))))

(defun walk-return-from (tail? forms) tail?
  (let ((fcn (pop forms)) (name (pop forms)) (result (pop forms)))
    (list* fcn name (walk-form nil result) forms)))
