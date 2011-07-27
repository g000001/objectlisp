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

;;;; Utilities

#+(or lambda symbolics)
(defprop defclassvar "Class variable" si:definition-type-name)

(defmacro DEFCLASSVAR ((sym obj) &optional value)
  `(progn
     (defvar ,sym)
     (establish-class-var ',sym ,obj (obl ,value))))

(defmacro DEFCLASSVARS (obj &body vars)
  `(progn
     ,@(nloop (for-in var vars)
	      (collect (if (symbolp var)
			   `(defclassvar (,var ,obj) )
			   `(defclassvar (,(car var) ,obj)
				         ,(cadr var)))))))

(defun establish-class-var (sym obj value)
  (if (record-source-file-name sym 'defclassvar)
      (ask-aux obj (have sym value))))

#+(or lambda symbolics)
(defprop definstancevar "Instance variable" si:definition-type-name)

(defmacro DEFINSTANCEVAR ((sym obj) &optional value &aux key)
  (flet ((value-fcn (form)	;; See BIND-BUILTIN-INSTANCE-VARS.
	   (cond ((null form) nil)
		 ((symbolp form) `',form)
		 ((not (consp form)) `',form)
		 ((eq (car form) 'quote) `',(ncons (cadr form)))
		 (t `#'(lambda () ,form)))))
    (if (consp sym)
	(setq key (cadr sym)  sym (car sym))
	(setq key sym))
    `(progn
       (defvar ,sym)
       (establish-instance-var ',sym ',key ,obj ,(value-fcn value)))))

(defmacro DEFINSTANCEVARS (obj &body vars)
  `(progn
     ,@(nloop (for-in var vars)
	      (collect (if (symbolp var)
			   `(definstancevar (,var ,obj) )
			   `(definstancevar
			      (,(car var) ,obj) ,(cadr var)))))))

; VAL-FCN is either a function or an atomic form.
(defun establish-instance-var (sym key obj val-fcn &aux old)
  (check-type sym symbol)
  (if (record-source-file-name sym 'definstancevar)
      (if (env-instance-var-inits (own-env obj))
	  (if (setq old (assq sym (env-instance-var-inits (own-env obj))))
	      (rplacd old (list key val-fcn))
	      (nconc (env-instance-var-inits (own-env obj))
		     (ncons (list sym key val-fcn))))
	  (setf (env-instance-var-inits (own-env obj))
		(ncons (list sym key val-fcn)))))
  nil)

(defun UNDEF-INSTANCEVAR (sym obj)
  (check-obj obj)
  (setf (env-instance-var-inits (own-env obj))
	(remq (assq sym (env-instance-var-inits (own-env obj)))
	      (env-instance-var-inits (own-env obj)))))

(defun bind-builtin-instance-vars (obj args &aux override)
  (nloop
    (for-in env (obj-envs obj))	;; See DEFINSTANCEVAR.
    (nloop (for-in sym-key-init (env-instance-var-inits env))
	    (for sym (car sym-key-init))
	    (for key (cadr sym-key-init))
	    (for init (caddr sym-key-init))
	   (unless (own? sym obj)
	     (if (setq override (memq key args))
		 (ask-aux obj (have sym (cadr override)))
	         (ask-aux obj
		   (have sym (cond ((functionp init) (funcall init))
				   ((symbolp init) (get-sym-val init))
				   ((consp init) (car init))
				   (t init)))))))))

(defun INSTANCEVAR-DEFS (&optional (obj *object))
  (copy-list (env-instance-var-inits (own-env obj))))

#+(or lambda symbolics)
(defprop defkind "Object" si:definition-type-name)

(defmacro DEFKIND (name &rest superclasses)
  `(progn
     (defvar ,name)
     (%defkind ',name (mapcar #'symbol-value ',(copy-list superclasses)))))

(defun DEFINE-KIND (name &rest superclasses)
  (%defkind name (copy-list superclasses)))

(defun %defkind (name superclasses
		      &aux (obj (if (boundp name) (symbol-value name))))
  (when (record-source-file-name name 'defkind)
    (if (object? obj)
	(setq obj (apply #'remake-obj obj superclasses))
        (setq obj (%kindof t t nil superclasses)))
    (ask-aux obj (have 'class-name name))
    (set name obj)))

(defun SPECIALIZATIONS (&optional (obj *object))
  (unless (global-obj? obj)
    (remq obj (append (copy-list (env-class-objs (own-env obj)))
		      (copy-list (env-instance-objs (own-env obj)))))))

;;;; Description

(defun CURRENT-OBJ ()
  (if (eq *object *internal-global-obj)
      nil
      *object))

(defun WHAT (&optional (obj *object) &aux subs subinsts)
  (check-obj obj)
  (if (global-obj? obj)
      (print "The global object.")
      (apply #'format t
	     "~%Object ~s, including~#[ only the global object~
           ~; ~s~; ~s and ~s~:;~@{~#[~; and~] ~s~^,~}~]." 
	     obj (base-objs obj)))
  (setq subs (specializations obj))
  (setq subinsts (nloop (collecting t)
			(for-in sub subs)
			(if (not (obj-class? sub))
			    (collect sub))
		        (or (obj-class? sub)
			    (setq subs (remq sub subs)))))
  (if subinsts
      (format t "~% ~d instance~:p." (length subinsts))
      (if subs (terpri)))
  (if subs
      (apply #'format t
	     " Class specialization~p~#[~
	     ~; ~s~; ~s and ~s~:;~@{~#[~; and~] ~s~^,~}~]."
	     (length subs) subs)))

(defun SHOW (&optional (obj *object))
  (what obj)
  (unless (null obj) (show1 (own-env obj))))

(defun show1 (env &aux (vals (env-val-bindings env))
	               (fcns (env-fcn-bindings env)))
  (iff (zerop (table-count vals))
       (format t "~%  No variables bound.")
    (format t "~%  Variables: ")
    (bindings-map #'(lambda (binding) (format t "~s " (nonglobal-binding-sym binding)))
		  vals))
  (iff (zerop (table-count fcns))
       (format t "~%  No functions bound.")
    (format t "~%  Functions: ")
    (bindings-map #'(lambda (binding) (format t "~s " (nonglobal-binding-sym binding)))
		  fcns))
  (when (env-instance-var-inits env)
	(format t "~%  Instance variable inits: ")
        (nloop (for-in sym-key-init (env-instance-var-inits env))
	       (format t "~s " (car sym-key-init))))
  nil)

(defun SHOW-VALS (&optional (obj *object))
  (what obj)
  (unless (global-obj? obj)
    (format t "~% Values:")
    (bindings-map #'(lambda (binding)
		      (format t "~%  ~s: ~s" (nonglobal-binding-sym binding)
			                     (binding-val binding)))
		  (env-val-bindings (own-env obj))))
  nil)

(defun SHOW-ALL (&optional (obj *object))
  (what obj)
  (unless (global-obj? obj)
    (nloop (for-in env (obj-envs obj))
	   (format t "~%~s: " (env-object env))
	   (show1 env))))

(defun OWN (&optional (obj *object))
  (check-obj obj)
  (unless (global-obj? obj)
      (bindings-map-return #'(lambda (binding) (nonglobal-binding-sym binding))
			   (env-val-bindings (own-env obj)))))

(defun OWN? (sym &optional (obj *object) &aux link)
  (check-obj obj)
  (or (and (global-obj? obj) (boundp sym))
      (and (setq link (get-val-link? sym))
	   (not (null (binding-from-env link (own-env obj)))))))

(defun WHERE (sym &optional (obj *object) &aux link)
  (check-obj obj)
  (if (or (global-obj? obj) (null (setq link (get-val-link? sym))))
      nil
      (some #'(lambda (env) (if (binding-from-env link env)
				(env-object env)))
	    (obj-envs obj))))

(defun THERE? (sym &optional (obj *object) &aux link)
  (check-obj obj)
  (or (and (setq link (get-val-link? sym))
	   (not (null (some #'(lambda (env) (binding-from-env link env))
			    (obj-envs obj)))))
      (boundp sym)))

(defun MAPC-OWN (fcn &optional (obj *object))
  (check-obj obj)
  (unless (global-obj? obj)
    (bindings-map #'(lambda (binding) (funcall fcn (nonglobal-binding-sym binding)))
		  (env-val-bindings (own-env obj)))))

(defun MAPCAR-OWN (fcn &optional (obj *object))
  (check-obj obj)
  (unless (global-obj? obj)
    (bindings-map-return #'(lambda (binding) (funcall fcn (nonglobal-binding-sym binding)))
      (env-val-bindings (own-env obj)))))

(defun FOWN (&optional (obj *object))
  (check-obj obj)
  (unless (global-obj? obj)
    (bindings-map-return #'(lambda (binding) (nonglobal-binding-sym binding))
			 (env-fcn-bindings (own-env obj)))))

(defun FOWN? (sym &optional (obj *object) &aux link)
  (check-obj obj)
  (or (and (global-obj? obj) (fboundp sym))
      (and (setq link (get-fcn-link? sym))
	   (not (null (fbinding-from-env link (own-env obj)))))))

(defun FWHERE (sym &optional (obj *object) &aux link)
  (check-obj obj)
  (if (or (global-obj? obj) (null (setq link (get-fcn-link? sym))))
      nil
      (nloop (collecting t)
	     (for-in env (obj-envs obj))
	     (if (not (null (fbinding-from-env link env)))
		 (collect (env-object env))))))

(defun FTHERE? (sym &optional (obj *object) &aux link)
  (check-obj obj)
  (or (and (setq link (get-fcn-link? sym))
	   (not (null (some #'(lambda (env) (binding-from-env link env))
			    (obj-envs obj)))))
      (fboundp sym)))

(defun FDOC (sym &optional (obj *object) &aux link)
  (check-obj obj)
  (iff (or (global-obj? obj) (null (setq link (get-fcn-link? sym))))
       nil
    (format t "~%Function binding of ~s in ~s is found in ~%" sym obj)
    (nloop (for-in env (obj-envs obj))
	   (for bind (fbinding-from-env link env))
	   (if bind (format t "~s, " (env-object env))))
    (if (fboundp sym) (format t "and the global object."))))

(defun MAPC-FOWN (fcn &optional (obj *object))
  (check-obj obj)
  (unless (global-obj? obj)
    (bindings-map #'(lambda (binding) (funcall fcn (nonglobal-binding-sym binding)))
	  (env-fcn-bindings (car (obj-envs obj))))))

(defun MAPCAR-FOWN (fcn &optional (obj *object))
  (check-obj obj)
  (unless (global-obj? obj)
    (bindings-map-return #'(lambda (binding) (funcall fcn (nonglobal-binding-sym binding)))
	  (env-fcn-bindings (car (obj-envs obj))))))
