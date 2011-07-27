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

;;;; Objects and envlist

; BEGINNING OF REPRESENTATION-DEPENDENT CODE FOR OBJECTS.

(defmicroparameter *object nil)
(defmicroparameter *val-class-disp nil)
(defmicroparameter *fcn-class-disp nil)
(defmicroparameter *sdw-class-disp nil)
(defmicroparameter *instance-envs nil)

(defmicroparameter *shadows nil)

(defun obj-print-fcn (obj #+lambda standard-output  #-lambda *standard-output*
			  ignore) #-(or lambda symbolics) ignore
  (print-object obj))

;MAKE-OBJ is called with a list of environments, innermost to outermost.
(defstruct (obj (:conc-name nil) #+lambda(:callable-constructors nil)
		(:constructor %make-obj (obj-envlist obj-superiors))
		(:print-function #-dec obj-print-fcn #+dec 'obj-print-fcn))
  obj-instance-envs
  obj-val-class-disp  ;;; Value class-bindings dispatch table.
  obj-fcn-class-disp  ;;; Fcn class-bindings dispatch table.
  obj-sdw-class-disp  ;;; Shadows class-bindings dispatch table.
  obj-class?
  obj-envlist       ;;; List of environments comprising this object, innermost to
                    ;;;  outermost.  Global env is implicit.
  obj-superiors     ;;; Args from KINDOF, for documentation.
  obj-index
  )

(defvar *internal-global-obj (%make-obj nil nil))

(defsubst global-obj? (obj)
  (or (null obj) (eq obj *internal-global-obj)))

; END OF REPRESENTATION-DEPENDENT CODE FOR OBJECTS.

;;;; Environments

; BEGINNING OF REPRESENTATION-DEPENDENT CODE FOR ENVIRONMENTS.

(defstruct (environment (:conc-name nil) #+lambda(:callable-constructors nil))
  env-class-objs     ;;All objects in which this is a class env, except any obj
                     ;;; that >shares< its class-disp tables with its superior.
  env-instance-objs  ;;All objects in which this is an instance env.
  (env-val-bindings  ;;Table of values bound locally in this env.
    (make-binding-table))
  (env-fcn-bindings  ;;Table of functions bound locally in this env.
    (make-binding-table))
  env-object         ;;Object for which this is outermost env (for documentation).
  env-instance-var-inits
  )

(defsubst obj-envs (obj)
  (if obj (obj-envlist obj)))

(defmacro own-env (obj)
  `(car (obj-envs ,obj)))

(defmacro global-env? (env)
  `(null ,env))

(defmacro object? (object)
  `(typep ,object 'obj))

(defmacro binding-from-env (link env)
  `(lookup-binding ,link (env-val-bindings ,env)))

(defmacro fbinding-from-env (link env)
  `(lookup-binding ,link (env-fcn-bindings ,env)))

; END OF REPRESENTATION-DEPENDENT CODE FOR ENVIRONMENTS.

; BEGINNNG OF REPRESENTATION-DEPENDENT CODE FOR LINKS.

(defstruct (link (:include basic-link)
		 (:conc-name nil) #+lambda(:callable-constructors nil)
		 (:constructor make-link
			       (link-key link-index link-dispatch)))
  link-dispatch
  (link-memo-key t)   ;; An envlist, class-disp table, or T if no key.
  link-memo-binding   ;; A binding, if valid.
 )

(defstruct (sdw-link (:include link)
		     (:conc-name nil) #+lambda(:callable-constructors nil)
		     (:constructor make-sdw-link
			    (link-key link-index link-dispatch link-fcn-link)))
  link-fcn-link)      ;; Fcn link associated with this shadow link.

(defmacro link-symbol (link)
  `(link-key ,link))

(defmacro get-val-link? (sym)
  `(get ,sym 'val-link))

(defmacro get-fcn-link? (sym)
  `(get ,sym 'fcn-link))

(defmacro get-sdw-link? (sym)
  `(get ,sym 'sdw-link))

(defun linkup-val (sym &aux link)
  (declare (special *val-lookup-global-only))
  (setq link (make-link sym (sxhash sym) *val-lookup-global-only))
  (setf (get sym 'val-link) link)
  (adjust-global-val-lookups link)
  link)

(defun linkup-fcn (sym &aux link)
  (declare (special *fcn-lookup-global-only))
  (setq link (make-link sym (sxhash sym) *fcn-lookup-global-only))
  (setf (get sym 'fcn-link) link)
  (adjust-global-fcn-lookups link)
  link)

(defun linkup-sdw (sym &aux link (fcn-link (get-fcn-link sym)))
  (setq link (make-sdw-link sym (sxhash sym) (sdw-disp-from-fcn-link fcn-link)
			    fcn-link))
  (setf (get sym 'sdw-link) link)
  (adjust-global-fcn-lookups fcn-link)
  link)

(defun get-val-link (sym)
  (or (get-val-link? sym)
      (linkup-val sym)))

(defun get-fcn-link (sym)
  (or (get-fcn-link? sym)
      (linkup-fcn sym)))

(defun get-sdw-link (sym)
  (or (get-sdw-link? sym)
      (linkup-sdw sym)))

; END OF REPRESENTATION-DEPENDENT CODE FOR LINKS.


; BEGINNNG OF REPRESENTATION-DEPENDENT CODE FOR DISPATCHES.

(defun make-class-disp-table (&rest key-val-pairs)
  (apply 'make-table key-val-pairs))

(defmacro class-insert (link binding disp)
  `(insert ,link ,binding ,disp))

(defmacro class-remove (link disp)
  `(table-remove ,link ,disp))

(defmacro class-lookup (link disp)
  `(car (lookup ,link ,disp)))

; END OF REPRESENTATION-DEPENDENT CODE FOR DISPATCHES.

;;;; Bindings

; BEGINNING OF REPRESENTATION-DEPENDENT CODE FOR BINDINGS.

; Bindings format: table.
; Binding format:
; RASSQ-CONS of (val/fcn . link) or SYM for global val/fcn cell, except
;  (LOCF SYM) or (LOCF #'SYM) in place of SYM if *LOCF? is nonnull.

(defun make-binding-table (&rest key-val-pairs)
  (apply #'make-table key-val-pairs))

(defmacro make-binding (link val)
  `(cons ,val ,link))

(defmacro insert-binding (link val bindings)
  `(insert ,link ,val ,bindings))

(defmacro remove-binding (link bindings)
  `(table-remove ,link ,bindings))

(defmacro bindings-map (fcn bindings)
  `(table-map ,fcn ,bindings))

(defmacro bindings-map-return (fcn bindings)
  `(table-map-return ,fcn ,bindings))

(defmacro lookup-binding (link bindings)
  `(lookup ,link ,bindings))

(defmacro nonglobal-binding-sym (binding)
  `(link-symbol (cdr ,binding)))

(defmacro nonglobal-binding-link (binding)
  `(cdr ,binding))

(defmacro binding-val (binding)
  (if *locf?
      `(car ,binding)
      `(let ((binding ,binding))
	 (if (symbolp binding)
	     (symbol-value binding)
	     (car binding)))))

(defmacro nonglobal-binding-fcn (binding)
  `(car ,binding))

(defmacro binding-fcn (binding)
  (if *locf?
      `(car ,binding)
      `(let ((binding ,binding))
	 (if (symbolp binding)
	     (symbol-function binding)
	     (nonglobal-binding-fcn binding)))))

(defun %shadowed-noop (&rest args)
  (declare (ignore args)))

(defmacro global-binding-fcn-for-shadow (symbol-or-loc)
  (if *locf?
      `(let ((loc ,symbol-or-loc))
	 (if (location-boundp loc)
	     (car loc)
	     #'%shadowed-noop))
      `(let ((sym ,symbol-or-loc))
         (if (fboundp sym)
	     (symbol-function sym)
	     #'%shadowed-noop))))

(defmacro binding-fcn-for-shadow (binding)
  `(let ((binding ,binding))
     (if (consp binding)
	 (nonglobal-binding-fcn binding)
         (global-binding-fcn-for-shadow binding))))

(defmacro set-binding-val (binding val)
  (if *locf?
      `(setf (car ,binding) ,val)
      `(let ((binding ,binding))
	 (if (symbolp binding)
	     (set binding ,val)
	     (setf (car binding) ,val)))))

(defmacro set-binding-fcn (binding fcn)
  (if *locf?
      `(setf (car ,binding) ,fcn)
      `(let ((binding ,binding))
	 (if (symbolp ,binding)
	     (setf (symbol-function binding) ,fcn)
	     (setf (car binding) ,fcn)))))


(defmacro global-val-binding (link)
  (if *locf?
      `(locf (symbol-value (link-symbol ,link)))
      `(link-symbol ,link)))

(defmacro global-fcn-binding (link)
  (if *locf?
      `(locf (symbol-function (link-symbol ,link)))
      `(link-symbol ,link)))

(defmacro global-sdw-binding (link)
  (declare (ignore link))
  (if *locf?
      `(locf (symbol-function '%shadowed-noop))
      ;; '%shadowed-noop))               ;????
      '(FUNCTION %shadowed-noop)))               ;????

; END OF REPRESENTATION-DEPENDENT CODE FOR BINDINGS.

;FINDING BINDINGS.

;Global binding-- the symbol itself-- >IS< included.
(defmacro binding-from-envlist (link envlist)
  `(nloop (for-in env ,envlist)
	  (for binding (binding-from-env ,link env))
	  (if binding (return-from nil binding))
	  (finally (global-val-binding ,link))))

(defmacro binding-from-class (link disp)
  `(or (class-lookup ,link ,disp)
       (global-val-binding ,link)))

;Global binding-- the symbol itself-- is >NOT< included.
(defmacro binding-from-envlist-proper (link envlist)
  `(nloop (for-in env ,envlist)
	  (for binding (binding-from-env ,link env))
	  (if binding (return binding))))

(defmacro fbinding-from-envlist (link envlist)
  `(nloop (for-in env ,envlist)
	  (for fbinding (fbinding-from-env ,link env))
	  (if fbinding (return fbinding))
	  (finally (global-fcn-binding ,link))))

(defmacro fbinding-from-class (link disp)
  `(or (class-lookup ,link ,disp)
       (global-fcn-binding ,link)))

;  `(let ((fbs (class-lookup ,link ,disp)))
;     (etypecase fbs
;       (cons (car fbs))
;       (null (global-fcn-binding ,link))
;       (symbol fbs))))

(defmacro fbinding-from-envlist-proper (link envlist)
  `(nloop (for-in env ,envlist)
	  (for fbinding (fbinding-from-env ,link env))
	  (if fbinding (return fbinding))))

;Returns LIST* of all but the 1st fbinding of SYM in ENVLIST,
; inner to outer binding.
;Global binding-- the symbol itself-- >IS< included.
(defmacro shadows-from-envlist (sdw-link envlist)
  `(let* ((fcn-link (link-fcn-link ,sdw-link))
	  (fbs (nloop (collecting t)
		     (for-in env ,envlist)
		     (for fbinding (fbinding-from-env fcn-link env))
		     (if fbinding (collect fbinding)))))
     (if fbs
	 (nconc (cdr fbs) (global-fcn-binding ,sdw-link))
         (global-sdw-binding ,sdw-link))))

(defmacro shadows-from-class (link disp)
  `(or (class-lookup ,link ,disp)
       (global-sdw-binding ,link)))

;Returns LIST of all fbindings of SYM in ENVLIST, inner to outer binding.
;Global binding-- the symbol itself-- is >NOT< included.
(defmacro fbindings-from-envlist-proper (link envlist)
 `(nloop (collecting t)
	 (for-in env ,envlist)
	 (for fbinding (fbinding-from-env ,link env))
	 (if fbinding (collect fbinding))))

(defmacro shadows-from-envlist-proper (sdw-link envlist)
  `(let ((fcn-link (link-fcn-link ,sdw-link)))
     (nloop (collecting t)
	    (for-in env ,envlist)
	    (for fbinding (fbinding-from-env fcn-link env))
	    (if fbinding (collect fbinding)))))

(microdefun fbinding-from-object (link obj)
  (fbinding-from-envlist link (obj-envs obj)))

; ADDING (and deleting) BINDINGS

(microdefun revise-val-class-disp-for-obj (link obj)
  (let ((binding (binding-from-envlist link (obj-envs obj))))
    (if binding
	(class-insert link binding (obj-val-class-disp obj))
	(class-remove link (obj-val-class-disp obj)))))

(microdefun revise-fcn-class-disp-for-obj (link obj)
  (let ((bindings (fbindings-from-envlist-proper link (obj-envs obj)))
	(sdw-link (get-sdw-link? (link-symbol link))))
    (cond (bindings
	   (class-insert link (car bindings) (obj-fcn-class-disp obj))
	   (when sdw-link
	     (class-insert sdw-link
			   (nconc (cdr bindings) (global-FCN-binding link))
			   (obj-sdw-class-disp obj))))
	  (t
	   (class-remove link (obj-fcn-class-disp obj))
	   (when sdw-link
	     (class-remove sdw-link (obj-sdw-class-disp obj)))))))

(microdefun revise-val-class-disps-for-env (link env)
  (nloop (for-in obj (env-class-objs env))
	 (revise-val-class-disp-for-obj link obj)))

(microdefun revise-fcn-class-disps-for-env (link env)
  (nloop (for-in obj (env-class-objs env))
	 (revise-fcn-class-disp-for-obj link obj)))


(defsubst flush-val-memo (link)
  (setf (link-memo-key link) t))

;Only call if binding doesn't already exist.
(microdefun bind-val-in-env (sym val env)
  (let ((link (get-val-link sym)))
    (progn
      (unless (null (env-class-objs env))
	(first-class-val-binding link))
      (unless (null (env-instance-objs env))
	(first-instance-val-binding link))
      (flush-val-memo link)
      (prog1
	;; !!! watch out for ABORTs
	(insert-binding link val (env-val-bindings env))
	(revise-val-class-disps-for-env link env)))))

(defsubst flush-fcn-memo (link)
  (setf (link-memo-key link) t))
  ;;;**** should clobber link-memo-bindings, too.

(defsubst flush-sdw-memo (link)
  (setf (link-memo-key link) t))

;Only call if binding doesn't already exist.
(microdefun bind-fcn-in-env (sym fcn env)
  (let ((link (get-fcn-link sym)))
    (progn
      (unless (null (env-class-objs env))
	(first-class-fcn-binding link))
      (unless (null (env-instance-objs env))
	(first-instance-fcn-binding link))
      (flush-fcn-memo link)
      (let ((sdw-link (get-sdw-link? sym)))
	(when sdw-link (flush-sdw-memo link)))
      (prog1
	;; !!! watch out for ABORTs
	(insert-binding link fcn (env-fcn-bindings env))
	(revise-fcn-class-disps-for-env link env)))))

(microdefun unbind-val-in-env (sym env)
  (let ((link (get-val-link sym)))
    (remove-binding link (env-val-bindings env))
    (flush-val-memo link)
    (revise-val-class-disps-for-env link env)))

(microdefun unbind-fcn-in-env (sym env)
  (let ((link (get-fcn-link sym)))
    (remove-binding link (env-fcn-bindings env))
    (flush-fcn-memo link)
    (revise-fcn-class-disps-for-env link env)))

; RESOLVING LINKS-- (run-time)

;memoize instance-only by instance, class-only by class,
;both-bound by instance (cuz it has to be checked there anyway),
; unless instance-envs null, then use class-disp.

;Returns innermost binding.
;Consults & maintains memoized binding.
(defmacro binding-from-link (link)
  `(microfuncall (link-dispatch ,link) ,link))

(defmacro val-binding-from-link (link)
  `(binding-from-link ,link))

(defmacro fcn-binding-from-link (link)
  `(binding-from-link ,link))

(defmacro sdw-binding-from-link (link)
  `(binding-from-link ,link))

;WARNING: Redefining this function will invalidate all extant LINK defstructs,
; which can only be fixed by REBOOTING TO A NON-OBL BAND.
(miscdefun val-lookup-global-only (link)
  (global-val-binding link))

(miscdefun fcn-lookup-global-only (link)
  (global-fcn-binding link))

(miscdefun sdw-lookup-global-only (link)
  (declare (ignore link))
  (global-sdw-binding link))

(defvar *val-lookup-global-only (microfunction val-lookup-global-only))
(eval-when (load eval) (setf (symbol-function 'val-lookup-global-only)
			     *val-lookup-global-only))
(defvar *fcn-lookup-global-only (microfunction fcn-lookup-global-only))
(eval-when (load eval) (setf (symbol-function 'fcn-lookup-global-only)
			     *fcn-lookup-global-only))
(defvar *sdw-lookup-global-only (microfunction sdw-lookup-global-only))
(eval-when (load eval) (setf (symbol-function 'sdw-lookup-global-only)
			     *sdw-lookup-global-only))

;WARNING: Redefining these functions will invalidate all extant LINK defstructs,
; which can only be fixed by REBOOTING TO A NON-OBL BAND.
(defmacro def-lookup-instance-bound-only (name global-fcn envlist-fcn fcn?)
  `(miscdefun ,name (link)
     ,@(if fcn? `((setq *shadows nil)))
     (let ((instance-envs *instance-envs))
       (cond ((null instance-envs) (,global-fcn link))
	     ((eq (link-memo-key link) instance-envs)
	      (link-memo-binding link))
	     (t (atomically
		  (setf (link-memo-key link) instance-envs)
		  (setf (link-memo-binding link)
			(,envlist-fcn link instance-envs))))))))

(def-lookup-instance-bound-only val-lookup-instance-bound-only
  global-val-binding binding-from-envlist nil)
(def-lookup-instance-bound-only fcn-lookup-instance-bound-only
  global-fcn-binding fbinding-from-envlist t)
(def-lookup-instance-bound-only sdw-lookup-instance-bound-only
  global-sdw-binding shadows-from-envlist nil)

(defvar *val-lookup-instance-bound-only
	(microfunction val-lookup-instance-bound-only))
(eval-when (load eval) (setf (symbol-function 'val-lookup-instance-bound-only)
			     *val-lookup-instance-bound-only))
(defvar *fcn-lookup-instance-bound-only
	(microfunction fcn-lookup-instance-bound-only))
(eval-when (load eval) (setf (symbol-function 'fcn-lookup-instance-bound-only)
			     *fcn-lookup-instance-bound-only))
(defvar *sdw-lookup-instance-bound-only
	(microfunction sdw-lookup-instance-bound-only))
(eval-when (load eval) (setf (symbol-function 'sdw-lookup-instance-bound-only)
			     *sdw-lookup-instance-bound-only))

;WARNING: Redefining these functions will invalidate all extant LINK defstructs,
; which can only be fixed by REBOOTING TO A NON-OBL BAND.
(defmacro def-lookup-class-bound-only (name disp class-fcn fcn?)
  `(miscdefun ,name (link)
     ,@(if fcn? `((setq *shadows nil)))
     (let ((class-disp ,disp))
       (if (eq (link-memo-key link) class-disp)
	   (link-memo-binding link)
	   (atomically
	     (setf (link-memo-key link) class-disp)
	     (setf (link-memo-binding link)
		   (,class-fcn link class-disp)))))))

(def-lookup-class-bound-only val-lookup-class-bound-only
  *val-class-disp binding-from-class nil)
(def-lookup-class-bound-only fcn-lookup-class-bound-only
  *fcn-class-disp fbinding-from-class t)
(def-lookup-class-bound-only sdw-lookup-class-bound-only
  *sdw-class-disp shadows-from-class nil)

(defvar *val-lookup-class-bound-only (microfunction val-lookup-class-bound-only))
(eval-when (load eval) (setf (symbol-function 'val-lookup-class-bound-only)
			     *val-lookup-class-bound-only))
(defvar *fcn-lookup-class-bound-only (microfunction fcn-lookup-class-bound-only))
(eval-when (load eval) (setf (symbol-function 'fcn-lookup-class-bound-only)
			     *fcn-lookup-class-bound-only))
(defvar *sdw-lookup-class-bound-only (microfunction sdw-lookup-class-bound-only))
(eval-when (load eval) (setf (symbol-function 'sdw-lookup-class-bound-only)
			     *sdw-lookup-class-bound-only))

;WARNING: Redefining these functions will invalidate all extant LINK defstructs,
; which can only be fixed by REBOOTING TO A NON-OBL BAND.
(defmacro def-lookup-both-bound (name disp envlist-fcn class-fcn
				 sdw? fcn?)
 `(miscdefun ,name (link)
   ,@(if fcn? `((setq *shadows nil)))
   (let ((instance-envs *instance-envs))
    (cond ((null instance-envs)
	   (let ((class-disp ,disp))
	     (if (eq (link-memo-key link) class-disp)
		 (link-memo-binding link)
	         (atomically
		   (setf (link-memo-key link) class-disp)
		   (setf (link-memo-binding link)
			 (,class-fcn link class-disp))))))
	  ((eq (link-memo-key link) instance-envs)
	   (link-memo-binding link))
	  (t (let (binding)
	       (if (setq binding (,envlist-fcn link instance-envs))
		   (atomically
		     (setf (link-memo-key link) instance-envs)
		     (setf (link-memo-binding link)
			   ,(if (null sdw?)
				'binding
			        `(nconc (cdr binding)
					(or (class-lookup link ,disp)
					    (global-FCN-binding link))))))
		   (atomically
		     (setf (link-memo-key link) instance-envs)
		     (setf (link-memo-binding link)
			   (,class-fcn link ,disp))))))))))

(def-lookup-both-bound val-lookup-both-bound
  *val-class-disp binding-from-envlist-proper binding-from-class nil nil)
(def-lookup-both-bound fcn-lookup-both-bound
  *fcn-class-disp fbinding-from-envlist-proper fbinding-from-class nil t)
(def-lookup-both-bound sdw-lookup-both-bound
  *sdw-class-disp shadows-from-envlist-proper shadows-from-class t nil)

(defvar *val-lookup-both-bound (microfunction val-lookup-both-bound))
(eval-when (load eval) (setf (symbol-function 'val-lookup-both-bound)
			     *val-lookup-both-bound))
(defvar *fcn-lookup-both-bound (microfunction fcn-lookup-both-bound))
(eval-when (load eval) (setf (symbol-function 'fcn-lookup-both-bound)
			     *fcn-lookup-both-bound))
(defvar *sdw-lookup-both-bound (microfunction sdw-lookup-both-bound))
(eval-when (load eval) (setf (symbol-function 'sdw-lookup-both-bound)
			     *sdw-lookup-both-bound))

(defmacro def-adjust-global-val-lookups ()
  (if *global-dispatch-fcns?
      `(defun adjust-global-val-lookups (link)
	 (adjust-get-val-fcn link)
	 (adjust-set-val-fcn link))
      `(defun adjust-global-val-lookups (link)
	 (declare (ignore link))
	 (values))))

(def-adjust-global-val-lookups)

(defmacro def-adjust-global-fcn-lookups ()
  (if *global-dispatch-fcns?
      `(defun adjust-global-fcn-lookups (link)
	 (adjust-get-fcn-fcn link)
	 (adjust-get-sdw-fcn link))
      `(defun adjust-global-fcn-lookups (link)
	 (declare (ignore link))
	 (values))))

(def-adjust-global-fcn-lookups)

(defmacro evalcase (key &body clauses)
  `(let ((key ,key))
     (cond ,@(nloop (for-in clause clauses)
	       (collect (if (or (eq (car clause) 't) (eq (car clause) 'otherwise))
			    `(t ,@(cdr clause))
			    `((eq ,(car clause) key) ,@(cdr clause))))))))

(defun sdw-disp-from-fcn-link (link)
  (evalcase (link-dispatch link)
    (*fcn-lookup-global-only *sdw-lookup-global-only)
    (*fcn-lookup-instance-bound-only
      *sdw-lookup-instance-bound-only)
    (*fcn-lookup-class-bound-only
      *sdw-lookup-class-bound-only)
    (*fcn-lookup-both-bound
      *sdw-lookup-both-bound)))

(defmacro def-adjust-get-val-fcn ()
  (if *global-dispatch-fcns?
   '(defun adjust-get-val-fcn (link &optional always?
			       &aux (name (get-sym-val-name (link-symbol link)))
			            (link-fcn (link-dispatch link)))
      (when (or always? (fboundp name))
	(setf (symbol-function name)
	      (evalcase link-fcn
		(*val-lookup-global-only
		 (function get-val-lookup-global-only))
		(*val-lookup-instance-bound-only
		 (function get-val-lookup-instance-bound-only))
		(*val-lookup-class-bound-only
		 (function get-val-lookup-class-bound-only))
		(*val-lookup-both-bound
		 (function get-val-lookup-both-bound))
		(otherwise (ferror nil
   "~%Unrecognized dispatch function in link ~s, should be one of ~s." link
                       (list *val-lookup-global-only
			     *val-lookup-instance-bound-only
			     *val-lookup-class-bound-only
			     *val-lookup-both-bound)))))))
   '(defun adjust-get-val-fcn (link &optional always?)
      (declare (ignore link always?))
      (values))))

(def-adjust-get-val-fcn)

(defmacro def-adjust-set-val-fcn ()
  (if *global-dispatch-fcns?
 '(defun adjust-set-val-fcn (link &optional always?
			     &aux (name (set-sym-val-name (link-symbol link)))
			     (link-fcn (link-dispatch link)))
    (when (or always? (fboundp name))
      (setf (symbol-function name)
	    (evalcase link-fcn
	      (*val-lookup-global-only
	       (function set-val-lookup-global-only))
	      (*val-lookup-instance-bound-only
	       (function set-val-lookup-instance-bound-only))
	      (*val-lookup-class-bound-only
	       (function set-val-lookup-class-bound-only))
	      (*val-lookup-both-bound
	       (function set-val-lookup-both-bound))
	      (otherwise (ferror nil
   "~%Unrecognized dispatch function in link ~s, should be one of ~s." link
                       (list *val-lookup-global-only
			     *val-lookup-instance-bound-only
			     *val-lookup-class-bound-only
			     *val-lookup-both-bound)))))))
 '(defun adjust-set-val-fcn (link &optional always?)
    (declare (ignore link always?))
    (values))))

(def-adjust-set-val-fcn)

(defmacro def-adjust-get-fcn-fcn ()
  (if *global-dispatch-fcns?
   '(defun adjust-get-fcn-fcn (link &optional always?
			       &aux (name (get-sym-fcn-name (link-symbol link)))
			            (link-fcn (link-dispatch link)))
      (when (or always? (fboundp name))
	(setf (symbol-function name)
	      (evalcase link-fcn
		(*fcn-lookup-global-only
		  (function fcncall-lookup-global-only))
		(*fcn-lookup-instance-bound-only
		  (function fcncall-lookup-instance-bound-only))
		(*fcn-lookup-class-bound-only
		  (function fcncall-lookup-class-bound-only))
		(*fcn-lookup-both-bound
		  (function fcncall-lookup-both-bound))
		(otherwise (ferror nil
   "~%Unrecognized dispatch function in link ~s, should be one of ~s." link
				   (list *fcn-lookup-global-only
					 *fcn-lookup-instance-bound-only
					 *fcn-lookup-class-bound-only
					 *fcn-lookup-both-bound)))))))
   '(defun adjust-get-fcn-fcn (link &optional always?)
      (declare (ignore link always?))
      (values))))

(def-adjust-get-fcn-fcn)

(defmacro def-adjust-get-sdw-fcn ()
  (if *global-dispatch-fcns?
   '(defun adjust-get-sdw-fcn (fcn-link &optional always?
			       &aux (name
				      (get-sym-sdw-name (link-symbol fcn-link))))
      (when (or always? (fboundp name))
	(setf (symbol-function name)
	      (evalcase (link-dispatch fcn-link)
		(*fcn-lookup-global-only
		  (function shadow-lookup-global-only))
		(*fcn-lookup-instance-bound-only
		  (function shadow-lookup-instance-bound-only))
		(*fcn-lookup-class-bound-only
		  (function shadow-lookup-class-bound-only))
		(*fcn-lookup-both-bound
		  (function shadow-lookup-both-bound))
		(otherwise (ferror nil
   "~%Unrecognized dispatch function in link ~s, should be one of ~s." fcn-link
				   (list *fcn-lookup-global-only
					 *fcn-lookup-instance-bound-only
					 *fcn-lookup-class-bound-only
					 *fcn-lookup-both-bound)))))))
   '(defun adjust-get-sdw-fcn (fcn-link &optional always?)
      (declare (ignore fcn-link always?))
      (values))))

(def-adjust-get-sdw-fcn)

(microdefun first-class-val-binding (link)
  (evalcase (link-dispatch link)
    (*val-lookup-global-only
     (setf (link-dispatch link) *val-lookup-class-bound-only)
     (adjust-global-val-lookups link))
    (*val-lookup-instance-bound-only
     (setf (link-dispatch link) *val-lookup-both-bound)
     (adjust-global-val-lookups link))))

(microdefun first-instance-val-binding (link)
  (evalcase (link-dispatch link)
    (*val-lookup-global-only
     (setf (link-dispatch link) *val-lookup-instance-bound-only)
     (adjust-global-val-lookups link))
    (*val-lookup-class-bound-only
     (setf (link-dispatch link) *val-lookup-both-bound)
     (adjust-global-val-lookups link))))

(microdefun first-class-fcn-binding (link)
  (evalcase (link-dispatch link)
    (*fcn-lookup-global-only
     (setf (link-dispatch link) *fcn-lookup-class-bound-only)
     (adjust-global-fcn-lookups link)
     (let ((sdw-link (get-sdw-link? (link-symbol link))))
       (when sdw-link
	 (setf (link-dispatch sdw-link) *sdw-lookup-class-bound-only))))
    (*fcn-lookup-instance-bound-only
     (setf (link-dispatch link) *fcn-lookup-both-bound)
     (adjust-global-fcn-lookups link)
     (let ((sdw-link (get-sdw-link? (link-symbol link))))
       (when sdw-link
	 (setf (link-dispatch sdw-link) *sdw-lookup-both-bound))))))

(microdefun first-instance-fcn-binding (link)
  (evalcase (link-dispatch link)
    (*fcn-lookup-global-only
     (setf (link-dispatch link) *fcn-lookup-instance-bound-only)
     (adjust-global-fcn-lookups link)
     (let ((sdw-link (get-sdw-link? (link-symbol link))))
       (when sdw-link
	 (setf (link-dispatch sdw-link) *sdw-lookup-instance-bound-only))))
    (*fcn-lookup-class-bound-only
     (setf (link-dispatch link) *fcn-lookup-both-bound)
     (adjust-global-fcn-lookups link)
     (let ((sdw-link (get-sdw-link? (link-symbol link))))
       (when sdw-link
	 (setf (link-dispatch sdw-link) *sdw-lookup-both-bound))))))

;;;; Object references

(defparameter *no-specials-warning nil)

; Reference routines that interface to the walker.

(defun QSYM-REF-FORM (sym)
  (unless *no-specials-warning (warn-if-undeclared-special-aux sym))
  (if *inside-defobfun
      (progn
	(arrange-to-link-val-ref sym)
	(if *loadtime-constant-links?
	    (form-to-get-sym-val-from-link
	      sym `(delayed-expand (get-val-link ',sym)))
	    (form-to-get-sym-val-from-link
	      sym (global-val-link-form sym))))
      `(get-sym-val ',sym)))

(defun symbol-ref-form (sym-form)
  `(get-sym-val ,sym-form))

(defun QSYM-SET-FORM (sym val-form)
  (unless *no-specials-warning (warn-if-undeclared-special-aux sym))
  (if *inside-defobfun
      (progn
	(arrange-to-link-val-set sym)
	(if *loadtime-constant-links?
	    (form-to-set-sym-val-from-link
	      sym `(delayed-expand (get-val-link ',sym))
	      val-form)
	    (form-to-set-sym-val-from-link
	      sym (global-val-link-form sym) val-form)))
      `(set-sym-val ',sym ,val-form)))

(defun SYMBOL-SET-FORM (sym-form val-form)
  `(set-sym-val ,sym-form ,val-form))

(defun SYMBOL-FREF-FORM (sym-form)
  `(get-sym-fcn ,sym-form))

(defun SYMBOL-FSET-FORM (sym-form fcn-form)
  `(set-sym-fcn ,sym-form ,fcn-form))

(defsubst subst-quote-for-fcnquote (fcn-form)
  (if (eq (car fcn-form) 'function)
      (cons 'quote (cdr fcn-form))
      fcn-form))

(defsubst constant-sym? (form)
  (and (consp form) (memq (car form) '(quote function))
       (symbolp (cadr form)) (null (cddr form))))

(defun FCNCALL-FORM (type fcn-form arg-forms &aux sym)
  (iff (not (constant-sym? fcn-form))
       `(fcncall-fcn-?sym ,type ,fcn-form ,@arg-forms)
    (setq sym (cadr fcn-form))
    (if (not *inside-defobfun)
        `(fcncall-fcn-sym ,type ,(subst-quote-for-fcnquote fcn-form)
			  ,@arg-forms)
        (form-to-fcncall-fcn-sym-from-link type sym arg-forms))))

(defun form-to-fcncall-fcn-sym-from-link (type sym arg-forms &aux fcn link-form)
  (cond ((not (eq sym *shadowed-fcn-sym))
	 (arrange-to-link-fcn-ref sym)
	 (setq link-form (if *loadtime-constant-links?
			     `(delayed-expand (get-fcn-link ',sym))
			     (global-fcn-link-form sym)))
	 (setq fcn (if *global-dispatch-fcns?
		       (get-sym-fcn-name sym)
		       'get-sym-fcn-from-link))
	 `(,type (,fcn ,link-form) ,@arg-forms))
	(t
	 (case type
	   (funcall `(funcall-shadows shadows ,@arg-forms))
	   (apply `(apply-shadows shadows ,@arg-forms))
	   (call `(call-shadows shadows ,@arg-forms))))))

(defun shadows-form (&aux fcn link-form)
  (iff (null *defobfun-obj-sym)
       '%shadowed-noop
    (arrange-to-link-sdw-ref *shadowed-fcn-sym)
    (setq link-form (if *loadtime-constant-links?
			`(delayed-expand (get-sdw-link ',*defobfun-fcn-sym))
		        (global-sdw-link-form *defobfun-fcn-sym)))
    (setq fcn (if *global-dispatch-fcns?
		  (get-sym-sdw-name *defobfun-fcn-sym)
	          'get-sym-sdw-from-link))
    `(or *shadows (,fcn ,link-form))))

; Auxiliary routines.

(defmacro def-global-val-link-form ()
  (if *global-link-symbols?
      '(defun global-val-link-form (sym)
	 (pkg-new-symbol *obf-pkg "Val-link "
			 (package-name (symbol-package sym)) ":" sym))
      '(defun global-val-link-form (sym)
	 (get-val-link sym))))

(def-global-val-link-form)

(defmacro def-global-fcn-link-form ()
  (if *global-link-symbols?
      '(defun global-fcn-link-form (sym)
	 (pkg-new-symbol *obf-pkg
			 "Fcn-link " (package-name (symbol-package sym)) ":" sym))
      '(defun global-fcn-link-form (sym)
	 (list 'get-fcn-link (list 'quote sym)))))

(def-global-fcn-link-form)

(defmacro def-global-sdw-link-form ()
  (if *global-link-symbols?
      '(defun global-sdw-link-form (sym)
	 (pkg-new-symbol *obf-pkg
			 "Sdw-link " (package-name (symbol-package sym)) ":" sym))
      '(defun global-sdw-link-form (sym)
	 (list 'get-sdw-link (list 'quote sym)))))

(def-global-sdw-link-form)

; (See DEFOBFUN, DEFOBFUN-INSTALL)
(defun arrange-to-link-val-ref (sym)
  (or (memq sym *val-ref-syms) (push sym *val-ref-syms))
  (when *global-dispatch-fcns?
    (let ((name (get-sym-val-name sym)))
      (unless (fboundp name)
	;; This isn't necessarily the right thing to set it to, but it will
	;; suppress the undef-fcn compiler warning, and will be adjusted later.
	(setf (symbol-function name) (function get-val-lookup-global-only)))))
  (def-val-link sym))

(defun def-val-link (sym &optional link &aux link-sym)
  (when *global-link-symbols?
    (setq link-sym (global-val-link-form sym))
    (unless (boundp link-sym) (proclaim (list 'special link-sym)))
    (unless (null link) (set link-sym link))
    link-sym))

(defun link-val-ref (sym &aux (link (get-val-link sym)))
  (def-val-link sym link)
  (adjust-get-val-fcn link t))

(defun arrange-to-link-val-set (sym)
  (or (memq sym *val-set-syms) (push sym *val-set-syms))
  (when *global-dispatch-fcns?
    (let ((name (set-sym-val-name sym)))
      (unless (fboundp name)
	;; This isn't necessarily the right thing to set it to, but it will
	;; suppress the undef-fcn compiler warning, and will be adjusted later.
	(setf (symbol-function name) (function set-val-lookup-global-only)))))
  (def-val-link sym))

(defun link-val-set (sym &aux (link (get-val-link sym)))
  (def-val-link sym link)
  (adjust-set-val-fcn link t))

(defun arrange-to-link-fcn-ref (sym)
  (or (memq sym *fcn-ref-syms) (push sym *fcn-ref-syms))
  (when *global-dispatch-fcns?
    (let ((name (get-sym-fcn-name sym)))
      (unless (fboundp name)
	;; This isn't necessarily the right thing to set it to, but it will
	;; suppress the undef-fcn compiler warning, and will be adjusted later.
	(setf (symbol-function name) (function fcncall-lookup-global-only)))))
  (def-fcn-link sym))

(defun def-fcn-link (sym &optional link &aux link-sym)
  (when *global-link-symbols?
    (setq link-sym (global-fcn-link-form sym))
    (unless (boundp link-sym) (proclaim (list 'special link-sym)))
    (unless (null link) (set link-sym link))
    link-sym))

(defun link-fcn-ref (sym sdw-sym fcn-sym &aux link)
  (iff (eq sym sdw-sym)
       (link-sdw-ref fcn-sym)
    (setq link (get-fcn-link sym))
    (def-fcn-link sym link)
    (adjust-get-fcn-fcn link t)))

(defun arrange-to-link-sdw-ref (sdw-sym)
  (or (memq sdw-sym *fcn-ref-syms) (push sdw-sym *fcn-ref-syms))
  (when *global-dispatch-fcns?
    (let ((name (get-sym-sdw-name *defobfun-fcn-sym)))
      (unless (fboundp name)
	;; This isn't necessarily the right thing to set it to, but it will
	;; suppress the undef-fcn compiler warning, and will be adjusted later.
	(setf (symbol-function name) (function shadow-lookup-global-only)))))
  (def-sdw-link *defobfun-fcn-sym))

(defun def-sdw-link (fcn-sym &optional sdw-link &aux link-sym)
  (when *global-link-symbols?
    (setq link-sym (global-sdw-link-form fcn-sym))
    (unless (boundp link-sym) (proclaim (list 'special link-sym)))
    (unless (null sdw-link) (set link-sym sdw-link))
    link-sym))

(defun link-sdw-ref (fcn-sym &aux (sdw-link (get-sdw-link fcn-sym)))
  (def-sdw-link fcn-sym sdw-link)
  (adjust-get-sdw-fcn (get-fcn-link fcn-sym) t))

; Interface rtns for getting things from links.

(defun get-sym-val-name (sym)
  (pkg-new-symbol *obf-pkg
		  "Get-sym-val " (package-name (symbol-package sym)) ":" sym))

(defun set-sym-val-name (sym)
  (pkg-new-symbol *obf-pkg
		  "Set-sym-val " (package-name (symbol-package sym)) ":" sym))

(defun get-sym-fcn-name (sym)
  (pkg-new-symbol *obf-pkg
		  "Get-sym-fcn " (package-name (symbol-package sym)) ":" sym))

(defun get-sym-sdw-name (sym)
  (pkg-new-symbol *obf-pkg
		  "Get-sym-sdw " (package-name (symbol-package sym)) ":" sym))

(defun form-to-get-sym-val-from-link (sym link-form)
 (if *global-dispatch-fcns?
     `(,(get-sym-val-name sym) ,link-form)
     `(get-sym-val-from-link ,link-form)))

(defun form-to-set-sym-val-from-link (sym link-form val-form)
 (if *global-dispatch-fcns?
     `(,(set-sym-val-name sym) ,link-form ,val-form)
     `(set-sym-val-from-link ,link-form ,val-form)))

(defun warn-if-undeclared-special-aux (sym)
  (declare (special *specials))
  (cond (*walkover? (when (not (or (memq sym *specials)
				   (globally-special? sym)))
		      (push sym *specials)
		      (warn-of-undeclared-special sym)))
	(*obl-compiling? (warn-if-undeclared-special sym))))

; Runtime reference interface.

; Global dispatch rtns:
(defun get-val-lookup-global-only (link)
  (binding-val (val-lookup-global-only link)))
(defun get-val-lookup-instance-bound-only (link)
  (binding-val (val-lookup-instance-bound-only link)))
(defun get-val-lookup-class-bound-only (link)
  (binding-val (val-lookup-class-bound-only link)))
(defun get-val-lookup-both-bound (link)
  (binding-val (val-lookup-both-bound link)))

; Global link (loadtime constant or symbol) rtn:
(miscdefun get-sym-val-from-link (link)
  (binding-val (val-binding-from-link link)))

; Else:
(miscdefun get-sym-val (sym)
  (let ((link (get-val-link? sym)))
	 (if link
	     (binding-val (val-binding-from-link link))
	     (symbol-value sym))))

; Global dispatch rtns:
(defun set-val-lookup-global-only (link val)
  (set-binding-val (val-lookup-global-only link) val))
(defun set-val-lookup-instance-bound-only (link val)
  (set-binding-val (val-lookup-instance-bound-only link) val))
(defun set-val-lookup-class-bound-only (link val)
  (set-binding-val (val-lookup-class-bound-only link) val))
(defun set-val-lookup-both-bound (link val)
  (set-binding-val (val-lookup-both-bound link) val))

(MISCDEFUN set-sym-val-from-link (link val)
  (set-binding-val (val-binding-from-link link) val))

(miscdefun set-sym-val (sym val)
  (let ((link (get-val-link? sym)))
    (if link
	(set-binding-val (val-binding-from-link link) val)
	(set sym val))))

(MISCDEFUN get-sym-fcn-from-link (link)
  (binding-fcn (fcn-binding-from-link link)))
; Global dispatch rtns:
(defun fcncall-lookup-global-only (link)
  (binding-fcn (fcn-lookup-global-only link)))
(defun fcncall-lookup-instance-bound-only (link)
  (binding-fcn (fcn-lookup-instance-bound-only link)))
(defun fcncall-lookup-class-bound-only (link)
  (binding-fcn (fcn-lookup-class-bound-only link)))
(defun fcncall-lookup-both-bound (link)
  (binding-fcn (fcn-lookup-both-bound link)))

(miscdefun get-sym-fcn (sym)
  (let ((link (get-fcn-link? sym)))
    (if (null link)
	(symbol-function sym)
	(binding-fcn (fcn-binding-from-link link)))))

(miscdefun set-sym-fcn (sym fcn)
  (let ((link (get-fcn-link? sym)))
    (if link
	(set-binding-fcn (fbinding-from-object link *object) fcn)
	(setf (symbol-function sym) fcn))))

(miscdefun get-?sym-fcn (?sym)
  (if (not (symbolp ?sym))
      ?sym
      (let ((link (get-fcn-link? ?sym)))
	(if (null link)
	    (symbol-function ?sym)
	    (binding-fcn (fcn-binding-from-link link))))))

; Global dispatch rtns:
(defun shadow-lookup-global-only (link)
  (sdw-lookup-global-only link))
(defun shadow-lookup-instance-bound-only (link)
  (sdw-lookup-instance-bound-only link))
(defun shadow-lookup-class-bound-only (link)
  (sdw-lookup-class-bound-only link))
(defun shadow-lookup-both-bound (link)
  (sdw-lookup-both-bound link))

(MISCDEFUN get-sym-sdw-from-link (link)
  (sdw-binding-from-link link))

(miscdefun get-sym-sdw (sym)
  (let ((link (get-sdw-link? sym)))
    (if (null link)
        (global-sdw-binding link)
        (sdw-binding-from-link link))))

(defmacro fcncall-fcn-sym (type sym &rest args)
  (if (not (eq sym *shadowed-fcn-sym))
      `(,type (get-sym-fcn ,sym) ,@args)
      `(,type (get-sym-sdw *defobfun-fcn-sym) ,@args)))

(defmacro fcncall-fcn-?sym (type sym &rest args)
  `(,type (get-?sym-fcn ,sym) ,@args))

; SDWS guaranteed nonnull here.
(defun funcall-shadows (sdws &rest args)
  (cond ((consp sdws)
	 (setq *shadows (cdr sdws))
	 (apply (binding-fcn-for-shadow (car sdws)) args))
	(t
	 (setq *shadows t)
	 (apply (global-binding-fcn-for-shadow sdws) args))))

; SDWS guaranteed nonnull here.
(defun apply-shadows (sdws &rest args)
  (cond ((consp sdws)
	 (setq *shadows (cdr sdws))
	 (apply #'apply (binding-fcn-for-shadow (car sdws)) args))
	(t
	 (setq *shadows t)
	 (apply #'apply (global-binding-fcn-for-shadow sdws) args))))

; SDWS guaranteed nonnull here.
#|(defun call-shadows (sdws &rest args)
  (cond ((consp sdws)
	 (setq *shadows (cdr sdws))
	 (apply #'call (binding-fcn-for-shadow (car sdws)) args))
	(t
	 (setq *shadows t)
	 (apply #'call (global-binding-fcn-for-shadow sdws) args))))|#

(defun call-shadows (sdws &rest args)
  (cond ((consp sdws)
	 (setq *shadows (cdr sdws))
	 (apply #'FUNcall (binding-fcn-for-shadow (car sdws)) args))
	(t
	 (setq *shadows t)
	 (apply #'FUNcall (global-binding-fcn-for-shadow sdws) args))))

;;;; HAVE, UNHAVE, FHAVE, UNFHAVE

(defun HAVE (&rest sym-val-pairs &aux link (env (own-env *object)) binding)
  (prog1
    (car sym-val-pairs)
    (nloop (stop-if (null sym-val-pairs))
	   (for sym (pop sym-val-pairs))
	   (for val (pop sym-val-pairs))
	   (check-type sym symbol)
	   (cond ((global-obj? *object) (set sym val))
		 ((and (setq link (get-val-link? sym))
		       (setq binding (binding-from-env link env)))
		  (set-binding-val binding val))
		 (t (bind-val-in-env sym val env))))))

(defun UNHAVE (&rest syms &aux link (env (own-env *object)))
  (nloop (for-in sym syms)
	 (check-type sym symbol)
	 (cond ((global-obj? *object)
		(ferror nil "~%Can't remove global variable."))
	       ((or (null (setq link (get-val-link? sym)))
		    (null (binding-from-env link env))) )
	       (t (unbind-val-in-env sym env))))
  (car syms))

(defun FHAVE (&rest sym-fcn-pairs)
  (nloop (stop-if (null sym-fcn-pairs))
	 (for sym (pop sym-fcn-pairs))
	 (for fcn (pop sym-fcn-pairs))
	 (check-type sym symbol)
	 (set-fbinding-in-obj sym fcn *object))
  (car sym-fcn-pairs))

(defun set-fbinding-in-obj (sym fcn obj &aux link fbinding (env (own-env obj)))
  (if (global-obj? obj)
      (setf (symbol-function sym) fcn)
    (if (pkg-unshadowable? sym)
	(ferror nil
		"Attempt to shadow the definition of ~s, an unshadowable symbol."
		sym)
	(if (and (setq link (get-fcn-link? sym))
		 (setq fbinding (fbinding-from-env link env)))
	    (set-binding-fcn fbinding fcn)
	    (bind-fcn-in-env sym fcn env)))))

(defun UNFHAVE (&rest syms &aux link (env (own-env *object)))
  (nloop (for-in sym syms)
	 (check-type sym symbol)
	 (cond ((global-obj? *object)
		(ferror nil "~%Can't remove global procedure."))
	       ((or (null (setq link (get-fcn-link? sym)))
		    (null (fbinding-from-env link env))) )
	       (t (unbind-fcn-in-env sym env))))
  (car syms))

(defmacro OBJ-LET-GLOBALLY (bindings &body body)
  (let ((gen (gensym)) (syms (mapcar #'car bindings)))
    `(let ((,gen (list ,@syms)))
       (unwind-protect (progn (psetq ,@(apply #'append bindings))
			      ,@body)
	 (mapc #'(lambda (sym old) (set sym old)) ',syms ,gen)))))

;;;; DEFOBFUN

#+(or lambda symbolics)
(defprop defobfun "Object Function" si:definition-type-name)

(defun tree-memq (thing tree)
  (or (eq thing tree)
      (and (consp tree)
	   (or (tree-memq thing (car tree))
	       (tree-memq thing (cdr tree))))))

(defun dcl-fcn-parent-form (parent-sym internal-sym)
  #-(or lambda symbolics) (declare (ignore parent-sym internal-sym))
  #+(or lambda symbolics)
  (unless (eq parent-sym internal-sym)
    `((declare (sys:function-parent ,parent-sym defobfun)))))

(defmacro defobfun-install-macro (obj-sym fcn-sym sdw-sym internal-sym)
  `(defobfun-install ,obj-sym ',fcn-sym ',sdw-sym #',internal-sym
		     ',*val-ref-syms ',*val-set-syms ',*fcn-ref-syms))

;(DEFOBFUN <fcn-sym> ...) or (DEFOBFUN (<fcn-sym> <obj-form>) ...)
;Note that DEFOBFUN >always< compiles the definition if *WALKOVER? is null.
(defmacro DEFOBFUN (fcn-sym vars &body body
		    &aux obj-sym shadowed-sym internal-sym lets)
  (setq internal-sym fcn-sym)
  (multiple-value-setq (vars lets) (hack-keyword-default-pkg vars))
  (unless (null lets) (setq body `((let* ,lets ,@body))))
  (when (consp fcn-sym)
    (setq obj-sym (second fcn-sym)
	  fcn-sym (first fcn-sym)
	  internal-sym (pkg-new-symbol (find-package 'obf)
				       "(" fcn-sym " " obj-sym ")")))
  (setq shadowed-sym (new-symbol "SHADOWED-" fcn-sym))
  (if *walkover?
      (let ((*defobfun-fcn-sym fcn-sym)
	    (*shadowed-fcn-sym shadowed-sym)
	    (*inside-defobfun t) (*defobfun-obj-sym obj-sym)
	    *val-ref-syms *val-set-syms *fcn-ref-syms)
	`(progn 'compile
	    (record-source-file-name ',fcn-sym 'defobfun t)
	    ,(if (or (tree-memq shadowed-sym vars)
		     (tree-memq shadowed-sym body))
		 (if (application-in-arglist? vars)
;		     #-lambda
		     `(defun ,internal-sym (&rest args
					    &aux (shadows ,(shadows-form)))
			,@(dcl-fcn-parent-form fcn-sym internal-sym)
			(apply #',(walkover `(lambda (shadows ,@vars) ,@body))
			       shadows args))
;		     #+lambda
;		     `(defun ,internal-sym (&rest args
;					    &aux (shadows ,(shadows-form)))
;			args
;			,@(dcl-fcn-parent-form fcn-sym internal-sym)
;			,(walkover `(destructuring-bind ,vars args ,@body)))
		     (walkover `(defun ,internal-sym ,vars
				  ,@(dcl-fcn-parent-form fcn-sym internal-sym)
				  (let ((shadows ,(shadows-form)))
				    ,@body))))
		 (walkover `(defun ,internal-sym ,vars
			      ,@(dcl-fcn-parent-form fcn-sym internal-sym)
			      ,@body)))
	    (defobfun-install ,obj-sym ',fcn-sym ',shadowed-sym #',internal-sym
			      ',*val-ref-syms ',*val-set-syms ',*fcn-ref-syms)))
      `(compiler-let ((*defobfun-fcn-sym fcn-sym)
		      (*shadowed-fcn-sym ',shadowed-sym)
		      (*inside-defobfun t) (*defobfun-obj-sym obj-sym)
		      (*val-ref-syms nil) (*val-set-syms nil) (*fcn-ref-syms nil))
	 (progn 'compile
	    (record-source-file-name ',fcn-sym 'defobfun t)
	    ,(if (or (tree-memq shadowed-sym vars)
		     (tree-memq shadowed-sym body))
		 (if (application-in-arglist? vars)
		     #-lambda
		     `(defun ,internal-sym (&rest args
					    &aux (shadows ,(shadows-form)))
			,@(dcl-fcn-parent-form fcn-sym internal-sym)
			(apply #'(lambda (shadows ,@vars) ,@body)
				shadows args))
		     #+lambda
		     `(defun ,internal-sym (&rest args
					    &aux (shadows ,(shadows-form)))
			args
			,@(dcl-fcn-parent-form fcn-sym internal-sym)
			(destructuring-bind ,vars args ,@body))
		     `(defun ,internal-sym ,vars
			,@(dcl-fcn-parent-form fcn-sym internal-sym)
			(let ((shadows ,(shadows-form))) ,@body)))
		 `(defun ,internal-sym ,vars
		    ,@(dcl-fcn-parent-form fcn-sym internal-sym)
		    ,@body))
	    (eval-when (eval) (compile ',internal-sym))
	    (defobfun-install-macro
	      ,obj-sym ,fcn-sym ,shadowed-sym ,internal-sym)))))


(defun defobfun-install (obj fcn-sym sdw-sym fcn val-refs val-sets fcn-refs)
  (mapc #'link-val-ref val-refs)
  (mapc #'link-val-set val-sets)
  (mapc #'(lambda (sym) (link-fcn-ref sym sdw-sym fcn-sym)) fcn-refs)
  (when obj
    (check-type obj obj)
    (set-fbinding-in-obj fcn-sym fcn obj))
  nil)

(defun application-in-arglist? (arglist)
  (some #'(lambda (form) (and (consp form) (consp (cadr form))))
	arglist))

; Code to support &KEY* for DEFOBFUN, and plain &KEY for deficient Commonlisps.

(defmacro cons-end (thing-form list-form)
  `(if (null ,list-form)
       (setf ,list-form (list ,thing-form))
       (rplacd (last ,list-form) (list ,thing-form))))

(defun hack-keyword-default-pkg (lambda-list &aux new)
  (setq new  (nloop (stop-if (null lambda-list))
	       (for thing (pop lambda-list))
	       (if (eq thing '&key*)
		   (return (nconc new
				  (cons '&key (collect-keyword-args lambda-list))
				  '(&allow-other-keys)))
		   (cons-end thing new))
	       (finally new)))
  (hack-manual-&keys new))

(defmacro defun-hack-manual-&keys ()
 (if (not *manual-&keys?) '(defun hack-manual-&keys (args) args)
; 2 vals returned: new arglist, list of LET* var forms.
 '(defun hack-manual-&keys (args &aux keys restarg new keywds keyargs inits)
  (setq keys (cdr (memq '&key args)))
  (iff (null keys)
      args
    (setq restarg (cdr (memq '&rest args)))
    (if (null restarg)
	(progn
	  (setq restarg 'restarg)
	  (nloop (stop-if (null args)) (for arg (pop args))
		 (stop-if (eq arg '&key))
		 (push arg new))
	  (push '&rest new) (push 'restarg new))
	(progn
	 (setq restarg (car restarg))
	 (nloop (stop-if (null args)) (for arg (pop args))
		(stop-if (eq arg '&key))
		(push arg new))))
    ;; NEW now holds (in reverse order) args up to, but not including, &KEY, and
    ;;  ending with &REST ,RESTARG.
    (nloop (with ks keys) (stop-if (null ks)) (with aux nil)
	   (for key (pop ks))
	   (if (eq key '&aux)
	       (setq aux t  key (pop ks)))
	   (cond ((eq key '&allow-other-keys) nil) ;Punt this, no more &KEY.
		 ((memq key lambda-list-keywords)
		  ;; Shouldn't really be any keywd here, but if so, pass the buck.
		  (push key new) (unless (null ks) (push (pop ks) new)))
		 (aux (cond ((listp key) (push (car key) keyargs)
					 (push (cadr key) inits))
			    (t (push key keyargs) (push nil inits)))
		      (push nil keywds))
		 ((listp key)
		  (push (cadr key) inits)
		  (cond ((listp (car key)) (push (caar key) keywds)
					   (push (cadar key) keyargs))
			(t (push (car key) keyargs)
			   (push (intern (symbol-name (car key)) 'keyword)
				 keywds))))
		 (t (push key keyargs) (push nil inits)
		    (push (intern (symbol-name key) 'keyword) keywds))))
    (values (nreverse new)
	    (nconc (if (eq restarg 'restarg)
		       (list '(arg nil))
		       (list '(arg nil) `(restarg ,restarg)))
		   (nloop (for-in arg (nreverse keyargs))
			  (for-in word (nreverse keywds))
			  (for-in init (nreverse inits))
			  (collect
			    `(,arg ,(if (null word)
					init
				        `(extract ,word ,init)))))))))))

(defun-hack-manual-&keys)

(defmacro extract (key init)
  `(if (setq arg (extract-key ',key restarg))
       (car arg)
       ,init))

(defun extract-key (key args)
  (nloop (stop-if (null args)) (for key1 (pop args))
	 (if (eq key1 key) (return args))
	 (pop args)))

; Be sure that this conses a fresh list, because its caller NCONC's it.
(defun collect-keyword-args (lambda-list &aux new)
  (nloop (stop-if (null lambda-list))
	 (for thing (pop lambda-list))
	 (cond ((eq thing '&aux)
		(return (nconc new (cons '&aux lambda-list))))
	       ((memq thing lambda-list-keywords) (cons-end thing new))
	       ((symbolp thing)
		(cons-end `((,thing ,thing)) new))
	       ((symbolp (car thing))
		(cons-end `((,(car thing) ,(car thing)) ,@(cdr thing)) new))
	       (t (cons-end thing new)))
	(finally new)))


(defun new-symbol (&rest strings)
  (intern (apply #'string-append strings)))

(defun pkg-new-symbol (pkg &rest strings)
  (intern (apply #'string-append strings) pkg))


;;;; ASK, KINDOF

(miscdefun check-obj (arg)
  (or (null arg) (check-type arg obj "an object")))

; (Returns OB.)
(miscdefun set-obj (ob)
  (if (null ob)
      (setq ob *internal-global-obj)
      (check-type ob obj "an object"))
  (setq	*instance-envs (obj-instance-envs ob)
	*val-class-disp (obj-val-class-disp ob)
	*fcn-class-disp (obj-fcn-class-disp ob)
	*sdw-class-disp (obj-sdw-class-disp ob))
  (setq *object ob))

(defmacro ASK (obj-form &body body)
  (if *walkover?
      `(ask-aux ,obj-form ,@(if *obl-walking?
				body
			        (walkover ; (if (null (cdr body))
					   ;   body
				  `((progn ,@body)))));)
      `(obl (ask-aux ,obj-form ,@body))))

(defmacro ask-aux (obj-form &body body)
  `(let ((obj ,obj-form)
	 *object *instance-envs *val-class-disp *fcn-class-disp *sdw-class-disp)
     (set-obj obj)
     ,@(or body (list nil))))

(defmacro ASK-FUNCALL (obj sym &rest args)
 `(let ((values (list ,@args)))
    (ask ,obj (apply ,sym values))))

(defmacro MAPC-ASK (objs &body body)
  `(mapc #'(lambda (obj) (ask obj ,@body)) ,objs))

(defmacro MAP-ASK (objs &body body)
  `(map #'(lambda (obj) (ask obj ,@body)) ,objs))

(defmacro MAPCAR-ASK (objs &body body)
  `(mapcar #'(lambda (obj) (ask obj ,@body)) ,objs))


(defun TALKTO (&optional obj)
  (iff (not (null obj))
       (set-obj obj)
    (set-obj obj)
    nil))

; KINDOF

(defun KINDOF (&rest objs)
  (%kindof t nil nil (copy-list objs)))

(defun MAKE-OBJ (&rest objs)
  (%kindof t nil nil (copy-list objs)))

(defun refresh-subobjs (obj &aux (subobjs (specializations obj)))
  (mapc #'refresh-obj subobjs)
  (mapc #'refresh-subobjs subobjs))

(defun REMAKE-OBJ (obj &rest bases)
  (check-obj obj)
  (let ((new-envs (flatten-objs-envs bases))
	(old-envs (obj-envs obj)))
    (unless (and (not (null old-envs))	; No new obj if still the same.
		 (equal (cdr old-envs) new-envs))
      ;;Replace old obj but keep the same 1st env.
      (%kindof nil (obj-class? obj) (not (obj-class? obj)) (copy-list bases)
	       (cons (own-env obj) new-envs)
	       obj)
      (refresh-subobjs obj)))
  obj)

(defun refresh-obj (obj)
  (apply #'remake-obj obj (obj-superiors obj)))

(defmacro apply-fcn-sym (fcn-sym args-form)
  `(apply (get-sym-fcn ,fcn-sym) ,args-form))

(defun ONEOF (class-obj &rest exist-args &aux instance-obj)
  (setq instance-obj (%kindof t nil t (ncons class-obj)))
  (ask-aux instance-obj
    (apply-fcn-sym 'exist exist-args))
  instance-obj)

(defvar *next-obj-index 0)

(defun next-obj-index ()
  (incf *next-obj-index))

;OBJS is list of superclasses (for KINDOF etc) or NIL (for nonstd. obj creation).
;ENVS should be environments to make new obj from,
; defaulting to flattening of OBJS' environments.
;An object made by KINDOF is an instance if any of its superclasses is,
; or if there are no superclasses.  Otherwise it's a class.
;(An object made by ONEOF is an instance. An object made by DEFKIND is a class.
; An object made by REMAKE-OBJ keeps its old status.)
(defun %kindof (own? force-class? force-instance? objs
		&optional (envs (progn (mapc #'check-obj objs)
				       (flatten-objs-envs objs)))
		          (oldobj nil)	; Recycle old obj-defstruct if provided.
		&aux class? env nenvs obj)
  (iff (not own?)
       (setq nenvs envs)
    (setq env (make-environment))
    (setq nenvs (cons env envs)))
  (iff (null oldobj)
       (progn (setq obj (%make-obj nenvs objs))
	      (setf (obj-index obj) (next-obj-index)))
    (setq obj oldobj)
    (setf (obj-envlist obj) nenvs)
    (setf (obj-superiors obj) objs))
  (when own?
    (setf (env-object env) obj))
  (setq class?
	(or force-class?
	    (and (not force-instance?)
		 (not (null objs))
		 (every #'(lambda (obj) (obj-class? obj))
			objs))))
  (setf (obj-class? obj) class?)
  (if class?
      (setup-class-disp-tables-for-object obj nenvs)
      (setup-disp-tables-for-instance-object obj objs nenvs))
  obj)

;Takes list of objects.  Returns list of environments.
(defun flatten-objs-envs (objs &aux temp new-envs)
  (nloop (for-in obj objs)
	 (nloop (for-in env (obj-envs obj))
		(push env temp)))
  (nloop (for-in env temp)
	 (or (memq env new-envs)
	     (push env new-envs)))
  new-envs)

(defun flush-env-memos-for-conversion (env class-to-instance?)
  (bindings-map #'(lambda (binding &aux (link (nonglobal-binding-link binding)))
		    (if class-to-instance?
			(first-instance-val-binding link)
		        (first-class-val-binding link))
		    (flush-val-memo link))
		(env-val-bindings env))
  (bindings-map #'(lambda (binding &aux (link (nonglobal-binding-link binding)))
		    (if class-to-instance?
			(first-instance-fcn-binding link)
		        (first-class-fcn-binding link))
		    (flush-fcn-memo link))
		(env-fcn-bindings env)))

(defun record-class-usage (obj class-envs &optional sharing-class-disps?)
  (nloop
    (for-in env class-envs)
    ;; If env was previously instance-only, punt memoization of its bindings.
    (when (null (env-class-objs env))
      (flush-env-memos-for-conversion env nil))
    (if (not sharing-class-disps?)
	(unless (memq obj (env-class-objs env))
	  (push obj (env-class-objs env))))))

(defun setup-class-disp-tables-for-object (obj envs
				          &aux (val-disp (make-class-disp-table))
				               (fcn-disp (make-class-disp-table))
				               (sdw-disp (make-class-disp-table)))
  (record-class-usage obj envs)
  (setf (obj-val-class-disp obj) val-disp)
  (setf (obj-fcn-class-disp obj) fcn-disp)
  (setf (obj-sdw-class-disp obj) sdw-disp)
  (nloop (for-in env (reverse envs))
	 (bindings-map #'(lambda (val-binding)
			   (class-insert (nonglobal-binding-link val-binding)
					  val-binding val-disp))
		       (env-val-bindings env))
	 (bindings-map #'(lambda (fcn-binding &aux link old)
			   (setq link (nonglobal-binding-link fcn-binding)
				 old (class-lookup link fcn-disp))
			   (class-insert link fcn-binding fcn-disp)
			   (setq link (get-sdw-link? (link-symbol link)))
			   (when link
			     (class-insert
			       link (if old
					(cons old (class-lookup link sdw-disp))
				        (global-FCN-binding link))
			       sdw-disp)))
		       (env-fcn-bindings env))))

(defun setup-disp-tables-for-instance-object (obj objs nenvs
					      &aux (final-class-objs objs)
					           class-envs)
  ;; Find longest tail of OBJS consisting of 1 (or 0 if none) instance obj
  ;; followed by 0 or more class objs.
  (nloop (for-on ?finals objs)
	 (for obj (car ?finals))
	 (if (not (obj-class? obj))
	     (setq final-class-objs ?finals)))
  ;; All envs before 1st final-class-obj's 1st class env are instance envs.
  ;; Remainder are class-envs.
  (setf (obj-instance-envs obj)
	(nloop (with new-class (car final-class-objs))
	       (with final-env
		     (if final-class-objs
			 (let ((last-instance (car (last (obj-instance-envs
							   new-class)))))
			   (if (null last-instance)
			       (own-env (car final-class-objs))
			       (cadr
				 (memq last-instance (obj-envlist new-class)))))))
	       (for-on cenvs nenvs)
	       (for env (car cenvs))
	       (stop-if (eq env final-env))
      ;;; If env was previously class-only, punt memoization of its bindings.
	       (when (null (env-instance-objs env))
		 (flush-env-memos-for-conversion env t))
	       (unless (memq obj (env-instance-objs env))
		 (push obj (env-instance-objs env)))
	       (collect env)
	       (finally (setq class-envs cenvs))))
  (cond ((= (length final-class-objs) 1)
     ;;; Special case: if exactly one class obj, just share its class-disp tables.
	 (setf (obj-val-class-disp obj)
	       (obj-val-class-disp (car final-class-objs)))
	 (setf (obj-fcn-class-disp obj)
	       (obj-fcn-class-disp (car final-class-objs)))
	 (setf (obj-sdw-class-disp obj)
	       (obj-sdw-class-disp (car final-class-objs)))
	 (record-class-usage obj class-envs t))
	(t ;; Otherwise make fresh disp tables.
	 (setup-class-disp-tables-for-object obj class-envs))))

(defun BASE-OBJS (&optional (obj *object))
  (if (global-obj? obj)
      nil
      (obj-superiors obj)))

(defun INHERITED-OBJS (&optional (obj *object))
  (if (global-obj? obj)
      nil
      (mapcar #'env-object (cdr (obj-envlist obj)))))

(defun OBJ-EQUAL (obj1 obj2)
  (or (eq obj1 obj2)
      (and (object? obj1) (object? obj2)
	   (equal (obj-envs obj1) (obj-envs obj2)))))

;;;; Toplevel, Print-rep

; Toplevel

(defun obj-listener-loop ()
  #+lambda (micro-load)
  (ask-aux nil
    (read-compile-print-loop)))

;Use this to make toplev forms (other than ASK, DEFOBFUN) work in a file.
(defmacro obl (form &body forms)
  (if *walkover?
      (if (null *obl-walking?)
	  (if forms (walkover `(progn ,form ,@forms))
	      (walkover form))
	  (if (null forms) form `(progn ,form ,@forms)))
      (if (or *obl-compiling? *inside-defobfun
 );	      #+lambda compiler:qc-file-in-progress)
	  (if (null forms) form `(progn ,form ,@forms))
	  `(progn 'compile
		  (inhibiting-fdefine-warnings
		    (let ((*no-specials-warning t))
		      (defobfun %run-dummy () ,form ,@forms))
		    (%run-dummy))))))

(defun read-compile-print-loop ()
  (catch 'exit
    (error-restart-loop ((#+lambda error #+symbolics dbg:error
			  #+(or lambda symbolics) sys:abort)
			 "Return to obLisp read-compile-print loop")
      (catch 'continue
	(print-apply-compile-read #+lambda "~%~a-> "
				  #+symbolics   "~a-> ")))))

(defun print-apply-compile-read (&optional prompt &aux vals)
  (setq + (if prompt
	      (prompt-and-read :expression prompt
			       (if (global-obj? *object)
				   "" (if (own? 'obj-name)
					  (get-sym-val 'obj-name)
					*object)))
	      (read)))
  (if (and (consp +) (memq (car +) '(defobfun defclassvar defclassvars
				      definstancevar definstancevars defkind)))
      (setq vals (multiple-value-list (eval +)))
      (setq vals (multiple-value-list (eval (LET ((*NO-SPECIALS-WARNING T))
					      (walkover +))))))
  (setq * (car vals))
  (mapc #'print vals))

(defun undefined-function-error ()
  (ferror nil "~%Undefined ObjectLisp function"))


; Print-rep

(SHADOW 'PRINT-OBJECT)
(defun print-object (obj)
  (ask-aux obj
    (apply-fcn-sym 'print-self nil)))

#+lambda
(defparameter *micro-loaded? nil)

#+lambda
(defun micro-load ()
 (if *microcode? (unless *micro-loaded?
		   (apply 'compiler:ma-load *microcode-fcns)
		   (mapc 'compiler:enable-micro-misc *misccode-fcns)))
 (setq *micro-loaded? t))

#+lambda (micro-load)
