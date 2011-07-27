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

(DEFPACKAGE :OBJ
  (:USE :CL)
  (:SHADOW :PRINT-OBJECT :CLASS-NAME)
  (:SHADOWING-IMPORT-FROM :SB-CLTL2 :COMPILER-LET)
  (:EXPORT ASK TALKTO KINDOF ONEOF MAKE-OBJ REMAKE-OBJ
           DEFOBFUN &KEY* DEFKIND DEFINE-KIND DEFCLASSVAR
           DEFINSTANCEVAR
           DEFCLASSVARS DEFINSTANCEVARS UNDEF-INSTANCEVAR
           INSTANCEVAR-DEFS
           HAVE UNHAVE FHAVE UNFHAVE
           ASK-FUNCALL MAPC-ASK MAPCAR-ASK MAP-ASK
           OBJECT? OBJ-EQUAL CURRENT-OBJ EXIST SHADOWED-EXIST
           OBJ-LET-GLOBALLY OBJ-LISTENER-LOOP OBL
           OBJ-NAME CLASS-NAME
           BASE-OBJS INHERITED-OBJS
           WHAT SHOW SHOW-ALL SHOW-VALS
           OWN OWN? WHERE THERE? MAPC-OWN
           MAPCAR-OWN OWN
           FOWN FOWN? FWHERE FTHERE? FDOC
           MAPC-FOWN MAPCAR-FOWN
           SPECIALIZATIONS PRINT-SELF
           ))


(DEFPACKAGE :OBF)

(in-package "OBJ")

#+symbolics (scl:deff deff #'scl:deff)
#+symbolics (deff pkg-find-package #'find-package)
#+symbolics (import '(zl:%pointer scl:defsubst scl:fdefine scl:defprop
		      scl:prompt-and-read scl:record-source-file-name
		      scl:without-interrupts scl:locf scl:location-boundp
		      scl:ferror scl:string-append scl:error-restart-loop))




;; patch
(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (SETF (FDEFINITION 'SPECIAL-FORM-P)
        (FDEFINITION 'SPECIAL-OPERATOR-P)))

;;;; Implementation parameters

; Some of these paramaters require recompiling ObjectLisp (in some cases, in a
; world that can have no ObjectLisp user code loaded yet), and/or recompiling all
; ObjectLisp user code, if the parameter changes.
; So make up your mind in advance.

; *LINKS-IN-CELLS? is nonnull iff this impl's lookup links are kept directly in
; val/fcn cells.  This requires runtime hooks to intercept all variable and
; function references.  These hooks are not specified yet.  Normally NIL.
(defparameter *links-in-cells? NIL)

; *GLOBAL-DISPATCH-FCNS? controls which of two ways a symbol's lookup dispatches
; on its binding status (global-only, instance-bound, etc).
; See GET-SYM-VAL, GET-SYM-VAL-FROM-LINK, etc.
; Must be NIL if *LINKS-IN-CELLS?.  Otherwise, it's a spacetime tradeoff: save
; time if T, save space if NIL.  Normally T.
(defparameter *global-dispatch-fcns? (if *links-in-cells? NIL
				       T));; Set this according to spacetime pref.

; *WALKOVER? is null iff either: the implementation's interception of free
; references is done from the compiler rather than by walking the code first;
; or, link structures are kept in val/fcn cells.  If nonnull, ObjectLisp code
; gets codewalked to transform free refs into object lookups.
(defparameter *walkover? (not *links-in-cells?))

; *MANUAL-&KEYS? should be nonnull for any Commonlisp implementation that fails
; to support the full &key facility, including the ((:<key> <arg>) <init>) form.
(defparameter *manual-&keys? nil)

; Set *LOADTIME-CONSTANT-LINKS? to T for any implementation that provides
;  DELAYED-EXPAND.
(defvar *loadtime-constant-links? #+lambda t #-lambda nil)

; DELAYED-EXPAND EVAL's FORM at load/eval time, & quotes the result.
#+lambda
(defmacro delayed-expand (form)    ; See sys:examples;squid, si::xr-#\,-macro
  (if compiler::qc-file-in-progress
      (if (eq compiler::qc-tf-output-mode 'compiler::compile-to-core)
	  (list 'quote (let ((default-cons-area working-storage-area))
			 (eval form)))
	  (list 'quote (cons compiler::eval-at-load-time-marker form)))
      (list 'quote (eval form))))

; Controls whether compiletime-constant link reference gets a global symbol, or
; goes thru plist.  T to save time, NIL to save space.
; (Always NIL if loadtime-constant links are used.)
(defparameter *global-link-symbols? (if *loadtime-constant-links? nil T))

; T iff fcn %POINTER is defined.
(defparameter *%pointer? #+(or lambda symbolics) t #-(or lambda symbolics) nil)

; *LOCF? is null unless the implementation supports locatives, including:
;  (LOCF (SYMBOL-VALUE/FUNCTION <form>)), LOCATION-BOUNDP,
;  and CAR/RPLACA of locatives.
(defparameter *locf? #+(or lambda symbolics) t #-(or lambda symbolics) nil)

(defmacro unless-defined (def name args &body body)
  (unless (or (fboundp name)
	      (special-form-p name)
	      (macro-function name))
    `(,def ,name ,args ,@body)))

;;;; Implementations that can provide real versions of the following should do so:

(defmacro INHIBITING-FDEFINE-WARNINGS (&body body)
  #+(or lambda symbolics)
  `(let ((#+lambda inhibit-fdefine-warnings
	  #+symbolics scl:inhibit-fdefine-warnings t))
     ,@body)
  #-(or lambda symbolics) body)

(defun GLOBALLY-SPECIAL? (sym)	; No commonlisp predicate for this!
  #+(or lambda symbolics) (get sym 'special)
  #-(or lambda symbolics)
  (eval `(flet ((sym () ,sym))
	   (let ((,sym 'sym))
	     (and (boundp ',sym) (eq (sym) ,sym))))))

(defun WARN-OF-UNDECLARED-SPECIAL (sym)
  (DECLARE (IGNORE SYM))
  #+lambda (unless compiler::inhibit-special-warnings
	     (compiler::warn 'compiler::free-variable :missing-declaration
			     "The variable ~S is used free; assumed special."
			     sym))
  #+symbolics
  (compiler::phase-1-warning
    "The variable ~S is unknown and has been declared SPECIAL" sym)
  #-(or lambda symbolics) NIL)

(defun WARN-IF-UNDECLARED-SPECIAL (sym)
  (DECLARE (IGNORE SYM))
  #+lambda (compiler::makespecial sym)
  #+symbolics (compiler::lookup-variable sym)
  #-(or lambda symbolics) NIL)

(defmacro FAST-AREF (array &rest indices)
  `(aref ,array ,@indices))  ;(Must be SETFable)

(unless-defined defmacro ERROR-RESTART-LOOP ((conditions prompt) &body body)
  (declare (ignore conditions prompt))
  `(loop ,@body))

(defmacro ATOMICALLY (&rest body)
  `(#+(or lambda symbolics) without-interrupts
    #-(or lambda symbolics) progn
     ,@body))

(unless-defined defmacro FERROR (cond &rest x)
  (if cond
      `(error ,cond ,@x)
      `(error ,@x)))

(unless-defined defun RECORD-SOURCE-FILE-NAME (&rest ignore) ignore t)

(unless-defined defun PROMPT-AND-READ (option format-str &rest prompts)
  (declare (ignore option))
  (apply #'format t format-str prompts)
  (read))

(unless-defined defmacro DEFSUBST (name args &body body)
  `(defun ,name ,args
     (declare (inline ,name))
     ,@body))

(unless-defined defmacro PKG-FIND-PACKAGE (&rest stuff)
  `(find-package ,@stuff))

(unless-defined defun STRING-APPEND (&rest strings)
  (apply #'concatenate 'string
	 (mapcar #'(lambda (str?)
		     (if (symbolp str?)
			 (symbol-name str?)
			 str?))
		 strings)))

(unless-defined defmacro REMQ (thing list)
  `(remove ,thing ,list))

(unless-defined defmacro ASSQ (thing list)
  `(assoc ,thing ,list))

(unless-defined defmacro RASSQ (thing list)
  `(rassoc ,thing ,list))

(unless-defined defmacro MEMQ (thing list)
  `(member ,thing ,list))

(unless-defined defmacro NCONS (thing)
  `(cons ,thing nil))



(defmacro iff (test then &body else)
  `(if ,test ,then ,@(if (null (cdr else))
			 else
			 `((progn ,@else)))))

(defun divide (num &rest nums)
  (floor (apply #'/  num nums)))

(defmacro quote-ignore (&rest things)
  things
  nil)

;;;; Variables for walker/compiler interception

(defparameter *shadowed-fcn-sym nil) ;Bound to SHADOWED-<foo> in DEFOBFUN (<foo>
(defparameter *defobfun-fcn-sym nil) ;Bound to <foo> inside DEFOBFUN (<foo>
(defparameter *inside-defobfun nil)  ;Bound to T inside DEFOBFUN.
(defparameter *defobfun-obj-sym nil) ;Bound to <sym> in (DEFOBFUN (<foo><sym>)...)
                                     ;Bound to NIL in (DEFOBFUN <foo> ...)

(defparameter *obf-pkg (find-package 'obf))

(defvar *val-ref-syms nil)
(defvar *val-set-syms nil)
(defvar *fcn-ref-syms nil)

;;;; LAMBDA / 3600 compiler hacking

;This gets defined in COMPILE.LISP for lambda/symbolics machines:
#+(or lambda symbolics)
(defparameter *obl-compiling? nil)

#+lambda (defparameter *microcode? nil)
#+lambda (if *microcode? (make-system 'micro-compilation-tools :noconfirm))
#+lambda (defparameter *subst-if-not-microcoding? nil)


;;;; Lambda microcompilation tools

;see SYS:MICRO-COMPILER;GJC-TESTS.LISP
#+lambda
(defun microinit ()
  (makunbound '*microcode-fcns)
  (makunbound '*misccode-fcns)
  (makunbound '*next-misc-opcode)
  (makunbound '*misc-opcodes))
#+lambda
(defparameter *microcode-fcns
	'(check-obj set-obj
	  get-sym-val-from-link set-sym-val-from-link
	  get-sym-fcn-from-link
	  get-sym-fcn get-?sym-fcn set-sym-fcn get-sym-fcn set-sym-val
	  get-sym-val
          unbind-fcn-in-env
	  unbind-val-in-env bind-fcn-in-env bind-val-in-env
	  revise-fcn-class-disps-for-env revise-val-class-disps-for-env
	  revise-fcn-class-disp-for-obj revise-val-class-disp-for-obj
	  fbinding-from-object
	  table-remove insert insert-new lookup alist-insert
          alist-insert-new alist-to-hash
	  hash-remove hash-insert-new double-table-size make-hash-cons
	  val-lookup-instance-bound-only val-lookup-class-bound-only
	  val-lookup-both-bound fcn-lookup-both-bound
	  fcn-lookup-instance-bound-only fcn-lookup-class-bound-only
	  val-lookup-global-only fcn-lookup-global-only
	  first-class-val-binding first-instance-val-binding
	  first-class-fcn-binding first-instance-fcn-binding
	  get-sym-sdw get-sym-sdw-from-link sdw-lookup-global-only
	  sdw-lookup-instance-bound-only sdw-lookup-class-bound-only
	  sdw-lookup-both-bound
	  ))
#+lambda
(defparameter *misccode-fcns
	'(get-sym-fcn-from-link
	  get-sym-val-from-link set-sym-val-from-link
	  check-obj set-obj get-?sym-fcn
	  get-sym-fcn set-sym-fcn get-sym-fcn set-sym-val
	  get-sym-val
	  val-lookup-instance-bound-only val-lookup-class-bound-only
	  val-lookup-both-bound fcn-lookup-both-bound
	  fcn-lookup-instance-bound-only fcn-lookup-class-bound-only
	  val-lookup-global-only fcn-lookup-global-only
	  get-sym-sdw get-sym-sdw-from-link sdw-lookup-global-only
	  sdw-lookup-instance-bound-only sdw-lookup-class-bound-only
	  sdw-lookup-both-bound
	  ))

#+lambda
(defparameter *misc-opcodes
   '((get-sym-val . #o1700) (set-sym-val . #o1701)
     (sdw-lookup-both-bound . #o1702) (set-sym-fcn . #o1703)
     (get-sym-fcn . #o1704)
     (get-?sym-fcn . #o1705)
     (get-sym-sdw . #o1706) (check-obj . #o1707)
     (set-sym-val-from-link . #o1710)
     (get-sym-val-from-link . #o1711)
     (get-sym-fcn-from-link . #o1712)
     (val-lookup-instance-bound-only . #o1713)
     (val-lookup-class-bound-only . #o1714)
     (val-lookup-both-bound . #o1715) (fcn-lookup-both-bound . #o1716)
     (fcn-lookup-instance-bound-only . #o1717)
     (fcn-lookup-class-bound-only . #o1720)
     (val-lookup-global-only . #o1721) (fcn-lookup-global-only . #o1722)
     (set-obj . #o1723)
     (get-sym-sdw-from-link . #o1724) (sdw-lookup-global-only . #o1725)
     (sdw-lookup-instance-bound-only . #o1726)
     (sdw-lookup-class-bound-only . #o1727)
     ))
#+lambda
(defun pair (list1 list2)
  (mapcar #'list list1 list2))
#+lambda
(defmacro microdefun (name args &body body)
  (or (memq name *microcode-fcns)
      (ferror "~%Function ~a is being microcoded, but is not on *MICROCODE-FCNS."
	      name))
  `(progn 'compile
     ,@(cond (*microcode?
	      `((compiler:define-micro-properties ,name ,args)
		(defun ,name ,args ,@body)))
	     (*subst-if-not-microcoding?
	      (let ((gens (loop for arg in args collect (gensym))))
		`((defsubst ,name ,gens
		    (let ,(pair args gens) ,@body)))))
	     (t `((defun ,name ,args ,@body))))))
#+lambda
(defmacro miscdefun (name args &body body)
  (or (memq name *microcode-fcns)
      (ferror "~%Function ~a is being microcoded, but is not on *MICROCODE-FCNS."
	      name))
  (or (memq name *misccode-fcns)
      (ferror "~%Function ~a is being misc-coded, but is not on *MISCCODE-FCNS."
	      name))
  `(progn 'compile
     ,@(cond (*microcode?
	      `((compiler:define-micro-properties ,name ,args
		  :opcode ,(cdr (assq name *misc-opcodes)))
		(defun ,name ,args ,@body)))
	     (t `((defun ,name ,args ,@body))))))

#-lambda
(defmacro microdefun (name args &body body) `(defun ,name ,args ,@body))
#-lambda
(defmacro miscdefun (name args &body body) `(defun ,name ,args ,@body))

;for now
(defmacro microfuncall (microfcn &rest args)
  `(funcall ,microfcn ,@args))

(defmacro defmicroparameter (&rest stuff)
  `(defparameter ,@stuff))

(defmacro microfunction (fcn)
  `(function ,fcn))
