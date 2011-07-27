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

(defobfun exist (&rest args &key* obj-name)
 (if obj-name (have 'obj-name obj-name))
 (bind-builtin-instance-vars (current-obj) args)
 nil)

(defvar obj-name)
(SHADOW 'CLASS-NAME :OBJ)
(defvar class-name)

(defvar *objs-print-self? t)

(defobfun print-self (&aux (ptr (if *%pointer?
				    (%pointer (current-obj))
				    (obj-index (current-obj)))))
  (if (AND *objs-print-self? (NOT (GLOBAL-OBJ? (CURRENT-OBJ))))
      (cond ;((null (current-obj)) (prin1 nil))
	    ((own? 'class-name)
	     (format t "#<The class ~a ~o>" class-name ptr))
	    ((own? 'obj-name)
	     (if (own? 'class-name)
		 (format t "#<~a, a ~a ~o>" obj-name class-name ptr)
		 (format t "#<~a ~o>" obj-name ptr)))
	    ((there? 'class-name)
	     (format t "#<An unnamed ~a ~o>" class-name ptr))
	    (t (format t "#<Object ~o>" ptr)))
      (format t "#<Object ~a>" ptr)))
