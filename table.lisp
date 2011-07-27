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

;; One of max-load, inv-load should be 1, the other an integer.
;; The real max load factor is max-load / inv-load.
(defparameter *maximum-load-factor 2)
(defparameter *inverse-load-factor 1)
(defparameter *minimum-hash-size 32.)
(defparameter *alist-to-hash-threshold 32.)
(defparameter *default-alist-to-hash-threshold 32.)
(defparameter *hash-to-alist-threshold -1)

; This file supports special-purpose tables with <link>/<value> entries,
; where a <link> contains an entry's key & associated hashcode.

(defstruct (basic-link (:conc-name nil) #+lambda (:callable-constructors nil)
		       (:constructor make-basic-link (link-key link-index)))
  link-index  link-key )

;Table format: (COUNT . ALIST) or (COUNT MASK . HASH-TABLE)

;ALIST ROUTINES

;Alist entry: (VALUE . LINK)

; Inputs must not repeat a given link more than once.
(defun alist-make-table (link-val-pairs &aux table)
  (nloop (stop-if (null link-val-pairs))
	 (for link  (pop link-val-pairs))
	 (for val  (pop link-val-pairs))
	 (push (cons val link) table))
  table)

(defsubst alist-lookup (link table)
  (rassq link (cdr table)))

(defsubst alist-insert-new-ok-size (link val table)
  (push (cons val link) (cdr table)))

(defsubst alist-remove (link table)
  (setf (cdr table) (remq (rassq link (cdr table)) (cdr table))))


;HASH ROUTINES

;Hash table entry: (VALUE . LINK)

(defmacro hash-slot-contents (array mask ix)
  `(fast-aref ,array (logand ,mask ,ix)))

(microdefun make-hash-cons (number-of-entries)
  (let ((size (max *minimum-hash-size
		   (nloop (with base (* 2 number-of-entries))
			  (for i 1 (* 2 i))
			  (stop-unless (< i base))
			  (finally i)))))
    (cons (1- size) (make-array size :adjustable nil))))

(defun hash-make-table (link-val-pairs &aux hashcons mask array)
  (setq hashcons (make-hash-cons (floor (length link-val-pairs) 2))
	mask (car hashcons)
	array (cdr hashcons))
  (nloop (stop-if (null link-val-pairs))
	 (for link (pop link-val-pairs))
	 (for val (pop link-val-pairs))
	 (push (cons val link) (hash-slot-contents array mask (link-index link))))
  (cons mask array))

(defsubst hash-lookup (link table)
  (rassq link (hash-slot-contents (cddr table) (cadr table) (link-index link))))

(microdefun double-table-size (table)
  (let* ((old-mask (cadr table))
	 (new-mask (1- (* 2 (1+ old-mask))))
	 (old-array (cddr table))
	 (NEW-ARRAY (MAKE-ARRAY (1+ NEW-MASK) :ADJUSTABLE NIL)))
    (RPLACD (CDR TABLE) NEW-ARRAY)
;   (adjust-array array #+lambda (ncons (1+ new-mask)) #-lambda (1+ new-mask))
    (rplaca (cdr table) new-mask)
    ;; Now xfer old array contents to new.
    (nloop (inc-til i 0 old-mask)
	   (for contents (fast-aref OLD-array i))
	   (nloop (for-in entry contents)
		  (for ix (link-index (cdr entry)))
;		  (unless (= (logand ix old-mask) (logand ix new-mask))
;		    (setq contents (remq entry contents))
		    (push entry (fast-aref NEW-array (logand ix new-mask))) ))))
;	   (setf (fast-aref array i) contents))))

(microdefun hash-insert-new (link val table)
  (incf (car table))
  (if (> (* *inverse-load-factor (car table))
	 (* *maximum-load-factor (1+ (cadr table))))
      (double-table-size table))
  (push (cons val link) (hash-slot-contents (cddr table) (cadr table)
					    (link-index link))))

(defsubst hash-insert (link val table)
  (let ((entry (rassq link (hash-slot-contents (cddr table) (cadr table)
					       (link-index link)))))
    (if entry
	(rplaca entry val)
	(hash-insert-new link val table))))

(microdefun hash-remove (link table)
  (let ((mask (cadr table))
	(array (cddr table))
	(index (link-index link)))
    (setf (hash-slot-contents array mask INDEX)
	  (remq (rassq link (hash-slot-contents array mask INDEX))
		(hash-slot-contents array mask INDEX)))))

(defun hash-table-map (fcn array)
  (nloop (inc-nottil i 0 (array-total-size array))
	 (mapc fcn (fast-aref array i))))

(defun hash-table-map-return (fcn array)
  (nloop (inc-nottil i 0 (array-total-size array))
	 (collect* (mapcar fcn (fast-aref array i)))))

;CONVERSION ROUTINES

(microdefun alist-to-hash (table)
  (let* ((alist (cdr table))
	 (hc (make-hash-cons (length alist)))
	 (array (cdr hc)))
    (nloop (for-in entry (cdr table))
	   (push entry (hash-slot-contents array (car hc)
					   (link-index (cdr entry)))))
    (rplacd table hc)))

(microdefun alist-insert-new (link val table)
  (cond ((>= (car table) *alist-to-hash-threshold)
	 (alist-to-hash table)
	 (hash-insert-new link val table))
	(t
	 (incf (car table))
	 (alist-insert-new-ok-size link val table))))

(microdefun alist-insert (link val table)
  (let ((cons (rassq link (cdr table))))
    (if cons
	(rplaca cons val)
	(alist-insert-new link val table))))

;GENERIC ROUTINES

(defun make-table (&rest link-val-pairs &aux (count (floor (length link-val-pairs) 2)))
  (if (> count *alist-to-hash-threshold)
      (cons count (hash-make-table link-val-pairs))
      (cons count (alist-make-table link-val-pairs))))

(defsubst table-count (table)
  (car table))

(defsubst alist? (table)
  (not (numberp (cadr table))))

;Returns an assq-cons.
(microdefun lookup (link table)
  (if (alist? table)
      (alist-lookup link table)
      (hash-lookup link table)))

(microdefun insert-new (link val table)
  (if (alist? table)
      (alist-insert-new link val table)
      (hash-insert-new link val table)))

(microdefun insert (link val table)
  (if (alist? table)
      (alist-insert link val table)
      (hash-insert link val table)))

(microdefun table-remove (link table)
  (if (alist? table)
      (alist-remove link table)
      (hash-remove link table)))

;FCN should take one input, a table entry, (VALUE . LINK).
(defun table-map (fcn table)
  (if (alist? table)
      (mapc fcn (cdr table))
      (hash-table-map fcn (cddr table))))

(defun table-map-return (fcn table)
  (if (alist? table)
      (mapcar fcn (cdr table))
      (hash-table-map-return fcn (cddr table))))
