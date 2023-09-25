;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-

(in-package :cl-user)

(asdf:defsystem objectlisp
  :name "Object Lisp"
  :description "Object Lisp"
  :COMPONENTS ((:FILE "preface")
               (:FILE "nloop"  :DEPENDS-ON ("preface"))
               (:FILE "table"  :DEPENDS-ON ("nloop" "preface"))
               (:FILE "obwalk" :DEPENDS-ON ("nloop" "preface" "table"))
               (:FILE "walk"   :DEPENDS-ON ("nloop" "preface" "table"))
               (:FILE "obj"    :DEPENDS-ON ("nloop" "preface" "table" "walk" "obwalk"))
               (:FILE "prims"  :DEPENDS-ON ("nloop" "preface" "table" "walk" "obwalk"))
               (:FILE "global" :DEPENDS-ON ("nloop" "preface" "table" "walk" "obwalk" "obj"))))
  

#||
  (:compile-load preface)
  (:compile-load nloop (:fasload preface) (:fasload preface))
  (:compile-load table (:fasload nloop preface) (:fasload nloop preface))
  (:compile-load obwalk (:fasload nloop preface table) (:fasload nloop preface table))
  (:compile-load walk (:fasload nloop preface table) (:fasload nloop preface table))
  (:compile-load main (:fasload nloop preface table  obwalk walk)
		      (:fasload nloop preface table  obwalk walk))
  (:compile-load prims (:fasload nloop preface table  obwalk walk main)
		       (:fasload nloop preface table  obwalk walk main))
  (:compile-load global (:fasload nloop preface table  obwalk walk main)
		        (:fasload nloop preface table  obwalk walk main)))

||#
