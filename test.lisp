

(IN-PACKAGE :OBJ)

(setq animal (kindof nil))


(COMPILE-FILE "preface")
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

(NLOOP )




(NLOOP (INC-TIL X 0 10)
       (COLLECT (PRINT X)))

(defun foo (list)
  (nloop (for-on l1 list) (for x (car l1)) (print x) (collect (1+ x))))

(FOO '(1 2 3 4))