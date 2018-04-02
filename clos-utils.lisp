
;  Common Lisp CLOS utilities
;
;  Written by:
;        Blake McBride
;        blake@mcbride.name
;        http://blake.mcbride.name
;
;  Version / Release 4/1/2018
;
;  Donated to the public domain.


(defmacro set-slot (i s v)
  `(setf (slot-value ,i ',s) ,v))

(defmacro get-slot (i s)
  `(slot-value ,i ',s))

(defclass metaclass (standard-class) ())

#+sbcl           (defmethod sb-mop:validate-superclass ((class metaclass) (superclass standard-class)) t)
#+(or cmu gcl)   (defmethod pcl:validate-superclass ((class metaclass) (superclass standard-class)) t)
#+abcl           (defmethod mop:validate-superclass ((class metaclass) (superclass standard-class)) t)
#+ccl            (defmethod ccl:validate-superclass ((class metaclass) (superclass standard-class)) t)

(defmacro defclass2 (class-name super-class-list class-variables instance-variables)
  (flet ((add-meta (sym)
		   (intern (concatenate 'string "META-" (symbol-name sym))))
	 (make-global (x)
		      (if (consp x)
			  (append x '(:allocation :class))
			(cons x '(:allocation :class)))))
	`(progn
	   (defclass ,(add-meta class-name)
	     ,(if (null super-class-list)
		  '(metaclass)
		(mapcar #'add-meta super-class-list))
	     ,(mapcar #'make-global class-variables)
	     (:metaclass metaclass))
	   (defclass ,class-name
	     ,(if (null super-class-list)
		  '(standard-object)
		super-class-list)
	     ,instance-variables
	     (:metaclass ,(add-meta class-name)))
	   (defvar ,class-name (find-class ',class-name))
;          (defvar ,(add-meta class-name) (find-class ',(add-meta class-name)))
	   )))

