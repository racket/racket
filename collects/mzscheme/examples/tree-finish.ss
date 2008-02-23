
;; Assumes that tree.so has been loaded

(require mzlib/class)

(define tree%
  (let ([method-names '(get-leaves 
			get-left get-right
			grow
			graft)])
    (make-primitive-class
     (lambda (class prop:object preparer dispatcher)
       (primitive-class-prepare-struct-type! 
	tree-primitive-class
	prop:object
	class
	preparer
	dispatcher))
     initialize-primitive-object
     'tree%
     object%
     '(leaves)
     null
     method-names
     null
     (map
      (lambda (name)
	(primitive-class-find-method tree-primitive-class name))
      method-names))))

