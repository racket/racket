
(module pconvert-prop mzscheme

  (provide prop:print-convert-constructor-name
	   print-convert-named-constructor?
	   print-convert-constructor-name)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; property recognized by print convert to set a value's constructor name:
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-values (prop:print-convert-constructor-name
		  print-convert-named-constructor?
		  print-convert-constructor-name)
    (make-struct-type-property 'print-convert-constructor-name
			       (lambda (s info)
				 (unless (symbol? s)
				   (raise-type-error '|prop:print-convert-constructor-name guard|
						     "symbol"
						     s))
				 s))))
