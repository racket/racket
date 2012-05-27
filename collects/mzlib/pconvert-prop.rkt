
(module pconvert-prop '#%kernel

  (#%provide prop:print-convert-constructor-name
             print-convert-named-constructor?
             print-convert-constructor-name
             prop:print-converter
             print-converter?
             print-converter-proc)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; property recognized by print convert to set a value's constructor name:
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-values (prop:print-convert-constructor-name
		  print-convert-named-constructor?
		  print-convert-constructor-name)
    (make-struct-type-property 'print-convert-constructor-name
			       (lambda (s info)
				 (if (symbol? s)
                                     (void)
                                     (raise-argument-error '|prop:print-convert-constructor-name guard|
                                                           "symbol?"
                                                           s))
				 s)))

  (define-values (prop:print-converter
		  print-converter?
		  print-converter-proc)
    (make-struct-type-property 'print-converter
			       (lambda (p info)
                                 (if (if (procedure? p)
                                         (procedure-arity-includes? p 2)
                                         #f)
                                     (void)
                                     (raise-argument-error '|prop:print-converter|
                                                           "(procedure-arity-includes/c 2)"
                                                           p))
				 p))))
