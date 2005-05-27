(module firstorder mzscheme

  (provide make-first-order
	   first-order->higher-order)

  (define-values (struct:fo make-first-order fo? fo-get fo-set!)
    (make-struct-type 'procedure #f 2 0 #f null (current-inspector) 0))  

  (define fo-proc-id (make-struct-field-accessor fo-get 1))

  (define (first-order->higher-order id)
    (let ([v (syntax-local-value id (lambda () #f))])
      (if (and (set!-transformer? v)
	       (fo? (set!-transformer-procedure v)))
	  (syntax-local-introduce 
	   (fo-proc-id (set!-transformer-procedure v)))
	  id))))


