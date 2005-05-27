
(module region mzscheme
  (provide region
	   struct:region make-region
	   region? region-x region-y region-w region-h 
	   region-label region-callback 
	   
	   make-button-region make-background-region

	   region-interactive-callback
	   region-paint-callback
	   region-button? region-hilite? region-decided-start? region-can-select?
	   set-region-hilite?! set-region-decided-start?! set-region-can-select?!
	   set-region-callback!
	   set-region-interactive-callback!)

  (define-syntax region
    (list-immutable #'struct:region
		    #'make-region
		    #'region?
		    (list-immutable
		     #'region-callback #'region-label 
		     #'region-h #'region-w 
		     #'region-y #'region-x 
		     #f)
		    (list-immutable #f)
		    #t))

  (define-values (struct:region make-region region? region-get region-set!)
    (make-struct-type 'region #f 6 6 #f))

  (define region-x (make-struct-field-accessor region-get 0))
  (define region-y (make-struct-field-accessor region-get 1))
  (define region-w (make-struct-field-accessor region-get 2))
  (define region-h (make-struct-field-accessor region-get 3))
  (define region-label (make-struct-field-accessor region-get 4))
  (define region-callback (make-struct-field-accessor region-get 5))

  (define set-region-callback! (make-struct-field-mutator region-set! 5))

  (define region-interactive-callback (make-struct-field-accessor region-get 6))
  (define region-paint-callback (make-struct-field-accessor region-get 7))
  (define region-button? (make-struct-field-accessor region-get 8))
  (define region-hilite? (make-struct-field-accessor region-get 9))
  (define region-decided-start? (make-struct-field-accessor region-get 10))
  (define region-can-select? (make-struct-field-accessor region-get 11))

  (define set-region-interactive-callback! (make-struct-field-mutator region-set! 6))
  (define set-region-paint-callback! (make-struct-field-mutator region-set! 7))
  (define set-region-button?! (make-struct-field-mutator region-set! 8))
  (define set-region-hilite?! (make-struct-field-mutator region-set! 9))
  (define set-region-decided-start?! (make-struct-field-mutator region-set! 10))
  (define set-region-can-select?! (make-struct-field-mutator region-set! 11))

  (define create-region
    (lambda (x y w h label callback)
      (unless (not (or (negative? w) (negative? h)))
	(error 'make-region "bad region size: ~a x ~a" w h))
      (make-region x y w h label callback #f #f #f #f #f #f)))

  (define make-background-region
    (lambda (x y w h paint-callback)
      (unless (not (or (negative? w) (negative? h)))
	(error 'make-background-region "bad region size: ~a x ~a" w h))
      (let ([r (make-region x y w h #f #f)])
	(set-region-paint-callback! r paint-callback)
	r)))

  (define make-button-region
    (lambda (x y w h label callback)
      (unless (not (or (negative? w) (negative? h)))
	(error 'make-button-region "bad region size: ~a x ~a" w h))
      (let ([r (make-region x y w h label callback)])
	(set-region-button?! r #t)
	r))))
