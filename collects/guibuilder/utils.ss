
(module utils mzscheme
  (require (prefix mred: (lib "mred.ss" "mred"))
	   (lib "class.ss")
	   (lib "etc.ss")
	   (lib "list.ss"))

  (define-syntax (private-field stx)
    (syntax-case stx ()
      [(_ (id val) ...)
       (syntax/loc stx (begin (define id val) ...))]))

  (define make-one-line/callback-edit
    (opt-lambda (parent label cb [v ""])
      (make-object mred:text-field% label parent 
		   (lambda (t e) (cb (send t get-value))) v)))

  (define make-number-control
    (lambda (parent label value get-min get-max set-v)
      (let* ([p (make-object mred:horizontal-panel% parent)]
	     [l (make-object mred:message% label p)]
	     [vl (make-object mred:message% "999999" p)]
	     [set-value
	      (lambda (n)
		(set! value n)
		(send vl set-label (number->string n))
		(set-v n))]
	     [b (make-object mred:button%
			     "Set..."
			     p
			     (lambda (b e)
			       (let ([v (mred:get-text-from-user 
					 (format "~a, in [~a, ~a]:" label (get-min) (get-max))
					 label
					 #f
					 (number->string value))])
				 (when v
				   (let ([n (string->number v)])
				     (if (and (integer? n) (exact? n) 
					      (>= n (get-min)) (<= n (get-max)))
					 (set-value n)
					 (mred:message-box "Error" "Bad value")))))))])
	(send vl set-label (number->string value))
	(make-object (class object% ()
		       (public*
			 [get-val (lambda () value)]
			 [check (lambda () 
				  (when (< value (get-min))
				    (set-value (get-min)))
				  (when (> value (get-max))
				    (set-value (get-max))))])
		       (super-new))))))
  
  (define new-name (lambda (base) (symbol->string (gensym base))))
  
  (define (stream-write-list stream l)
    (send stream put (length l))
    (for-each
     (lambda (i)
       (send stream put (string->bytes/utf-8 i)))
     l))
  
  (define (get-bytes->string version)
    (if (version . >= . 5)
	bytes->string/utf-8
	bytes->string/latin-1))

  (define (stream-read-list stream version)
    (let ([n (send stream get-exact)]
	  [b->s (get-bytes->string version)])
      (let loop ([n n])
	(if (zero? n)
	    null
	    (cons (b->s (send stream get-bytes)) (loop (sub1 n)))))))

  (define cached-region #f)
  (define cached-region-dc #f)

  (define (with-clipping-region dc x y w h thunk)
    (let ([r (send dc get-clipping-region)]
	  [r2 (if (eq? dc cached-region-dc)
		  cached-region
		  (make-object mred:region% dc))])
      (set! cached-region-dc #f)
      (send r2 set-rectangle x y w h)
      (send r2 intersect r)
      (send dc set-clipping-region r2)
      (thunk)
      (send dc set-clipping-region r)
      (set! cached-region r2)
      (set! cached-region-dc dc)))
  
  (provide private-field
	   make-one-line/callback-edit
	   make-number-control
	   new-name
	   get-bytes->string	   
	   stream-write-list
	   stream-read-list
	   with-clipping-region))

