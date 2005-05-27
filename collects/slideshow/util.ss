
(module util mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (provide make-bitmap
	   define-accessor
	   define/provide-struct)

  ;; If bitmap creation fails, try an explicit GC.
  ;; (This loop should be built into MrEd, but it wasn't
  ;; at the time this code was written.)
  (define (make-bitmap w h)
    (let loop ([n 0])
      (let ([bm (make-object bitmap% w h)])
	(if (or (= n 4)
		(send bm ok?))
	    bm
	    (begin
	      (collect-garbage)
	      (loop (add1 n)))))))

  (define-syntax define-accessor
    (syntax-rules ()
      [(_ margin get-margin)
       (define-syntax margin
	 (syntax-id-rules ()
	   [(margin arg) ((get-margin) arg...)]
	   [margin (get-margin)]))]))
  

  (define-syntax define/provide-struct
    (syntax-rules ()
      [(_ id flds)
       (begin
	 (define-struct id flds)
	 (provide (struct id flds)))])))



