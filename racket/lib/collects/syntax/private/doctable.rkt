(module doctable racket/base
  (define ht (make-hasheq))

  (define (register-documentation src-stx label v)
    (let ([mod (let ([s (syntax-source-module src-stx)])
                 (resolved-module-path-name
                  (if (module-path-index? s)
                      (module-path-index-resolve s)
                      s)))])
      (let ([mht (hash-ref ht mod
				 (lambda ()
				   (let ([mht (make-hasheq)])
				     (hash-set! ht mod mht)
				     mht)))])
	(hash-set! mht label v))))

  (define (lookup-documentation mod label)
    (let ([mod (resolved-module-path-name mod)])
      (let ([mht (hash-ref ht mod (lambda () #f))])
        (and mht
             (hash-ref mht label (lambda () #f))))))

  (provide register-documentation
	   lookup-documentation))
