
(module doctable mzscheme
  (define ht (make-hash-table))

  (define (register-documentation src-stx label v)
    (let ([mod (let ([s (syntax-source-module src-stx)])
                 (resolved-module-path-name
                  (if (module-path-index? s)
                      (module-path-index-resolve s)
                      s)))])
      (let ([mht (hash-table-get ht mod
				 (lambda ()
				   (let ([mht (make-hash-table)])
				     (hash-table-put! ht mod mht)
				     mht)))])
	(hash-table-put! mht label v))))

  (define (lookup-documentation mod label)
    (let ([mod (resolved-module-path-name mod)])
      (let ([mht (hash-table-get ht mod (lambda () #f))])
        (and mht
             (hash-table-get mht label (lambda () #f))))))

  (provide register-documentation
	   lookup-documentation))
