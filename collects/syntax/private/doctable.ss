
(module doctable mzscheme
  (require (lib "moddep.ss" "syntax"))

  (define ht (make-hash-table 'equal))

  (define (register-documentation src-stx label v)
    (let ([mod (let ([s (syntax-source-module src-stx)])
		 (if (module-path-index? s)
		      ((current-module-name-resolver)
		       (collapse-module-path-index s `(lib "docprovide.ss" "syntax"))
		       #f #f)
		     s))])
      (let ([mht (hash-table-get ht mod
				 (lambda ()
				   (let ([mht (make-hash-table)])
				     (hash-table-put! ht mod mht)
				     mht)))])
	(hash-table-put! mht label v))))

  (define (lookup-documentation mod label)
    (let ([mht (hash-table-get ht mod (lambda () #f))])
      (and mht
	   (hash-table-get mht label (lambda () #f)))))

  (provide register-documentation
	   lookup-documentation))
