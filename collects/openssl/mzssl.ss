(module mzssl mzscheme
  (define ssl-available? #f)
  (provide ssl-available?)

  (define-syntax provide-wrappers
    (syntax-rules ()
      [(_) (begin)]
      [(_ id1 id ...)
       (begin
	 (define (id1 . args)
	   (error 'id1 "extension not compiled"))
	 (provide id1)
	 (provide-wrappers id ...))]))

  (provide-wrappers
   ssl-connect ssl-connect/enable-break
   ssl-listen ssl-listener? ssl-close
   ssl-accept ssl-accept/enable-break
   ssl-addresses
   ssl-make-client-context ssl-client-context?
   ssl-load-certificate-chain!
   ssl-set-verify!
   ssl-load-verify-root-certificates!
   ssl-load-private-key!
   ssl-load-suggested-certificate-authorities!))

