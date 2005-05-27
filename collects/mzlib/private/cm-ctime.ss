(module cm-ctime mzscheme
  (provide current-external-file-registrar)
  
  (define current-external-file-registrar
    (make-parameter
     void
     (lambda (p)
       (unless (and (procedure? p)
		    (procedure-arity-includes? p 1))
	 (raise-type-error 'current-external-file-registrar "procedure (arity 2)" p))
       p))))
