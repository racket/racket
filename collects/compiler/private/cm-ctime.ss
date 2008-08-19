(module cm-ctime '#%kernel
  (#%provide current-external-file-registrar)
  
  (define-values (current-external-file-registrar)
    (make-parameter
     void
     (lambda (p)
       (if (if (procedure? p)
               (procedure-arity-includes? p 1)
               #f)
           'ok
           (raise-type-error 'current-external-file-registrar "procedure (arity 2)" p))
       p))))
