(module tex2page mzscheme
  (require mzlib/etc)
  (provide tex2page)
  (define
   tex2page
   (lambda (f)
     (parameterize
       ((current-namespace (make-namespace)))
       (namespace-require
        `(file ,(path->string (build-path (this-expression-source-directory)
                                          "tex2page-aux.rkt"))))
       ((namespace-variable-value 'tex2page) f)))))
