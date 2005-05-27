(module tex2page mzscheme
  (require (lib "etc.ss"))
  (provide tex2page)
  (define
   tex2page
   (lambda (f)
     (parameterize
       ((current-namespace (make-namespace)))
       (namespace-require
	(build-path (this-expression-source-directory) "tex2page-aux.ss"))
       ((namespace-variable-value 'tex2page) f)))))
