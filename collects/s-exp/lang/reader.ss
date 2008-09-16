(module reader syntax/module-reader
  -ignored-
  #:wrapper2
  (lambda (in rd stx?)
    (let* ([mod* (rd in)]
           [mod  (if stx? (syntax->list mod*) mod*)]
           [mod  `(,(car mod) ,(cadr mod) ,@(cdddr mod))])
      (if stx? (datum->syntax mod* mod mod*) mod))))
