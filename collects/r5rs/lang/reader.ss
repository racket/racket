
(module reader scheme/base
  (require (only-in syntax/module-reader wrap-read-all))
  (provide (rename-out [*read read]
                       [*read-syntax read-syntax]))
  
  (define (*read in)
    (wrap in read))
  
  (define (*read-syntax src in)
    (wrap in (lambda (in)
               (read-syntax src in))))

  (define (wrap port read)
    (parameterize ([read-case-sensitive #f]
                   [read-accept-infix-dot #f]
                   [read-curly-brace-as-paren #f]
                   [read-square-bracket-as-paren #f])
      (wrap-read-all 'r5rs port read))))
