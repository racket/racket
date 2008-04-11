
(module reader scheme/base
  (require (only-in syntax/module-reader wrap-read-all))
  (provide (rename-out [*read read]
                       [*read-syntax read-syntax]))
  
  (define (*read in modpath line col pos)
    (wrap in read modpath #f line col pos))
  
  (define (*read-syntax src in modpath line col pos)
    (wrap in (lambda (in)
               (read-syntax src in))
          modpath src line col pos))

  (define (wrap port read modpath src line col pos)
    (parameterize ([read-case-sensitive #f]
                   [read-accept-infix-dot #f]
                   [read-curly-brace-as-paren #f]
                   [read-square-bracket-as-paren #f])
      (wrap-read-all 'r5rs port read modpath src line col pos))))
