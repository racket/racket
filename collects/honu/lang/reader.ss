
(module reader scheme/base
  (require (only-in syntax/module-reader wrap-read-all))
  (provide (rename-out [*read read]
                       [*read-syntax read-syntax]))
  
  (define (*read in modpath line col pos)
    (wrap in read-honu modpath #f line col pos))
  
  (define (*read-syntax src in modpath line col pos)
    (wrap in (lambda (in)
               (read-honu-syntax src in))
          modpath src line col pos))

  (define (wrap port read modpath src line col pos)
    (wrap-read-all 'honu port read modpath src line col pos)))
