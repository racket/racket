
(module wrap racket
  (provide (rename-out (module-begin #%module-begin)))
  (require (lib "include.ss"))
  (define-syntax (module-begin stx)
    (let ([name (syntax-property stx 'enclosing-module-name)])
      #`(#%module-begin 
         (include #,(format "~a.sch" name))))))
