
(module wrap mzscheme
  (provide (rename module-begin #%module-begin))
  (require mzlib/include)
  (define-syntax (module-begin stx)
    (let ([name (syntax-property stx 'enclosing-module-name)])
      #`(#%plain-module-begin 
         (include #,(format "~a.sch" name))))))
