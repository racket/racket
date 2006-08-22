
(module wrap mzscheme
  (provide (rename module-begin #%module-begin))
  (require (lib "include.ss"))
  (define-syntax (module-begin stx)
    (let ([name (syntax-property stx 'enclosing-module-name)])
      #`(#%plain-module-begin 
         (require "map.ss")
         (include #,(format "~a.sch" name))))))
