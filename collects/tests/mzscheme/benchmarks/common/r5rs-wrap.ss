
(module r5rs-wrap r5rs
  (#%require scheme/include
             (only scheme/base error time bitwise-not bitwise-and)
             (only scheme/base provide rename-out)
             (for-syntax scheme/base))
  (provide (rename-out [module-begin #%module-begin]))
  (define-syntax (module-begin stx)
    (let ([name (syntax-property stx 'enclosing-module-name)])
      #`(#%module-begin 
         (include #,(format "~a.sch" name))))))
