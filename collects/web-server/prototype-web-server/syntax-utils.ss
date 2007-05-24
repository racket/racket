(module syntax-utils mzscheme
  (require-for-template mzscheme)
  (provide recertify
           recertify*
           generate-formal)
  
  ;; syntax syntax -> syntax
  (define (recertify expr old-expr)
    (syntax-recertify expr old-expr (current-code-inspector) #f))
  
  ;; (listof syntax) syntax -> syntax
  ;; recertify a list of syntax parts given the whole
  (define (recertify* exprs old-expr)
    (map
     (lambda (expr)
       (syntax-recertify expr old-expr (current-code-inspector) #f))
     exprs))
  
  ;; generate-formal: -> identifier
  (define (generate-formal sym-name)
    (let ([name (datum->syntax-object #f (gensym sym-name))])
      (with-syntax ([(lambda (formal) ref-to-formal)
                     (if (syntax-transforming?)
                         (local-expand #`(lambda (#,name) #,name) 'expression '())
                         #`(lambda (#,name) #,name))])
        (values #'formal #'ref-to-formal))))
    )
  
  
  