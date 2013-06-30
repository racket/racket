
(module old-ds '#%kernel
  (#%require racket/private/define-struct
             (for-syntax '#%kernel
                         racket/private/stxcase-scheme))

  (#%provide define-struct let-struct old-datum)
  
  (define-syntaxes (define-struct)
    (lambda (stx)
      (with-syntax ([orig stx])
        (syntax-case stx ()
          [(_ base (field ...))
           #'(define-struct/derived orig base (field ...) #:mutable)]
          [(_ base (field ...) insp)
           (with-syntax ([insp
                          (if (keyword? (syntax-e #'insp))
                              (datum->syntax #'insp
                                             (cons '#%datum #'insp)
                                             #'insp)
                              #'insp)])
             #'(define-struct/derived orig base (field ...) #:mutable #:inspector insp))]))))

  (define-syntaxes (let-struct)
    (syntax-rules ()
      [(_ base (field ...) body1 body ...)
       (let-values ()
         (define-struct base (field ...))
         body1 body ...)]))

  (define-syntaxes (old-datum)
    (syntax-rules ()
      [(_ . any) (quote any)])))
