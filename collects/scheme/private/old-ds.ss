
(module old-ds '#%kernel
  (#%require "define-struct.ss"
             (for-syntax "stxcase-scheme.ss"))

  (#%provide define-struct let-struct datum)
  
  (define-syntaxes (define-struct)
    (syntax-rules ()
      [(_ base (field ...))
       (define-struct* base (field ...) #:mutable)]
      [(_ base (field ...) insp)
       (define-struct* base (field ...) #:mutable #:inspector insp)]))

  (define-syntaxes (let-struct)
    (syntax-rules ()
      [(_ base (field ...) body1 body ...)
       (let-values ()
         (define-struct base (field ...))
         body1 body ...)]))

  (define-syntaxes (datum)
    (syntax-rules ()
      [(_ . any) (quote any)])))
