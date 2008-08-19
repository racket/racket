#lang scheme

(require (for-syntax scheme)
         web-server/template/lib)

(provide (except-out (all-from-out scheme) #%module-begin)
         (rename-out [*module-begin #%module-begin])
         (all-from-out web-server/template/lib))

(define-syntax (*module-begin stx)
  (syntax-case stx (require)
    [(_ id (require r ...) nl (fv ...) body ...)
     #'(#%module-begin
        (require r ...)
        (define-template id (fv ...) (#%string-append body ...))
        (provide id))]
    [(_ id (fv ...) body ...)
     #'(#%module-begin
        (define-template id (fv ...) (#%string-append body ...))
        (provide id))]))

#;(define-syntax (define-template stx)
  (syntax-case stx ()
    [(_ id body)
     (with-syntax ([(pmb body)
                    (local-expand 
                     (quasisyntax/loc stx body)
                     'expression 
                     empty)])
       (let ([fvars (free-vars #'body)])
         (quasisyntax/loc stx
           (define (id #,@fvars)
             body))))]))

(define-syntax (define-template stx)
  (syntax-case stx ()
    [(_ id (fv ...) body)
     (quasisyntax/loc stx
       (define (id fv ...)
         body))]))
