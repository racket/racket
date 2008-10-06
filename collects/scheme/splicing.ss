#lang scheme/base
(require (for-syntax scheme/base))

(provide splicing-let-syntax
         splicing-let-syntaxes
         splicing-letrec-syntax
         splicing-letrec-syntaxes)

(define-for-syntax (do-let-syntax stx rec? multi?)
  (syntax-case stx ()
    [(_ ([ids expr] ...) body ...)
     (let ([all-ids (map (lambda (ids-stx)
                           (let ([ids (if multi?
                                          (syntax->list ids-stx)
                                          (list ids-stx))])
                             (unless ids
                               (raise-syntax-error 
                                #f
                                "expected a parenthesized sequence of identifiers"
                                stx
                                ids-stx))
                             (for-each (lambda (id)
                                         (unless (identifier? id)
                                           (raise-syntax-error
                                            #f
                                            "expected an identifier"
                                            stx
                                            id)))
                                       ids)
                             ids))
                         (syntax->list #'(ids ...)))])
       (let ([dup-id (check-duplicate-identifier
                      (apply append all-ids))])
         (when dup-id
           (raise-syntax-error
            #f
            "duplicate binding"
            stx
            dup-id)))
       (if (eq? 'expression (syntax-local-context))
           (with-syntax ([let-stx (if rec?
                                      (if multi?
                                          #'letrec-syntaxes
                                          #'letrec-syntax)
                                      (if multi?
                                          #'let-syntaxes
                                          #'let-syntax))])
             (syntax/loc stx
               (let-stx ([ids expr] ...)
                        (#%expression body)
                        ...)))
           (let ([def-ctx (syntax-local-make-definition-context)]
                 [ctx (list (gensym 'intdef))])
             (syntax-local-bind-syntaxes (apply append all-ids) #f def-ctx)
             (let* ([add-context
                     (lambda (expr)
                       (let ([q (local-expand #`(quote #,expr)
                                              ctx
                                              (list #'quote)
                                              def-ctx)])
                         (syntax-case q ()
                           [(_ expr) #'expr])))])
               (with-syntax ([((id ...) ...)
                              (map (lambda (ids)
                                     (map add-context ids))
                                   all-ids)]
                             [(expr ...)
                              (let ([exprs (syntax->list #'(expr ...))])
                                (if rec?
                                    (map add-context exprs)
                                    exprs))]
                             [(body ...)
                              (map add-context (syntax->list #'(body ...)))])
                 #'(begin
                     (define-syntaxes (id ...) expr)
                     ...
                     body ...))))))]))

(define-syntax (splicing-let-syntax stx)
  (do-let-syntax stx #f #f))

(define-syntax (splicing-let-syntaxes stx)
  (do-let-syntax stx #f #t))

(define-syntax (splicing-letrec-syntax stx)
  (do-let-syntax stx #t #f))

(define-syntax (splicing-letrec-syntaxes stx)
  (do-let-syntax stx #t #t))