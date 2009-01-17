#lang scheme/base
(require (for-syntax scheme/base
                     syntax/kerncase)
         "stxparam.ss"
         "private/stxparam.ss")

(provide splicing-let-syntax
         splicing-let-syntaxes
         splicing-letrec-syntax
         splicing-letrec-syntaxes
         splicing-syntax-parameterize)

(define-for-syntax (do-let-syntax stx rec? multi? let-stx-id)
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
           (with-syntax ([let-stx let-stx-id])
             (syntax/loc stx
               (let-stx ([ids expr] ...)
                        (#%expression body)
                        ...)))
           (let ([def-ctx (syntax-local-make-definition-context)]
                 [ctx (list (gensym 'intdef))])
             (syntax-local-bind-syntaxes (apply append all-ids) #f def-ctx)
             (internal-definition-context-seal def-ctx)
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
  (do-let-syntax stx #f #f #'let-syntax))

(define-syntax (splicing-let-syntaxes stx)
  (do-let-syntax stx #f #t #'let-syntaxes))

(define-syntax (splicing-letrec-syntax stx)
  (do-let-syntax stx #t #f #'letrec-syntax))

(define-syntax (splicing-letrec-syntaxes stx)
  (do-let-syntax stx #t #t #'letrec-syntaxes))

;; ----------------------------------------

(define-syntax (splicing-syntax-parameterize stx)
  (if (eq? 'expression (syntax-local-context))
      ;; Splicing is no help in an expression context:
      (do-syntax-parameterize stx #'let-syntaxes)
      ;; Let `syntax-parameterize' check syntax, then continue
      (do-syntax-parameterize stx #'ssp-let-syntaxes)))

(define-syntax (ssp-let-syntaxes stx)
  (syntax-case stx ()
    [(_ ([(id) rhs] ...) body ...)
     (with-syntax ([(splicing-temp ...) (generate-temporaries #'(id ...))])
       #'(begin
           ;; Evaluate each RHS only once:
           (define-syntax splicing-temp rhs) ...
           ;; Partially expand `body' to push down `let-syntax':
           (expand-ssp-body (id ...) (splicing-temp ...) body)
           ...))]))

(define-syntax (expand-ssp-body stx)
  (syntax-case stx ()
    [(_ (sp-id ...) (temp-id ...) body)
     (let ([body (local-expand #'(letrec-syntaxes ([(sp-id) (syntax-local-value (quote-syntax temp-id))]
                                                   ...)
                                   (force-expand body))
                               (syntax-local-context)
                               null ;; `force-expand' actually determines stopping places
                               #f)])
       ;; Extract expanded body out of `body':
       (syntax-case body (quote)
         [(ls _ _ (quote body))
          (let ([body #'body])
            (syntax-case body (begin define-values define-syntaxes define-for-syntaxes)
              [(begin expr ...)
               (syntax/loc body
                 (begin (expand-ssp-body (sp-id ...) (temp-id ...) expr) ...))]
              [(define-values (id ...) rhs)
               (syntax/loc body
                 (define-values (id ...)
                   (letrec-syntaxes ([(sp-id) (syntax-local-value (quote-syntax temp-id))] ...)
                     rhs)))]
              [(define-syntaxes . _) body]
              [(define-for-syntaxes . _) body]
              [expr (syntax/loc body
                      (letrec-syntaxes ([(sp-id) (syntax-local-value (quote-syntax temp-id))] ...)
                        expr))]))]))]))

(define-syntax (force-expand stx)
  (syntax-case stx ()
    [(_ stx)
     ;; Expand `stx' to reveal type of form, and then preserve it via
     ;; `quote':
     #`(quote #,(local-expand #'stx
                              'module
                              (kernel-form-identifier-list)
                              #f))]))
