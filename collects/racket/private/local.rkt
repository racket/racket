#lang racket/base
(require (for-syntax racket/base)
         (for-syntax syntax/kerncase))
(provide (for-syntax do-local))

(define-for-syntax (do-local stx letrec-syntaxes+values-id)
  (syntax-case stx ()
    [(_ (defn ...) body1 body ...)
     (let* ([def-ctx (syntax-local-make-definition-context)]
            [defs (let ([expand-context (cons (gensym 'intdef)
                                              (let ([orig-ctx (syntax-local-context)])
                                                (if (pair? orig-ctx)
                                                    orig-ctx
                                                    null)))])
                    (let loop ([defns (syntax->list (syntax (defn ...)))])
                      (apply
                       append
                       (map
                        (lambda (defn)
                          (let ([d (local-expand
                                    defn
                                    expand-context
                                    (kernel-form-identifier-list)
                                    def-ctx)]
                                [check-ids (lambda (defn ids)
                                             (for-each
                                              (lambda (id)
                                                (unless (identifier? id)
                                                  (raise-syntax-error
                                                   #f
                                                   "not an identifier for definition"
                                                   defn
                                                   id)))
                                              ids))])
                            (syntax-case d (define-values define-syntaxes begin)
                              [(begin defn ...)
                               (loop (syntax->list (syntax (defn ...))))]
                              [(define-values (id ...) body)
                               (let ([ids (syntax->list (syntax (id ...)))])
                                 (check-ids d ids)
                                 (syntax-local-bind-syntaxes ids #f def-ctx)
                                 (list d))]
                              [(define-values . rest)
                               (raise-syntax-error
                                #f "ill-formed definition" stx d)]
                              [(define-syntaxes (id ...) rhs)
                               (let ([ids (syntax->list (syntax (id ...)))])
                                 (check-ids d ids)
                                 (with-syntax ([rhs (local-transformer-expand
                                                     #'rhs
                                                     'expression
                                                     null)])
                                   (syntax-local-bind-syntaxes ids #'rhs def-ctx)
                                   (list (quasisyntax/loc d (define-syntaxes #,ids rhs)))))]
                              [(define-syntaxes . rest)
                               (raise-syntax-error
                                #f "ill-formed definition" stx d)]
                              [_else
                               (raise-syntax-error
                                #f "not a definition" stx defn)])))
                        defns))))])
       (internal-definition-context-seal def-ctx)
       (let ([ids (apply append
                         (map
                          (lambda (d)
                            (syntax-case d ()
                              [(_ ids . __) (syntax->list (syntax ids))]))
                          defs))]
             [vbindings (apply append
                               (map (lambda (d)
                                      (syntax-case d (define-values)
                                        [(define-values ids rhs)
                                         (list #'(ids rhs))]
                                        [_ null]))
                                    defs))]
             [sbindings (apply append
                               (map (lambda (d)
                                      (syntax-case d (define-syntaxes)
                                        [(define-syntaxes ids rhs)
                                         (list #'(ids rhs))]
                                        [_ null]))
                                    defs))])
         (let ([dup (check-duplicate-identifier ids)])
           (when dup
             (raise-syntax-error #f "duplicate identifier" stx dup)))
         (with-syntax ([sbindings sbindings]
                       [vbindings vbindings]
                       [LSV letrec-syntaxes+values-id]
                       [(body ...)
                        (map (lambda (stx)
                               ;; add def-ctx:
                               (let ([q (local-expand #`(quote #,stx)
                                                      'expression
                                                      (list #'quote)
                                                      def-ctx)])
                                 (syntax-case q ()
                                   [(_ stx) #'stx])))
                             (syntax->list #'(body1 body ...)))])
           (syntax/loc stx
             (LSV sbindings vbindings
               body ...)))))]
    [(_ x body1 body ...)
     (raise-syntax-error #f "not a definition sequence" stx (syntax x))]))
