#lang racket/base
(require (for-syntax racket/base
                     syntax/kerncase
                     racket/syntax
                     "private/stxparamkey.rkt")
         (for-meta 2  ; for wrap-et-param
                   racket/base
                   syntax/kerncase)
         "private/stxparam.rkt"
         "private/local.rkt")

(provide splicing-let-syntax
         splicing-let-syntaxes
         splicing-letrec-syntax
         splicing-letrec-syntaxes
         splicing-let
         splicing-let-values
         splicing-letrec
         splicing-letrec-values
         splicing-letrec-syntaxes+values
         splicing-local
         splicing-syntax-parameterize
         splicing-parameterize)

(module syntax/loc/props racket/base
  (require (for-syntax racket/base))
  (provide syntax/loc/props quasisyntax/loc/props)

  (define-syntaxes [syntax/loc/props quasisyntax/loc/props]
    (let ()
      (define (mk-syntax/loc/props syntax-id)
        (λ (stx)
          (syntax-case stx ()
            [(_ src-expr template)
             #`(let ([src src-expr])
                 (datum->syntax (quote-syntax #,stx) (syntax-e (#,syntax-id template)) src src))])))
      (values (mk-syntax/loc/props #'syntax)
              (mk-syntax/loc/props #'quasisyntax)))))

(require (for-syntax 'syntax/loc/props)
         (for-meta 2 'syntax/loc/props))

(define-syntax (splicing-local stx)
  (do-local stx (lambda (def-ctx expand-context sbindings vbindings bodys)
                  (if (eq? 'expression (syntax-local-context))
                      (quasisyntax/loc stx
                        (letrec-syntaxes+values
                         #,sbindings
                         #,vbindings
                         #,@bodys))
                      ;; Since we alerady have bindings for the current scopes,
                      ;; add an extra scope for re-binding:
                      (let ([i (make-syntax-introducer #t)])
                        (with-syntax ([([s-ids s-rhs] ...) (i sbindings)]
                                      [([(v-id ...) v-rhs] ...) (i vbindings)]
                                      [(body ...) (i bodys)]
                                      [(marked-id markless-id)
                                       (let ([id #'id])
                                         ;; The marked identifier should have both the extra
                                         ;; scope and the intdef scope, to be removed from
                                         ;; definitions expanded from `body`:
                                         (list (i (internal-definition-context-introduce def-ctx id))
                                               id))])
                          (with-syntax ([(top-decl ...)
                                         (if (equal? 'top-level (syntax-local-context))
                                             #'((define-syntaxes (v-id ... ...) (values)))
                                             null)])
                            (quasisyntax/loc stx
                              (begin
                                top-decl ...
                                (define-syntaxes s-ids s-rhs) ...
                                (define-values (v-id ...) v-rhs) ...
                                (splicing-let-start/body marked-id markless-id body)
                                ...)))))))))

(define-syntax (splicing-let-syntax stx)
  (do-let-syntax stx #f #f #'let-syntax #'define-syntaxes #f))

(define-syntax (splicing-let-syntaxes stx)
  (do-let-syntax stx #f #t #'let-syntaxes #'define-syntaxes #f))

(define-syntax (splicing-letrec-syntax stx)
  (do-let-syntax stx #t #f #'letrec-syntax #'define-syntaxes #f))

(define-syntax (splicing-letrec-syntaxes stx)
  (do-let-syntax stx #t #t #'letrec-syntaxes #'define-syntaxes #f))

(define-syntax (splicing-let stx)
  (do-let-syntax stx #f #f #'let #'define-values #f))

(define-syntax (splicing-let-values stx)
  (do-let-syntax stx #f #t #'let-values #'define-values #f))

(define-syntax (splicing-letrec stx)
  (do-let-syntax stx #t #f #'letrec #'define-values #t))

(define-syntax (splicing-letrec-values stx)
  (do-let-syntax stx #t #t #'letrec-values #'define-values #t))

(define-for-syntax (do-let-syntax stx rec? multi? let-id def-id need-top-decl?)
  (syntax-case stx ()
    [(_ ([ids expr] ...) body ...)
     (let ([all-ids (map ((if multi? check-ids check-id) stx)
                         (syntax->list #'(ids ...)))])
       (check-dup-binding stx all-ids)
       (if (eq? 'expression (syntax-local-context))
           (with-syntax ([LET let-id])
             (syntax/loc stx
               (LET ([ids expr] ...)
                 (#%expression body)
                 ...)))
           (with-syntax ([((id ...) ...)
                          (for/list ([ids (in-list all-ids)])
                            (for/list ([id (in-list ids)])
                              (syntax-property id 'definition-intended-as-local #t)))]
                         [DEF def-id]
                         [rec? rec?]
                         [(marked-id markless-id)
                          (let ([id #'id])
                            (list ((make-syntax-introducer #t) id)
                                  id))])
             (with-syntax ([(top-decl ...)
                            (if (and need-top-decl? (equal? 'top-level (syntax-local-context)))
                                #'((define-syntaxes (id ... ...) (values)))
                                null)])
               
               (syntax/loc stx
                 (begin
                   (splicing-let-start/def marked-id markless-id #f top-decl) ...
                   (splicing-let-start/def marked-id markless-id rec? (DEF (id ...) expr))
                   ...
                   (splicing-let-start/body marked-id markless-id body)
                   ...))))))]))

(define-syntax (splicing-let-start/def stx)
  (syntax-case stx ()
    [(_ marked-id markless-id rec? (DEF ids rhs))
     ;; Add the mark to every definition's identifiers; also
     ;; add to the body, if it's a recursively scoped binding:
     (let ([i (make-syntax-delta-introducer #'marked-id #'markless-id)])
       #`(DEF #,(i #'ids) #,(if (syntax-e #'rec?)
                                (i #'rhs)
                                #'rhs)))]))

(define-syntax (splicing-let-start/body stx)
  (syntax-case stx ()
    [(_ marked-id markless-id body)
     ;; Tenatively add the mark to the body,; we'll remove it on every
     ;; bit of syntax that turns out to be a binding:
     (let ([i (make-syntax-delta-introducer #'marked-id #'markless-id)])
       #`(splicing-let-body marked-id markless-id #,(i #'body)))]))

(define-for-syntax ((maybe unintro) form)
  (if (syntax-property form 'definition-intended-as-local)
      form
      (unintro form)))

(define-syntax (splicing-let-body stx)
  (syntax-case stx ()
    [(_ marked-id markless-id body)
     (let ([unintro (lambda (form)
                      ((make-syntax-delta-introducer #'marked-id #'markless-id) form 'remove))]
           [body (local-expand #'body (syntax-local-context) #f)])
       (syntax-case body (begin
                           define-values
                           define-syntaxes
                           begin-for-syntax
                           module
                           module*
                           #%require
                           #%provide
                           #%declare)
         [(begin form ...)
          (syntax/loc/props body
            (begin (splicing-let-body marked-id markless-id form) ...))]
         [(define-values ids rhs)
          (quasisyntax/loc/props body
            (define-values #,(map (maybe unintro) (syntax->list #'ids)) rhs))]
         [(define-syntaxes ids rhs)
          (quasisyntax/loc/props body
            (define-syntaxes #,(map (maybe unintro) (syntax->list #'ids)) rhs))]
         [(begin-for-syntax e ...)
          (syntax/loc/props body
            (begin-for-syntax (splicing-let-body/et marked-id markless-id e) ...))]
         [(module . _) (unintro body)]
         [(module* . _) body]
         [(#%require . _) (unintro body)]
         [(#%provide . _) body]
         [(#%declare . _) body]
         [_ body]))]))

(begin-for-syntax
  (define-for-syntax ((maybe unintro) form)
    (if (syntax-property form 'definition-intended-as-local)
        form
        (unintro form)))

  (define-syntax (splicing-let-body/et stx)
    (syntax-case stx ()
      [(_ marked-id markless-id body)
       (let* ([unintro (lambda (form)
                         ((make-syntax-delta-introducer #'marked-id #'markless-id) form 'remove))]
              [body (local-expand #'body (syntax-local-context) #f)])
         (syntax-case body (begin
                             define-values
                             define-syntaxes
                             begin-for-syntax
                             module
                             module*
                             #%require
                             #%provide
                             #%declare)
           [(begin form ...)
            (syntax/loc/props body
              (begin (splicing-let-body/et marked-id markless-id form) ...))]
           [(define-values ids rhs)
            (quasisyntax/loc/props body
              (define-values #,(map (maybe unintro) (syntax->list #'ids)) rhs))]
           [(define-syntaxes ids rhs)
            (quasisyntax/loc/props body
              (define-syntaxes #,(map (maybe unintro) (syntax->list #'ids)) rhs))]
           [(begin-for-syntax . es)
            ;; Give up on splicing definitions at phase level 2 and deeper:
            body]
           [(module . _) (unintro body)]
           [(module* . _) body]
           [(#%require . _) (unintro body)]
           [(#%provide . _) body]
           [(#%declare . _) body]
           [_ body]))])))

(define-for-syntax ((check-id stx) id-stx)
  (unless (identifier? id-stx)
    (raise-syntax-error #f "expected an identifier" stx id-stx))
  (list id-stx))

(define-for-syntax ((check-ids stx) ids-stx)
  (let ([ids (syntax->list ids-stx)])
    (unless ids
      (raise-syntax-error 
       #f
       "expected a parenthesized sequence of identifiers"
       stx
       ids-stx))
    (for-each (check-id stx) ids)
    ids))

(define-for-syntax (check-dup-binding stx idss)
  (let ([dup-id (check-duplicate-identifier (apply append idss))])
    (when dup-id
      (raise-syntax-error #f "duplicate binding" stx dup-id))))

;; ----------------------------------------

(define-syntax (splicing-letrec-syntaxes+values stx)
  (syntax-case stx ()
    [(_ ([sids sexpr] ...) ([vids vexpr] ...) body ...)
     (let* ([all-sids (map (check-ids stx)
                           (syntax->list #'(sids ...)))]
            [all-vids (map (check-ids stx)
                           (syntax->list #'(vids ...)))]
            [all-ids (append all-sids all-vids)])
       (check-dup-binding stx all-ids)
       (if (eq? 'expression (syntax-local-context))
           (syntax/loc stx
             (letrec-syntaxes+values ([sids sexpr] ...) ([vids vexpr] ...)
               (#%expression body) ...))
           (with-syntax ([((vid ...) ...) all-vids]
                         [(marked-id markless-id)
                          (let ([id #'id])
                            (list ((make-syntax-introducer #t) id)
                                  id))])
             (with-syntax ([(top-decl ...)
                            (if (equal? 'top-level (syntax-local-context))
                                #'((define-syntaxes (vid ... ...) (values)))
                                null)])
               (syntax/loc stx
                 (begin
                   (splicing-let-start/def marked-id markless-id #f top-decl) ...
                   (splicing-let-start/def marked-id markless-id #t (define-syntaxes sids sexpr))
                   ...
                   (splicing-let-start/def marked-id markless-id #t (define-values (vid ...) vexpr))
                   ...
                   (splicing-let-start/body marked-id markless-id body ...)))))))]))

;; ----------------------------------------

(define-syntax (splicing-syntax-parameterize stx)
  (if (eq? 'expression (syntax-local-context))
      ;; Splicing is no help in an expression context:
      (do-syntax-parameterize stx #'let-syntaxes #f #f)
      ;; Let `syntax-parameterize' check syntax, then continue
      (do-syntax-parameterize stx #'ssp-let-syntaxes #t #t)))

(define-syntax (ssp-let-syntaxes stx)
  (syntax-case stx ()
    [(_ ([(id) rhs] ...) orig-id ... (llk binds body ...))
     #'(begin
         ;; Evaluate each RHS only once:
         (define-syntax id rhs) ...
         ;; Partially expand `body' to push down `let-syntax':
         (expand-ssp-body binds [orig-id ...] body)
         ...)]))

(define-syntax (expand-ssp-body stx)
  (syntax-case stx ()
    [(_ binds orig-ids body)
     (let ([ctx (syntax-local-make-definition-context #f #f)])
       (let ([body (with-continuation-mark
                    current-parameter-environment
                    (extend-parameter-environment (current-parameter-environment) #'binds)
                    (local-expand #'(force-expand body)
                                  (syntax-local-context)
                                  null ;; `force-expand' actually determines stopping places
                                  ctx))])
         (let ([body
                ;; Extract expanded body out of `body':
                (syntax-case body (quote)
                  [(quote body) #'body])])
           (syntax-case body ( begin
                               define-values
                               define-syntaxes
                               begin-for-syntax
                               module
                               module*
                               #%require
                               #%provide
                               #%declare )
              [(begin expr ...)
               (syntax/loc/props body
                 (begin (expand-ssp-body binds orig-ids expr) ...))]
              [(define-values (id ...) rhs)
               (syntax/loc/props body
                 (define-values (id ...)
                   (let-local-keys binds rhs)))]
              [(define-syntaxes ids rhs)
               (syntax/loc/props body
                 (define-syntaxes ids (wrap-param-et rhs binds)))]
              [(begin-for-syntax e ...)
               (syntax/loc/props body
                 (begin-for-syntax (wrap-param-et e binds) ...))]
              [(module . _) body]
              [(module* name #f form ...)
               (datum->syntax body
                              (list #'module* #'name #f
                                    #`(expand-ssp-module-begin
                                       binds orig-ids
                                       #,body name form ...))
                              body)]
              [(module* . _) body]
              [(#%require . _) body]
              [(#%provide . _) body]
              [(#%declare . _) body]
              [expr (syntax/loc body
                      (let-local-keys binds expr))]))))]))

(define-syntax (expand-ssp-module-begin stx)
  (syntax-case stx ()
    [(_ binds orig-ids mod-form mod-name-id body-form ...)
     (unless (eq? (syntax-local-context) 'module-begin)
       (raise-syntax-error #f "only allowed in module-begin context" stx))
     (with-syntax ([new-binds (update-parameter-keys #'orig-ids #'binds)])
       (with-continuation-mark
        current-parameter-environment
        (extend-parameter-environment (current-parameter-environment)
                                      #'new-binds)
        (let* ([forms (syntax->list #'(body-form ...))]
                ;; emulate how the macroexpander expands module bodies and introduces #%module-begin
                [body (if (= (length forms) 1)
                          (let ([body (local-expand (car forms) 'module-begin #f)])
                            (syntax-case body (#%plain-module-begin)
                              [(#%plain-module-begin . _) body]
                              [_ (datum->syntax #'mod-form (list '#%module-begin body) #'mod-form)]))
                          (datum->syntax #'mod-form (list* '#%module-begin forms) #'mod-form))]
                [body (syntax-property body 'enclosing-module-name (syntax-e #'mod-name-id))]
                [body (local-expand body 'module-begin #f)])
           (syntax-case body (#%plain-module-begin)
             [(#%plain-module-begin form ...)
              (syntax/loc/props body
                (#%plain-module-begin
                 (expand-ssp-body new-binds orig-ids form) ...))]
             [_ (raise-syntax-error
                 #f "expansion of #%module-begin is not a #%plain-module-begin form" body)]))))]))

(define-syntax (letrec-syntaxes/trans stx)
  (syntax-case stx ()
    [(_ bindings body)
     (syntax-property
      #'(letrec-syntaxes bindings body)
      'certify-mode
      'transparent)]))

(define-syntax (force-expand stx)
  (syntax-case stx ()
    [(_ stx)
     ;; Expand `stx' to reveal type of form, and then preserve it via
     ;; `quote':
     (syntax-property
      #`(quote #,(local-expand #'stx
                               'module
                               (kernel-form-identifier-list)
                               #f))
      'certify-mode
      'transparent)]))

(begin-for-syntax
 (define-syntax (wrap-param-et stx)
   (syntax-case stx ()
     [(_ e binds)
      (let ([as-expression
             (lambda ()
               #'(with-continuation-mark
                  current-parameter-environment
                  (extend-parameter-environment (current-parameter-environment) (quote-syntax binds))
                  e))])
        (if (eq? (syntax-local-context) 'expression)
            (as-expression)
            (let ([e (local-expand #'e
                                   (syntax-local-context)
                                   (kernel-form-identifier-list)
                                   #f)])
              (syntax-case e (begin
                               define-syntaxes define-values
                               begin-for-syntax
                               module module*
                               #%require #%provide #%declare
                               quote-syntax)
                [(begin form ...)
                 (syntax/loc/props e
                   (begin (wrap-param-et form binds) ...))]
                [(define-syntaxes . _) e]
                [(begin-for-syntax . _) e]
                [(define-values ids rhs)
                 (syntax/loc/props e
                   (define-values ids (wrap-param-et rhs binds)))]
                [(module . _) e]
                [(module* n #f form ...)
                 (datum->syntax
                  e
                  (syntax-e #'(module* n #f (wrap-param-et form binds) ...))
                  e
                  e)]
                [(module* . _) e]
                [(#%require . _) e]
                [(#%provide . _) e]
                [(#%declare . _) e]
                [(quote-syntax . _) e]
                [else (as-expression)]))))])))

;; ----------------------------------------

(define-syntax (splicing-parameterize stx)
  (syntax-case stx ()
    [(_ ([param value] ...) body ...)
     (with-syntax ([(param/checked ...)
                    (for/list ([param-stx (in-list (syntax->list #'(param ...)))])
                      #`(let ([param-val #,param-stx])
                          (unless (parameter? param-val)
                            (raise-argument-error 'splicing-parameterize "parameter?" param-val))
                          param-val))])
       (if (eq? (syntax-local-context) 'expression)
           #'(parameterize ([param/checked value] ...)
               body ...)
           (let ([introduce (make-syntax-introducer #t)])
             (with-syntax ([scopeless-id (datum->syntax #f 'scopeless-id)]
                           [scoped-id (introduce (datum->syntax #f 'scoped-id))]
                           [(scoped-body ...) (map introduce (syntax->list #'(body ...)))]
                           ; make sure the parameterization can be GC’d at the top/module level
                           [(free-parameterization-expr ...)
                            (case (syntax-local-context)
                              [(top-level module) #'((set! new-parameterization #f))]
                              [else #'()])])
               #'(begin
                   (define new-parameterization
                     (parameterize ([param/checked value] ...)
                       (current-parameterization)))
                   (splicing-parameterize-body
                    scopeless-id scoped-id new-parameterization scoped-body) ...
                   free-parameterization-expr ...)))))]))

(define-syntax (splicing-parameterize-body stx)
  (syntax-case stx ()
    [(_ scopeless-id scoped-id parameterization body)
     (let* ([introducer (make-syntax-delta-introducer #'scoped-id #'scopeless-id)]
            [unintro (λ (stx) (introducer stx 'remove))]
            [expanded-body (local-expand #'body (syntax-local-context)
                                         (kernel-form-identifier-list))])
       (kernel-syntax-case expanded-body #f
         [(begin new-body ...)
          (syntax/loc/props expanded-body
            (begin
              (splicing-parameterize-body scopeless-id scoped-id parameterization new-body)
              ...))]
         [(define-values ids rhs)
          (quasisyntax/loc/props expanded-body
            (define-values #,(map (maybe unintro) (syntax->list #'ids))
              (call-with-parameterization parameterization (λ () rhs))))]
         [(define-syntaxes ids rhs)
          (quasisyntax/loc/props expanded-body
            (define-syntaxes #,(map (maybe unintro) (syntax->list #'ids)) rhs))]
         [(begin-for-syntax . _) expanded-body]
         [(module . _) (unintro expanded-body)]
         [(module* . _) expanded-body]
         [(#%require . _) (unintro expanded-body)]
         [(#%provide . _) expanded-body]
         [(#%declare . _) expanded-body]
         [expr
          (syntax/loc/props expanded-body
            (call-with-parameterization parameterization (λ () expr)))]))]))

