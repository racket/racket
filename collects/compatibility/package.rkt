#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     syntax/kerncase
                     syntax/boundmap
                     syntax/define
                     syntax/flatten-begin
                     syntax/context))

(provide define-package
         package-begin

         open-package
         open*-package

         define*
         define*-values
         define*-syntax
         define*-syntaxes

         (for-syntax package?
                     package-exported-identifiers
                     package-original-identifiers))

(define-for-syntax (do-define-* stx define-values-id)
  (syntax-case stx ()
    [(_ (id ...) rhs)
     (let ([ids (syntax->list #'(id ...))])
       (for-each (lambda (id)
                   (unless (identifier? id)
                     (raise-syntax-error
                      #f
                      "expected an identifier for definition"
                      stx
                      id)))
                 ids)
       (with-syntax ([define-values define-values-id])
         (syntax/loc stx
           (define-values (id ...) rhs))))]))
(define-syntax (-define*-values stx)
  (do-define-* stx #'define-values))
(define-syntax (-define*-syntaxes stx)
  (do-define-* stx #'define-syntaxes))
(define-syntax (define*-values stx)
  (syntax-case stx ()
    [(_ (id ...) rhs)
     (syntax-property
      (syntax/loc stx (-define*-values (id ...) rhs))
      'certify-mode
      'transparent-binding)]))
(define-syntax (define*-syntaxes stx)
  (syntax-case stx ()
    [(_ (id ...) rhs)
     (syntax-property
      (syntax/loc stx (-define*-syntaxes (id ...) rhs))
      'certify-mode
      'transparent-binding)]))

(define-syntax (define* stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda)])
    (quasisyntax/loc stx
      (define*-values (#,id) #,rhs))))
(define-syntax (define*-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda)])
    (quasisyntax/loc stx
      (define*-syntaxes (#,id) #,rhs))))

(begin-for-syntax
 (define-struct package (exports hidden)
   #:omit-define-syntaxes
   #:property prop:procedure (lambda (r stx)
                               (raise-syntax-error
                                #f
                                "misuse of a package name"
                                stx)))

 (define (generate-hidden id)
   ;; Like `generate-temporaries', but preserve the symbolic name
   ((make-syntax-introducer) (datum->syntax #f (syntax-e id))))
 
 (define (reverse-mapping who id exports hidden)
   (or (ormap (lambda (m)
                (and (free-identifier=? id (cdr m))
                     (car m)))
              exports)
       (ormap (lambda (h)
                (and (free-identifier=? id h)
                     ;; Not at top level, where free-id=? is unreliable,
                     ;; and re-definition is ok:
                     (identifier-binding id)
                     ;; Name is inaccessible. Generate a temporary to
                     ;; avoid potential duplicate-definition errors
                     ;; when the name is bound in the same context as
                     ;; the package.
                     (generate-hidden id)))
              hidden)
       id)))

(define-for-syntax (move-props orig new)
  (datum->syntax new
                 (syntax-e new)
                 orig
                 orig))

(define-for-syntax code-insp (variable-reference->module-declaration-inspector
                              (#%variable-reference)))
(define-for-syntax (disarm* stx)
  (cond
   [(and (syntax? stx)
         (pair? (syntax-e stx)))
    (let ([stx (syntax-disarm stx code-insp)])
      (datum->syntax stx (disarm* (syntax-e stx)) stx stx))]
   [(pair? stx) (cons (disarm* (car stx)) (disarm* (cdr stx)))]
   [else stx]))

(define-for-syntax (do-define-package stx exp-stx)
  (syntax-case exp-stx ()
    [(_ pack-id mode exports form ...)
     (let ([id #'pack-id]
           [exports #'exports]
           [mode (syntax-e #'mode)])
       (unless (eq? mode '#:begin)
         (unless (identifier? id)
           (raise-syntax-error #f
                               "expected an identifier"
                               stx
                               id)))
       (let ([exports
              (cond
               [(syntax->list exports)
                => (lambda (l)
                     (for-each (lambda (i)
                                 (unless (identifier? i)
                                   (raise-syntax-error #f
                                                       "expected identifier to export"
                                                       stx
                                                       i)))
                               l)
                     (let ([dup-id (check-duplicate-identifier l)])
                       (when dup-id
                         (raise-syntax-error
                          #f
                          "duplicate export"
                          stx
                          dup-id)))
                     l)]
               [else (raise-syntax-error #f
                                         (format "expected a parenthesized sequence of identifiers ~a"
                                                 (case mode
                                                   [(#:only) "to export"]
                                                   [(#:all-defined-except) "to exclude from export"]
                                                   [else (format "for ~a" mode)]))
                                         stx
                                         exports)])])
         (let* ([def-ctx (syntax-local-make-definition-context)]
                [ctx (generate-expand-context #t)]
                [pre-package-id (lambda (id def-ctxes)
                                  (identifier-remove-from-definition-context 
                                   id 
                                   def-ctxes))]
                [kernel-forms (list*
                               #'-define*-values
                               #'-define*-syntaxes
                               (kernel-form-identifier-list))]
                [init-exprs (syntax->list #'(form ...))]
                [new-bindings (make-bound-identifier-mapping)]
                [fixup-sub-package (lambda (renamed-exports renamed-defines def-ctxes)
                                     (lambda (stx)
                                       (syntax-case* (disarm* stx) (define-syntaxes #%plain-app make-package quote-syntax 
                                                                     list cons #%plain-lambda)
                                                     free-transformer-identifier=?
                                         [(define-syntaxes (pack-id)
                                            (#%plain-app
                                             make-package
                                             (#%plain-lambda ()
                                                             (#%plain-app list 
                                                                          (#%plain-app cons 
                                                                                       (quote-syntax export)
                                                                                       (quote-syntax renamed))
                                                                          ...))
                                             hidden))
                                          (with-syntax ([(export ...)
                                                         (map (lambda (id)
                                                                (if (or (ormap (lambda (e-id)
                                                                                 (bound-identifier=? id e-id))
                                                                               renamed-exports)
                                                                        (not (ormap (lambda (e-id)
                                                                                      (bound-identifier=? id e-id))
                                                                                    renamed-defines)))
                                                                    ;; Need to preserve the original
                                                                    (pre-package-id id def-ctxes)
                                                                    ;; It's not accessible, so just hide the name
                                                                    ;;  to avoid re-binding errors. (Is this necessary,
                                                                    ;;  or would `pre-package-id' take care of it?)
                                                                    (generate-hidden id)))
                                                              (syntax->list #'(export ...)))])
                                            (syntax/loc stx
                                              (define-syntaxes (pack-id)
                                                (make-package
                                                 (lambda ()
                                                   (list (cons (quote-syntax export)
                                                               (quote-syntax renamed))
                                                         ...))
                                                 hidden))))]
                                         [_ stx])))]
                [complement (lambda (bindings ids)
                              (let ([tmp (make-bound-identifier-mapping)])
                                (bound-identifier-mapping-for-each bindings
                                                                   (lambda (k v)
                                                                     (bound-identifier-mapping-put! tmp k #t)))
                                (for-each (lambda (id)
                                            (bound-identifier-mapping-put! tmp id #f))
                                          ids)
                                (filter
                                 values
                                 (bound-identifier-mapping-map tmp (lambda (k v) (and v k))))))])
           (let ([register-bindings!
                  (lambda (ids)
                    (for-each (lambda (id)
                                (when (bound-identifier-mapping-get new-bindings id (lambda () #f))
                                  (raise-syntax-error #f 
                                                      "duplicate binding"
                                                      stx
                                                      id))
                                (bound-identifier-mapping-put! new-bindings
                                                               id
                                                               #t))
                              ids))]
                 [add-package-context (lambda (def-ctxes)
                                        (lambda (stx)
                                          (let ([q (local-expand #`(quote #,stx)
                                                                 ctx
                                                                 (list #'quote)
                                                                 def-ctxes)])
                                            (syntax-case q ()
                                              [(_ stx) #'stx]))))])
             (let loop ([exprs init-exprs]
                        [rev-forms null]
                        [def-ctxes (list def-ctx)])
               (cond
                [(null? exprs)
                 (for-each (lambda (def-ctx)
                             (internal-definition-context-seal def-ctx))
                           def-ctxes)
                 (let ([exports-renamed (map (add-package-context def-ctxes) exports)]
                       [defined-renamed (bound-identifier-mapping-map new-bindings
                                                                      (lambda (k v) k))])
                   (for-each (lambda (ex renamed)
                               (unless (bound-identifier-mapping-get new-bindings
                                                                     renamed
                                                                     (lambda () #f))
                                 (raise-syntax-error #f
                                                     (format "no definition for ~a identifier"
                                                             (case mode
                                                               [(#:only) "exported"]
                                                               [(#:all-defined-except) "excluded"]))
                                                     stx
                                                     ex)))
                             exports
                             exports-renamed)
                   (let-values ([(exports exports-renamed)
                                 (if (memq mode '(#:only #:begin))
                                     (values exports exports-renamed)
                                     (let ([all-exports-renamed (complement new-bindings exports-renamed)])
                                       ;; In case of define*, get only the last definition:
                                       (let ([tmp (make-bound-identifier-mapping)])
                                         (for-each (lambda (id)
                                                     (bound-identifier-mapping-put!
                                                      tmp
                                                      ((add-package-context def-ctxes) 
                                                       (pre-package-id id def-ctxes))
                                                      #t))
                                                   all-exports-renamed)
                                         (let* ([exports-renamed (bound-identifier-mapping-map tmp (lambda (k v) k))]
                                                [exports (map (lambda (id) (pre-package-id id def-ctxes))
                                                              exports-renamed)])
                                           (values exports exports-renamed)))))]
                                [(prune)
                                 (lambda (stx)
                                   (identifier-prune-lexical-context stx (list (syntax-e stx) '#%top)))])
                     (with-syntax ([(export ...) (map prune exports)]
                                   [(renamed ...) (map prune exports-renamed)]
                                   [(hidden ...) (map prune (complement new-bindings exports-renamed))])
                       (let ([body (map (fixup-sub-package exports-renamed defined-renamed def-ctxes) 
                                        (reverse rev-forms))])
                         (if (eq? mode '#:begin)
                             (if (eq? 'expression (syntax-local-context))
                                 (quasisyntax/loc stx (let () #,@body))
                                 (quasisyntax/loc stx (begin #,@body)))
                             (quasisyntax/loc stx
                               (begin
                                 #,@(if (eq? 'top-level (syntax-local-context))
                                        ;; delcare all bindings before they are used:
                                        #`((define-syntaxes #,defined-renamed (values)))
                                        null)
                                 #,@body
                                 (define-syntax pack-id
                                   (make-package
                                    (lambda ()
                                      (list (cons (quote-syntax export)
                                                  (quote-syntax renamed))
                                            ...))
                                    (lambda ()
                                      (list (quote-syntax hidden) ...)))))))))))]
                [else
                 (let ([expr (local-expand (car exprs)
                                           ctx
                                           kernel-forms 
                                           def-ctxes)])
                   (syntax-case expr (begin)
                     [(begin . rest)
                      (loop (append (flatten-begin expr) (cdr exprs))
                            rev-forms
                            def-ctxes)]
                     [(def (id ...) rhs)
                      (and (or (free-identifier=? #'def #'define-syntaxes)
                               (free-identifier=? #'def #'-define*-syntaxes))
                           (andmap identifier? (syntax->list #'(id ...))))
                      (with-syntax ([rhs (local-transformer-expand
                                          #'rhs
                                          'expression
                                          null)])
                        (let ([star? (free-identifier=? #'def #'-define*-syntaxes)]
                              [ids (syntax->list #'(id ...))])
                          (let* ([def-ctx (if star?
                                              (syntax-local-make-definition-context (car def-ctxes))
                                              (last def-ctxes))]
                                 [ids (map
                                       (lambda (id) (syntax-property id 'unshadowable #t))
                                       (if star? 
                                           (map (add-package-context (list def-ctx)) ids)
                                           ids))])
                            (syntax-local-bind-syntaxes ids #'rhs def-ctx)
                            (register-bindings! ids)
                            (loop (cdr exprs)
                                  (cons (move-props expr #`(define-syntaxes #,ids rhs))
                                        rev-forms)
                                  (if star? (cons def-ctx def-ctxes) def-ctxes)))))]
                     [(def (id ...) rhs)
                      (and (or (free-identifier=? #'def #'define-values)
                               (free-identifier=? #'def #'-define*-values))
                           (andmap identifier? (syntax->list #'(id ...))))
                      (let ([star? (free-identifier=? #'def #'-define*-values)]
                            [ids (syntax->list #'(id ...))])
                        (let* ([def-ctx (if star?
                                            (syntax-local-make-definition-context (car def-ctxes))
                                            (last def-ctxes))]
                               [ids (map
                                     (lambda (id) (syntax-property id 'unshadowable #t))
                                     (if star? 
                                         (map (add-package-context (list def-ctx)) ids)
                                         ids))])
                          (syntax-local-bind-syntaxes ids #f def-ctx)
                          (register-bindings! ids)
                          (loop (cdr exprs)
                                (cons (move-props expr #`(define-values #,ids rhs)) rev-forms)
                                (if star? (cons def-ctx def-ctxes) def-ctxes))))]
                     [else
                      (loop (cdr exprs) 
                            (cons (if (and (eq? mode '#:begin)
                                           (null? (cdr exprs)))
                                      expr
                                      #`(define-values () (begin #,expr (values))))
                                  rev-forms)
                            def-ctxes)]))]))))))]))

(define-syntax (define-package stx)
  (syntax-case stx ()
    [(_ id #:all-defined form ...)
     (do-define-package stx #'(define-package id #:all-defined () form ...))]
    [(_ id #:all-defined-except ids form ...)
     (do-define-package stx stx)]
    [(_ id #:only ids form ...)
     (do-define-package stx stx)]
    [(_ id ids form ...)
     (do-define-package stx #'(define-package id #:only ids form ...))]))

(define-syntax (package-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (do-define-package stx #'(define-package #f #:begin () form ...))]))

(define-for-syntax (do-open stx define-syntaxes-id)
  (syntax-case stx ()
    [(_ pack-id)
     (let ([id #'pack-id])
       (unless (identifier? id)
         (raise-syntax-error #f
                             "expected an identifier for a package"
                             stx
                             id))
       (let ([v (syntax-local-value id (lambda () #f))])
         (unless (package? v)
           (raise-syntax-error #f
                               "identifier is not bound to a package"
                               stx
                               id))
         (let ([introduce (syntax-local-make-delta-introducer 
                           (syntax-local-introduce id))])
           (with-syntax ([(intro ...)
                          (map (lambda (i)
                                 (syntax-local-introduce
                                  (syntax-local-get-shadower
                                   (introduce i))))
                               (map car ((package-exports v))))]
                         [(defined ...)
                          (map (lambda (v) (syntax-local-introduce (cdr v)))
                               ((package-exports v)))]
                         [((a . b) ...) (map (lambda (p)
                                               (cons (syntax-local-introduce (car p))
                                                     (syntax-local-introduce (cdr p))))
                                             ((package-exports v)))]
                         [(h ...) (map syntax-local-introduce ((package-hidden v)))])
             (syntax-property
              #`(#,define-syntaxes-id (intro ...)
                  (let ([rev-map (lambda (x)
                                   (reverse-mapping
                                    'pack-id
                                    x
                                    (list (cons (quote-syntax a)
                                                (quote-syntax b))
                                          ...)
                                    (list (quote-syntax h) ...)))])
                    (values (make-rename-transformer #'defined rev-map)
                            ...)))
              'disappeared-use
              (syntax-local-introduce id))))))]))

(define-syntax (open-package stx)
  (do-open stx #'define-syntaxes))
(define-syntax (open*-package stx)
  (do-open stx #'define*-syntaxes))

(define-for-syntax (package-exported-identifiers id)
  (let ([v (and (identifier? id)
                (syntax-local-value id (lambda () #f)))])
    (unless (package? v)
      (if (identifier? id)
          (raise-arguments-error 'package-exported-identifiers "identifier is not bound to a package"
                                 "identifier" id)
          (raise-argument-error 'package-exported-identifiers "identifier?" id)))
    (let ([introduce (syntax-local-make-delta-introducer 
                      (syntax-local-introduce id))])
      (map (lambda (i)
             (syntax-local-introduce
              (syntax-local-get-shadower
               (introduce (car i)))))
           ((package-exports v))))))

(define-for-syntax (package-original-identifiers id)
  (let ([v (and (identifier? id)
                (syntax-local-value id (lambda () #f)))])
    (unless (package? v)
      (if (identifier? id)
          (raise-arguments-error 'package-original-identifiers "identifier is not bound to a package"
                                 "identifier" id)
          (raise-argument-error 'package-original-identifiers "identifier?" id)))
    (map cdr ((package-exports v)))))
