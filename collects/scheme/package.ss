#lang scheme/base
(require (for-syntax scheme/base
                     syntax/kerncase
                     syntax/boundmap))

(provide define-package
         open-package)

(begin-for-syntax
 (define-struct package (exports hidden)
   #:omit-define-syntaxes
   #:property prop:procedure (lambda (r stx)
                               (raise-syntax-error
                                #f
                                "misuse of a package name"
                                stx)))
 
 (define (reverse-mapping id exports hidden)
   (or (ormap (lambda (m)
                (and (free-identifier=? id (cdr m))
                     (car m)))
              exports)
       (ormap (lambda (h)
                (and (free-identifier=? id h)
                     ;; Name is inaccessible. Generate a temporary to
                     ;; avoid potential duplicate-definition errors
                     ;; when the name is bound in the same context as
                     ;; the package.
                     (car (generate-temporaries (list id)))))
              hidden)
       id)))

(define-syntax (define-package stx)
  (syntax-case stx ()
    [(_ pack-id exports form ...)
     (let ([id #'pack-id]
           [exports #'exports])
       (unless (identifier? id)
         (raise-syntax-error #f
                             "expected an identifier"
                             stx
                             id))
       (let ([exports
              (cond
               [(eq? (syntax-e exports) 'all-defined) #f]
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
                                         "expected a parenthesized sequence of identifiers to export"
                                         stx
                                         exports)])])
         (let* ([def-ctx (syntax-local-make-definition-context)]
                [ctx (cons (gensym 'intdef)
                           (let ([orig-ctx (syntax-local-context)])
                             (if (pair? orig-ctx)
                                 orig-ctx
                                 null)))]
                [pre-package-id (lambda (id)
                                  (identifier-remove-from-definition-context 
                                   id 
                                   def-ctx))]
                [kernel-forms (kernel-form-identifier-list)]
                [init-exprs (syntax->list #'(form ...))]
                [new-bindings (make-bound-identifier-mapping)]
                [fixup-sub-package (lambda (renamed-exports renamed-defines)
                                     (lambda (stx)
                                       (syntax-case* stx (define-syntaxes #%plain-app make-package quote-syntax 
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
                                                                    (pre-package-id id)
                                                                    ;; It's not accessible, so just hide the name
                                                                    ;;  to avoid re-binding errors.
                                                                    (car (generate-temporaries (list id)))))
                                                              (syntax->list #'(export ...)))])
                                            (syntax/loc stx
                                              (define-syntaxes (pack-id)
                                                (make-package
                                                 (lambda ()
                                                   (list (cons (quote-syntax export)
                                                               (quote-syntax renamed))
                                                         ...))
                                                 hidden))))]
                                         [_ stx])))])
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
                 [add-package-context (lambda (stx)
                                        (let ([q (local-expand #`(quote #,stx)
                                                               ctx
                                                               (list #'quote)
                                                               def-ctx)])
                                          (syntax-case q ()
                                            [(_ stx) #'stx])))])
             (let loop ([exprs init-exprs]
                        [rev-forms null]
                        [defined null])
               (cond
                [(null? exprs)
                 (internal-definition-context-seal def-ctx)
                 (let ([exports-renamed (map add-package-context (or exports null))]
                       [defined-renamed (bound-identifier-mapping-map new-bindings
                                                                      (lambda (k v) k))])
                   (for-each (lambda (ex renamed)
                               (unless (bound-identifier-mapping-get new-bindings
                                                                     renamed
                                                                     (lambda () #f))
                                 (raise-syntax-error #f
                                                     "no definition for exported identifier"
                                                     stx
                                                     ex)))
                             (or exports null)
                             exports-renamed)
                   (with-syntax ([(export ...) exports]
                                 [(renamed ...) exports-renamed]
                                 [(hidden ...)
                                  (begin
                                    (for-each (lambda (ex)
                                                (bound-identifier-mapping-put! new-bindings ex #f))
                                              exports-renamed)
                                    (filter
                                     values
                                     (bound-identifier-mapping-map new-bindings
                                                                   (lambda (k v) (and v k)))))])
                     #`(begin
                         #,@(map (fixup-sub-package exports-renamed defined-renamed) (reverse rev-forms))
                         (define-syntax pack-id
                           (make-package
                            (lambda ()
                              (list (cons (quote-syntax export)
                                          (quote-syntax renamed))
                                    ...))
                            (lambda ()
                              (list (quote-syntax hidden) ...)))))))]
                [else
                 (let ([expr (local-expand (car exprs) ctx kernel-forms def-ctx)])
                   (syntax-case expr (begin define-syntaxes define-values)
                     [(begin . rest)
                      (loop (append (syntax->list #'rest) (cdr exprs))
                            rev-forms
                            defined)]
                     [(define-syntaxes (id ...) rhs)
                      (andmap identifier? (syntax->list #'(id ...)))
                      (with-syntax ([rhs (local-transformer-expand
                                          #'rhs
                                          'expression
                                          null)])
                        (let ([ids (syntax->list #'(id ...))])
                          (syntax-local-bind-syntaxes ids #'rhs def-ctx)
                          (register-bindings! ids)
                          (loop (cdr exprs)
                                (cons #'(define-syntaxes (id ...) rhs)
                                      rev-forms)
                                (cons ids defined))))]
                     [(define-values (id ...) rhs)
                      (andmap identifier? (syntax->list #'(id ...)))
                      (let ([ids (syntax->list #'(id ...))])
                        (syntax-local-bind-syntaxes ids #f def-ctx)
                        (register-bindings! ids)
                        (loop (cdr exprs)
                              (cons expr rev-forms)
                              (cons ids defined)))]
                     [else
                      (loop (cdr exprs) 
                            (cons #`(define-values () (begin #,expr (values)))
                                  rev-forms)
                            defined)]))]))))))]))

(define-syntax (open-package stx)
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
             #'(begin
                 (define-syntaxes (intro ...)
                   (let ([rev-map (lambda (x)
                                    (reverse-mapping
                                     x
                                     (list (cons (quote-syntax a)
                                                 (quote-syntax b))
                                           ...)
                                     (list (quote-syntax h) ...)))])
                     (values (make-rename-transformer #'defined rev-map)
                             ...))))))))]))
