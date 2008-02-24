#lang scheme/base

(require (for-syntax scheme/base
                     syntax/context
                     syntax/kerncase))

(provide local)

(define-syntax (local stx)
  (syntax-case stx ()
    [(_ (defn ...) body1 body ...)
     (let ([defs (let ([expand-context (generate-expand-context)])
                   (let loop ([defns (syntax->list (syntax (defn ...)))])
                     (apply
                      append
                      (map
                       (lambda (defn)
                         (let ([d (local-expand
                                   defn
                                   expand-context
                                   (kernel-form-identifier-list))]
                               [check-ids (lambda (ids)
                                            (for-each
                                             (lambda (id)
                                               (unless (identifier? id)
                                                 (raise-syntax-error
                                                  #f
                                                  "not an identifier for definition"
                                                  stx
                                                  id)))
                                             ids))])
                           (syntax-case d (define-values define-syntaxes begin)
                             [(begin defn ...)
                              (loop (syntax->list (syntax (defn ...))))]
                             [(define-values (id ...) body)
                              (begin
                                (check-ids (syntax->list (syntax (id ...))))
                                (list d))]
                             [(define-values . rest)
                              (raise-syntax-error
                               #f "ill-formed definition" stx d)]
                             [(define-syntaxes (id ...) body)
                              (begin
                                (check-ids (syntax->list (syntax (id ...))))
                                (list d))]
                             [(define-syntaxes . rest)
                              (raise-syntax-error
                               #f "ill-formed definition" stx d)]
                             [_else
                              (raise-syntax-error
                               #f "not a definition" stx defn)])))
                       defns))))])
       (let ([ids (apply append
                         (map
                          (lambda (d)
                            (syntax-case d ()
                              [(_ ids . __) (syntax->list (syntax ids))]))
                          defs))])
         (let ([dup (check-duplicate-identifier ids)])
           (when dup
             (raise-syntax-error #f "duplicate identifier" stx dup)))
         (with-syntax ([(def ...) defs])
           (syntax/loc stx
             (let () def ... (let () body1 body ...))))))]
    [(_ x body1 body ...)
     (raise-syntax-error #f "not a definition sequence" stx (syntax x))]))
