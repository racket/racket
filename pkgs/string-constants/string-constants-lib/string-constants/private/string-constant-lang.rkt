#lang racket/base
(require (for-syntax racket/base))

(provide (rename-out [-#%module-begin #%module-begin])
         #%datum
         #%top-interaction)

(define-syntax (-#%module-begin stx)
  (syntax-case stx ()
    [(_ (name strs ...) ...)
     (and (andmap identifier? (syntax->list (syntax (name ...))))
          (andmap (λ (x) (not (null? (syntax-e x)))) (syntax->list #'((strs ...) ...)))
          (andmap (λ (x) (string? (syntax-e x))) (syntax->list (syntax (strs ... ...)))))
     (let ([expln
            (string-append
             " (multi-line string constants must be broken on spaces"
             " and the space must start at the beginning of the"
             " (non-first) string constant")])
       (for ([strs-stx (in-list (syntax->list #'((strs ...) ...)))])
         (define strs (syntax->list strs-stx))
         (for ([this-str (in-list strs)]
               [next-str (in-list (cdr strs))])
           (unless (regexp-match #rx"^ " (syntax-e next-str))
             (raise-syntax-error 'string-constant-lang 
                                 (string-append
                                  "expected a string that begins with a space"
                                  expln)
                                 stx
                                 next-str))
           (when (regexp-match #rx" $" (syntax-e this-str))
             (raise-syntax-error 'string-constant-lang 
                                 (string-append
                                  "expected a string that does not end with a space"
                                  expln)
                                 stx
                                 this-str))))
       (with-syntax ([string-constants (datum->syntax stx 'string-constants)])
         (syntax
          (#%plain-module-begin
           (provide string-constants)
           (define string-constants
             (make-hash (list (cons 'name (string-append strs ...)) ...)))))))]
    [(_ prs ...)
     (for ([pr-stx (in-list (syntax->list (syntax (prs ...))))])
       (let ([pr (syntax->datum pr-stx)])
         (unless (and (list? pr) 
                      (<= 2 (length pr))
                      (symbol? (car pr))
                      (andmap string? (cdr pr)))
           (raise-syntax-error 'string-constant-lang "bad string constant" stx pr-stx))))]))
