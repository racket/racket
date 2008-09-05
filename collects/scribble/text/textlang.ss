#lang scheme/base

(require (for-syntax scheme/base syntax/kerncase)
         scheme/promise "../text.ss")

(provide (except-out (all-from-out scheme/base) #%module-begin)
         (rename-out [module-begin #%module-begin])
         (all-from-out scheme/promise "../text.ss"))

(begin-for-syntax
  (define definition-ids ; ids that don't require forcing
    (syntax->list #'(define-values define-syntaxes define-values-for-syntax
                     require provide #%require #%provide)))
  (define stoplist (append definition-ids (kernel-form-identifier-list)))
  (define (definition-id? id)
    (and (identifier? id)
         (ormap (lambda (i) (free-identifier=? id i)) definition-ids)))
  (define (newline-stx? stx)
    (let ([p (syntax-property stx 'scribble)])
      (and (pair? p) (eq? (car p) 'newline))))
  (define swallow-newlines? #t))

;; use `swallow-newlines?' for state, to get rid of newlines that follow
;; definition expressions (must do that, since we need to expand expressions
;; one-by-one, so #%module-begin will do its job) -- this relies on
;; left-to-right macro expansion.

(define-syntax (toplevel-decorate stx)
  (let ([context (syntax-local-context)])
    (syntax-case stx ()
      [(this expr)
       (let ([expr* (local-expand #'expr context stoplist)])
         (syntax-case expr* (begin)
           [(begin x ...) #'(begin (this x) ...)]
           [(id . rest) (definition-id? #'id)
            (begin (set! swallow-newlines? #t) expr*)]
           [_ (if (and swallow-newlines? (newline-stx? expr*))
                #'(begin)
                (begin (set! swallow-newlines? #f) #`(output #,expr*)))]))])))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ expr ...)
     (begin (set! swallow-newlines? #t) ; not really necessary
            #'(#%module-begin (toplevel-decorate expr) ...))]))
