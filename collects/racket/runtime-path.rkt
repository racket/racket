#lang racket/base
(require (for-syntax racket/base))

(require mzlib/runtime-path)
(provide (all-from-out mzlib/runtime-path)
         (for-syntax #%datum)
         define-runtime-module-path)

(define-syntax (define-runtime-module-path stx)
  (syntax-case stx ()
    [(_ id mod-path)
     (begin
       (unless (memq (syntax-local-context) '(top-level module module-begin))
         (raise-syntax-error #f
                             "allowed only in a module top-level or top-level context"
                             stx))
       (unless (identifier? #'id)
         (raise-syntax-error #f
                             "expected an identifier to bind"
                             stx
                             #'id))
       (unless (module-path? (syntax->datum #'mod-path))
         (raise-syntax-error #f
                             "expected a literal module path"
                             stx
                             #'mod-path))
       #`(begin
           (require (only-in (for-label mod-path)))
           (define id (combine-module-path (#%variable-reference) 'mod-path))))]))

(define (combine-module-path vr mod-path)
  (module-path-index-resolve (module-path-index-join
                              mod-path
                              (variable-reference->resolved-module-path vr))))
