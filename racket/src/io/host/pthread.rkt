#lang racket/base
(require racket/private/primitive-table
         "../common/internal-error.rkt"
         (only-in '#%linklet primitive-table)
         (for-syntax racket/base))

(void (unless (primitive-table '#%pthread)
        (internal-error "pthreads not provided by host")))

(define-syntax (bounce stx)
  (syntax-case stx ()
    [(_ table bind ...)
     (with-syntax ([([orig-id here-id] ...)
                    (for/list ([bind (in-list (syntax->list #'(bind ...)))])
                      (if (identifier? bind)
                          (list bind bind)
                          bind))])
       #'(begin
           (provide here-id ...)
           (import-from-primitive-table table bind ...)))]))

(bounce #%pthread
        unsafe-make-place-local
        unsafe-place-local-ref
        unsafe-place-local-set!
        unsafe-add-global-finalizer
        unsafe-strip-impersonator
        prop:unsafe-authentic-override)
