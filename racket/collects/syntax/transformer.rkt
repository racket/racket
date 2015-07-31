#lang racket/base

(require (for-template racket/base))

(provide make-variable-like-transformer)

(define (make-variable-like-transformer ref-stx [set!-handler #f])
  (unless (syntax? ref-stx)
    (raise-type-error 'make-variable-like-transformer "syntax?" ref-stx))
  (unless (or (syntax? set!-handler) (procedure? set!-handler) (eq? set!-handler #f))
    (raise-type-error 'make-variable-like-transformer "(or/c syntax? procedure? #f)" set!-handler))
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [id
        (identifier? #'id)
        ref-stx]
       [(set! id val)
        (cond [(procedure? set!-handler)
               (set!-handler stx)]
              [(syntax? set!-handler)
               (with-syntax ([setter set!-handler])
                 (syntax/loc stx (setter val)))]
              [else
               (raise-syntax-error #f "cannot mutate identifier" stx #'id)])]
       [(id . args)
        (let ([stx* (cons #'(#%expression id) (cdr (syntax-e stx)))])
          (datum->syntax stx stx* stx))]))))
