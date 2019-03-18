#lang racket/base

(require (for-template racket/base))

(provide make-variable-like-transformer
         make-constant-like-transformer)

(define (make-variable-like-transformer ref-stx [set!-handler #f])
  (unless (or (syntax? ref-stx) (procedure? ref-stx))
    (raise-type-error 'make-variable-like-transformer "(or/c syntax? procedure?)" ref-stx))
  (unless (or (syntax? set!-handler) (procedure? set!-handler) (eq? set!-handler #f))
    (raise-type-error 'make-variable-like-transformer "(or/c syntax? procedure? #f)" set!-handler))
  (define ref-f
    (make-constant-like-transformer ref-stx))
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! id val)
        (cond [(procedure? set!-handler)
               (set!-handler stx)]
              [(syntax? set!-handler)
               (with-syntax ([setter set!-handler])
                 (syntax/loc stx (setter val)))]
              [else
               (raise-syntax-error #f "cannot mutate identifier" stx #'id)])]
       [_
        (ref-f stx)]))))

(define (make-constant-like-transformer ref-stx)
  (unless (or (syntax? ref-stx) (procedure? ref-stx))
    (raise-type-error 'make-constant-like-transformer "(or/c syntax? procedure?)" ref-stx))
  (lambda (stx)
    (syntax-case stx ()
      [id
       (identifier? #'id)
       (cond [(syntax? ref-stx) ref-stx]
             [else              (ref-stx stx)])]
      [(id . args)
       (let ([stx* (cons #'(#%expression id) (cdr (syntax-e stx)))])
         (datum->syntax stx stx* stx))])))

