#lang racket/base

(provide define-require-syntax)

(require (for-syntax racket/base "require-transform.rkt"))

(define-for-syntax orig-insp (variable-reference->module-declaration-inspector
                              (#%variable-reference)))

(define-for-syntax (make-require-macro proc)
  (make-require-transformer
   (lambda (stx)
     (let* ([i (make-syntax-introducer)]
            [d-stx (syntax-disarm stx orig-insp)]
            [new-stx (i (proc (i d-stx)))])
       (expand-import (syntax-rearm new-stx stx))))))

(define-syntax (define-require-syntax stx)
  (syntax-case stx ()
    [(_ id proc)
     (identifier? #'id)
     (syntax/loc stx
       (define-syntax id
         (make-require-macro proc)))]
    [(_ (id . args) . body)
     (identifier? #'id)
     (syntax/loc stx
       (define-require-syntax id
         (lambda args . body)))]))
