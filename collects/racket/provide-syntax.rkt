#lang racket/base

(provide define-provide-syntax)

(require (for-syntax racket/base
                     "provide-transform.rkt"))

(define-for-syntax orig-insp (variable-reference->module-declaration-inspector
                              (#%variable-reference)))

(define-for-syntax (make-provide-macro proc)
  (make-provide-transformer
   (lambda (stx modes)
     (let* ([i (make-syntax-introducer)]
            [d-stx (syntax-disarm stx orig-insp)]
            [new-stx (i (proc (i d-stx)))])
       (expand-export (syntax-rearm new-stx stx) modes)))))

(define-syntax (define-provide-syntax stx)
  (syntax-case stx ()
    [(_ id proc)
     (identifier? #'id)
     (syntax/loc stx
       (define-syntax id
         (make-provide-macro proc)))]
    [(_ (id . args) . body)
     (identifier? #'id)
     (syntax/loc stx
       (define-provide-syntax id
         (lambda args . body)))]))
