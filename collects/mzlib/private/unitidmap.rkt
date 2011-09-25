#lang racket/base

;; Help Desk binding info:
(define (binding binder bound stx)
  stx
  ;; This 'bound-in-source is no longer needed
  #;
  (syntax-property
   stx
   'bound-in-source
   (cons binder (syntax-local-introduce bound))))

(define (make-id-mapper unbox-stx the-binder)
  (let ([set!-stx (datum->syntax unbox-stx 'set! #f)])
    (make-set!-transformer
     (lambda (sstx)
       (cond
         [(identifier? sstx) 
          (binding the-binder sstx
                   unbox-stx)]
         [(free-identifier=? set!-stx (car (syntax-e sstx)))
          (raise-syntax-error
           'unit
           "cannot set! imported or exported variables"
           sstx)]
         [else
          (binding
           the-binder (car (syntax-e sstx))
           (datum->syntax
            sstx
            (cons unbox-stx (cdr (syntax-e sstx)))
            sstx))])))))

(provide make-id-mapper)

