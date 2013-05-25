#lang racket/base
(require racket/contract/base
         "model/trace.rkt"
         "model/reductions-config.rkt"
         "model/reductions.rkt")

(provide/contract
 [expand-only 
  (any/c (listof identifier?) . -> . syntax?)]
 [expand/hide
  (any/c (listof identifier?) . -> . syntax?)]
 [expand/show-predicate
  (any/c (-> identifier? any/c) . -> . syntax?)])

(define (->predicate ids)
  (lambda (id)
    (for/or ([x ids]) (free-identifier=? id x))))

(define (expand-only stx to-show)
  (expand/show-predicate stx (->predicate to-show)))

(define (expand/hide stx to-hide)
  (expand/show-predicate stx (compose not (->predicate to-hide))))

(define (expand/show-predicate stx show?)
  (let-values ([(result deriv) (trace/result stx)])
    (when (exn? result) (raise result))
    (let-values ([(_steps _defs _uses stx exn2)
                  (parameterize ((macro-policy show?))
                    (reductions+ deriv))])
      (when (exn? exn2) (raise exn2))
      stx)))
