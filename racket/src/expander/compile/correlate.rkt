#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/datum-map.rkt"
         "../host/correlate.rkt"
         (only-in "../host/syntax-to-reader-syntax.rkt" srcloc->vector))

;; The `correlate*` function takes the source location of an expander
;; syntax object and applies it to a host-system syntax object (i.e.,
;; a "correlated")

(provide correlate*
         correlate~
         correlate/app
         ->correlated)

(define (correlate* stx s-exp)
  (if (syntax-srcloc stx)
      (datum->correlated s-exp (srcloc->vector (syntax-srcloc stx)))
      s-exp))

;; For terms where we know the compiler currently doesn't
;; pay attention to source locations, so there's no reason
;; to keep them:
(define (correlate~ stx s-exp)
  s-exp)

(define (correlate/app stx s-exp)
  (if (eq? (system-type 'vm) 'chez-scheme)
      (correlate* stx s-exp)
      (correlate~ stx s-exp)))

(define (->correlated s)
  (datum->correlated s #f))
