#lang racket/base
(require "../common/struct-star.rkt"
         "config.rkt"
         "primitive-parameter.rkt")

(provide check-parameter
         override-parameter
         force-parameters!
         (all-from-out "primitive-parameter.rkt"))

(define unknown (gensym 'unknown))

;; Speed up parameter checking and protect against changes
;; by caching parameter values
(define (check-parameter param config)
  (define cache (read-config-parameter-cache config))
  (define v (hash-ref (read-config-parameter-override config)
                      param
                      (hash-ref cache param unknown)))
  (cond
   [(eq? v unknown)
    (define v (param))
    (hash-set! cache param v)
    v]
   [else v]))

(define (override-parameter param config v)
  (struct*-copy read-config config
                [parameter-override (hash-set
                                     (read-config-parameter-override config)
                                     param
                                     v)]))

;; Protect against callbacks that can change parameters
;; by caching all parameters at current values:
(define (force-parameters! config)
  (define cache (read-config-parameter-cache config))
  (unless (hash-ref cache 'all-forced #f)
    (hash-set! cache 'all-forced #t)
    (check-parameter read-case-sensitive config)
    (check-parameter read-square-bracket-as-paren config)
    (check-parameter read-curly-brace-as-paren config)
    (check-parameter read-square-bracket-with-tag config)
    (check-parameter read-curly-brace-with-tag config)
    (check-parameter read-cdot config)
    (check-parameter read-accept-graph config)
    (check-parameter read-accept-compiled config)
    (check-parameter read-accept-box config)
    (check-parameter read-accept-bar-quote config)
    (check-parameter read-decimal-as-inexact config)
    (check-parameter read-accept-dot config)
    (check-parameter read-accept-infix-dot config)
    (check-parameter read-accept-quasiquote config)
    (check-parameter read-accept-reader config)
    (check-parameter read-accept-lang config)))
