#lang racket/base
(require "config.rkt"
         "special.rkt"
         "readtable.rkt"
         "accum-string.rkt"
         "parameter.rkt"
         "error.rkt"
         "digit.rkt"
         "vector.rkt")

(provide read-vector-or-graph
         get-graph-hash)

(define (read-vector-or-graph read-one dispatch-c init-c in config)
  (define accum-str (accum-string-init! config))
  (accum-string-add! accum-str init-c)
  
  (define init-v (digit->number init-c))
  
  (define v (read-digits in config accum-str
                         #:base 10 #:max-count +inf.0
                         #:init init-v
                         #:zero-digits-result init-v))
  (define-values (post-line post-col post-pos) (port-next-location in))
  
  (define (get-accum c)
    (format "~a~a~a" dispatch-c (accum-string-get! accum-str config) c))
  (define-syntax-rule (guard-legal e c body ...)
    (cond
     [e body ...]
     [else (bad-syntax-error in config (get-accum c))]))
  
  (define c (read-char/special in config))
  (define ec (effective-char c config))
  (case ec
    [(#\()
     (accum-string-abandon! accum-str config)
     (read-vector read-one c #\( #\) in config #:length v)]
    [(#\[)
     (accum-string-abandon! accum-str config)
     (guard-legal
      (check-parameter read-square-bracket-as-paren config)
      (get-accum c)
      (read-vector read-one c #\[ #\] in config #:length v))]
    [(#\{)
     (accum-string-abandon! accum-str config)
     (guard-legal
      (check-parameter read-curly-brace-as-paren config)
      (get-accum c)
      (read-vector read-one c #\{ #\} in config #:length v))]
    [else
     (case c
       [(#\= #\#)
        (when (or (read-config-for-syntax? config)
                  (not (check-parameter read-accept-graph config)))
          (reader-error in config
                        "`#...~a` forms not ~a"
                        c
                        (if (read-config-for-syntax? config)
                            "allowed in `read-syntax` mode"
                            "enabled")))
        (unless ((accum-string-count accum-str) . <= . 8)
          (reader-error in config
                        "graph ID too long in `~a~a~a`"
                        dispatch-c (accum-string-get! accum-str config) c))
        (case c
          [(#\=)
           (define ph (make-placeholder 'placeholder))
           (define ht (get-graph-hash config))
           (when (hash-ref ht v #f)
             (reader-error in config
                           "multiple `~a~a~a` tags"
                           dispatch-c (accum-string-get! accum-str config) c))
           (hash-set! ht v ph)
           (define result-v (read-one #f in (next-readtable config)))
           (when (eof-object? result-v)
             (reader-error in config #:due-to result-v
                           "expected an element for graph after `~a~a~a`, found end-of-file"
                           dispatch-c (accum-string-get! accum-str config) c))
           (accum-string-abandon! accum-str config)
           (placeholder-set! ph result-v)
           ph]
          [(#\#)
           (begin0
            (hash-ref 
             (or (read-config-state-graph (read-config-st config))
                 #hash())
             v
             (lambda ()
               (reader-error in config
                             "no preceding `~a~a=` for `~a~a~a`"
                             dispatch-c v
                             dispatch-c (accum-string-get! accum-str config) c)))
            (accum-string-abandon! accum-str config))])]
       [else
        (reader-error in config
                      #:due-to c
                      "bad syntax `~a`"
                      (get-accum c))])]))

;; ----------------------------------------

(define (get-graph-hash config)
  (define st (read-config-st config))
  (or (read-config-state-graph st)
      (let ([ht (make-hasheqv)])
        (set-read-config-state-graph! st ht)
        ht)))
