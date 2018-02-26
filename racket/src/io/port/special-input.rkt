#lang racket/base
(require "../common/check.rkt"
         "input-port.rkt"
         "parameter.rkt"
         "read-and-peek.rkt"
         "string-input.rkt"
         "progress-evt.rkt"
         "count.rkt")

(provide read-byte-or-special
         peek-byte-or-special
         read-char-or-special
         peek-char-or-special)

(define/who (read-byte-or-special [orig-in (current-input-port)]
                                  [special-wrap #f]
                                  [source-name #f])
  (check who input-port? orig-in)
  (check who #:or-false (procedure-arity-includes/c 1) special-wrap)
  (let ([in (->core-input-port orig-in)])
    (define read-byte (core-input-port-read-byte in))
    (cond
      [read-byte (do-read-byte who read-byte in)]
      [else
       (extract-special-value (read-byte-via-bytes in)
                              in source-name -1
                              special-wrap)])))


(define/who (peek-byte-or-special [orig-in (current-input-port)]
                                  [skip-k 0]
                                  [progress-evt #f]
                                  [special-wrap #f]
                                  [source-name #f])
  (check who input-port? orig-in)
  (check who exact-nonnegative-integer? skip-k)
  (check who #:or-false evt? progress-evt)
  (check who #:or-false (procedure-arity-includes/c 1) special-wrap)
  (when progress-evt
    (check-progress-evt who progress-evt orig-in))
  (let ([in (->core-input-port orig-in)])
    (define peek-byte (core-input-port-read-byte in))
    (cond
      [peek-byte (do-peek-byte who peek-byte in orig-in)]
      [else
       (extract-special-value (peek-byte-via-bytes in skip-k #:progress-evt progress-evt)
                              in source-name skip-k
                              special-wrap)])))

;; ----------------------------------------

(define/who (read-char-or-special [in (current-input-port)]
                                  [special-wrap #f]
                                  [source-name #f])
  (check who input-port? in)
  (check who #:or-false (procedure-arity-includes/c 1) special-wrap)
  (extract-special-value (do-read-char who in #:special-ok? #t)
                         in source-name -1
                         special-wrap))

(define/who (peek-char-or-special [in (current-input-port)]
                                  [skip-k 0]
                                  [special-wrap #f]
                                  [source-name #f])
  (check who input-port? in)
  (check who exact-nonnegative-integer? skip-k)
  (check who #:or-false (procedure-arity-includes/c 1) special-wrap)
  (extract-special-value (do-peek-char who in skip-k #:special-ok? #t)
                         in source-name skip-k
                         special-wrap))

;; ----------------------------------------

(define (extract-special-value v in source-name delta special-wrap)
  (cond
    [(procedure? v)
     (define special
       (cond
         [(not source-name)
          (cond
            [(procedure-arity-includes? v 0)
             (v)]
            [else
             (v #f #f #f #f)])]
         [else
          (define-values (line col pos) (port-next-location in))
          (v source-name
             line
             (and col (+ col delta))
             (and pos (+ pos delta)))]))
     (if special-wrap
         (special-wrap special)
         special)]
    [else v]))
