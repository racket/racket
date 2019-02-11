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
    (define v (read-a-byte who in #:special-ok? #t))
    (if (fixnum? v)
        v
        (extract-special-value v
                               in source-name -1
                               special-wrap))))


(define/who (peek-byte-or-special [orig-in (current-input-port)]
                                  [skip-k 0]
                                  [progress-evt #f]
                                  [special-wrap #f]
                                  [source-name #f])
  (check who input-port? orig-in)
  (check who exact-nonnegative-integer? skip-k)
  (check who #:or-false evt? progress-evt)
  (check who special-wrap-for-peek? #:contract special-wrap-for-peek/c-str special-wrap)
  (when progress-evt
    (check-progress-evt who progress-evt orig-in))
  (let ([in (->core-input-port orig-in)])
    (cond
      [(not progress-evt)
       (define v (peek-a-byte who in skip-k #:special-ok? #t))
       (if (fixnum? v)
           v
           (extract-special-value v
                                  in source-name skip-k
                                  special-wrap))]
      [else
       (extract-special-value (peek-byte-via-bytes in skip-k #:progress-evt progress-evt)
                              in source-name skip-k
                              special-wrap)])))

;; ----------------------------------------

(define/who (read-char-or-special [in (current-input-port)]
                                  [special-wrap #f]
                                  [source-name #f])
  (let ([in (->core-input-port in who)])
    (check who #:or-false (procedure-arity-includes/c 1) special-wrap)
    (extract-special-value (read-a-char who in #:special-ok? #t)
                           in source-name -1
                           special-wrap)))

(define/who (peek-char-or-special [in (current-input-port)]
                                  [skip-k 0]
                                  [special-wrap #f]
                                  [source-name #f])
  (check who input-port? in)
  (check who exact-nonnegative-integer? skip-k)
  (check who special-wrap-for-peek? #:contract special-wrap-for-peek/c-str special-wrap)
  (extract-special-value (peek-a-char who in skip-k #:special-ok? #t)
                         in source-name skip-k
                         special-wrap))

;; ----------------------------------------

(define (extract-special-value v in source-name delta special-wrap)
  (cond
    [(procedure? v)
     (cond
       [(eq? special-wrap 'special)
        'special]
       [else
        (define special
          (cond
            [#f
             ;; There doesn't seem to be a case anymore
             ;; where the old Racket implementation uses
             ;; the 0-arity and/or no-position protocol
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
            special)])]
    [else v]))

(define (special-wrap-for-peek? w)
  (or (not w) (eq? w 'special) (and (procedure? w)
                                    (procedure-arity-includes? w 1))))

(define special-wrap-for-peek/c-str
  "(or/c (any/c -> any/c) #f 'special)")
                
