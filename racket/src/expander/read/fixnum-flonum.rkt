#lang racket/base
(require "config.rkt"
         "error.rkt"
         "whitespace.rkt"
         "location.rkt"
         "special.rkt"
         "symbol-or-number.rkt"
         "readtable.rkt"
         "special-comment.rkt")

(provide read-fixnum
         read-flonum)

(define (read-number-literal read-one init-c in config
                             mode starts-num? num? num-str)
  (define c (read-char/skip-whitespace-and-comments init-c read-one in config))
  (define-values (line col pos) (port-next-location* in c))
  (define ec (readtable-effective-char (read-config-readtable config) c))
  (cond
    [(or (not (char? c))
         (starts-num? ec))
     (define v (if (char? c)
                   (read-symbol-or-number c in config #:mode mode)
                   c))
     (cond
       [(num? v) v]
       [(eof-object? v) v]
       [(and (special-comment? v) (read-config-keep-comment? v)) v]
       [else
        (reader-error in (reading-at config line col pos)
                      "expected a ~a, found ~v"
                      num-str v)])]
    [(special-comment-via-readtable? c read-one in config)
     => (lambda (v)
          (if (read-config-keep-comment? config)
              v
              (read-number-literal read-one #f in config
                                   mode starts-num? num? num-str)))]
    [else (reader-error in (reading-at config line col pos)
                        "expected a ~a"
                        num-str)]))

(define (starts-fixnum? c)
  (or (char<=? #\0 c #\9)
      (char=? c #\-)
      (char=? c #\+)
      (char=? c #\#)))

(define (read-fixnum read-one init-c in config)
  (read-number-literal read-one init-c in config
                       "#e" starts-fixnum? fixnum? "fixnum"))

(define (starts-flonum? c)
  (or (starts-fixnum? c)
      (char=? c #\.)))

(define (read-flonum read-one init-c in config)
  (read-number-literal read-one init-c in config
                       "#i" starts-flonum? flonum? "flonum"))
