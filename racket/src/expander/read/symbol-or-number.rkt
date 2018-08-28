#lang racket/base
(require "config.rkt"
         "special.rkt"
         "wrap.rkt"
         "readtable.rkt"
         "delimiter.rkt"
         "consume.rkt"
         "accum-string.rkt"
         "error.rkt"
         "parameter.rkt"
         "number.rkt")

(provide read-symbol-or-number)

(define (read-symbol-or-number init-c in orig-config
                               ;; `mode` can be 'symbol-or-number,
                               ;; 'symbol, 'symbol/indirect, 'keyword,
                               ;; or a number prefix string like "#e";
                               ;; only the 'symbol-or-number and
                               ;; 'symbol modes use a readtable's
                               ;; symbol handler
                               #:mode [mode 'symbol-or-number]
                               #:extra-prefix [extra-prefix #f])
  (define config (if (string? mode)
                     (override-parameter read-cdot orig-config #f)
                     orig-config))
  (define rt (read-config-readtable config))
  (cond
   [(and rt
         (or (eq? mode 'symbol-or-number)
             (eq? mode 'symbol/indirect))
         (readtable-symbol-parser rt))
    => (lambda (handler)
         (readtable-apply handler init-c in
                          config
                          (read-config-line config)
                          (read-config-col config)
                          (read-config-pos config)))]
   [else
    (define accum-str (accum-string-init! config))
    (define quoted-ever? #f)
    (define case-sens? (check-parameter read-case-sensitive config))
    (when extra-prefix
      (accum-string-add! accum-str extra-prefix))
    (define source (read-config-source config))
    
    ;; If we encounter an EOF or special in the wrong place:
    (define (unexpected-quoted c after-c)
      (reader-error in config
                    #:due-to c
                    "~a following `~a` in ~a"
                    (if (eof-object? c) "end-of-file" "non-character")
                    after-c (cond
                             [(eq? mode 'keyword) "keyword"]
                             [(string? mode) "number"]
                             [else "symbol"])))
    
    (let loop ([init-c init-c]
               [pipe-quote-c #f] ; currently quoting?
               [foldcase-from 0]) ; keep track of range to foldcase for case-insens
      (define c (or init-c (peek-char/special in config 0 source)))
      (define ec (readtable-effective-char rt c))
      (cond
       [(and pipe-quote-c
             (not (char? ec)))
        ;; Interrupted while in quoting mode
        (unless init-c (consume-char/special in config c))
        (unexpected-quoted c pipe-quote-c)]
       [(and (not pipe-quote-c)
             (readtable-char-delimiter? rt c config))
        ;; EOF or other delimiter - done!
        (unless case-sens?
          (accum-string-convert! accum-str string-foldcase foldcase-from))]
       [(and pipe-quote-c
             (char=? c pipe-quote-c)) ; note: `pipe-quote-c` determines close, not readtable
        ;; End quoting mode
        (unless init-c (consume-char in c))
        (loop #f #f (accum-string-count accum-str))]
       [(and (char=? ec #\|)
             (check-parameter read-accept-bar-quote config))
        ;; Start quoting mode
        (unless init-c (consume-char in c))
        (set! quoted-ever? #t)
        (unless case-sens?
          (accum-string-convert! accum-str string-foldcase foldcase-from))
        (loop #f c (accum-string-count accum-str))]
       [(and (char=? ec #\\)
             (not pipe-quote-c))
        ;; Single-character quoting 
        (unless init-c (consume-char in c))
        (define next-c (read-char/special in config source))
        (unless (char? next-c)
          (unexpected-quoted next-c c))
        (unless (or pipe-quote-c case-sens?)
          (accum-string-convert! accum-str string-foldcase foldcase-from))
        (accum-string-add! accum-str next-c)
        (set! quoted-ever? #t)
        (loop #f #f (accum-string-count accum-str))]
       [else
        ;; Everything else
        (unless init-c (consume-char in c))
        (accum-string-add! accum-str c)
        (loop #f pipe-quote-c foldcase-from)]))
    
    (define str (accum-string-get! accum-str config))
    
    ;; Disallow "." as a symbol
    (when (and (= 1 (string-length str))
               (not quoted-ever?)
               (char=? #\. (effective-char (string-ref str 0) config)))
      (reader-error in config "illegal use of `.`"))
    
    (define num
      (and (or (eq? mode 'symbol-or-number)
               (string? mode))
           (not quoted-ever?)
           (unchecked-string->number (if (string? mode)
                                         (string-append mode str)
                                         str)
                                     10
                                     'read
                                     (if (check-parameter read-decimal-as-inexact config)
                                         'decimal-as-inexact
                                         'decimal-as-exact))))
    (when (string? num)
      (reader-error in config "~a" num))

    (when (and (not num)
               (string? mode))
      (reader-error in config
                    "bad number: `~a`"
                    (string-append mode str)))
    
    (wrap (or num
              (and (eq? mode 'keyword)
                   (string->keyword str))
              (string->symbol str))
          in
          config
          str)]))
