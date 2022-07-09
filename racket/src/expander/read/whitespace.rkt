#lang racket/base
(require "../common/struct-star.rkt"
         "config.rkt"
         "special.rkt"
         "readtable.rkt"
         "consume.rkt"
         "error.rkt"
         "location.rkt"
         "special.rkt"
         "special-comment.rkt")

(provide read-char/skip-whitespace-and-comments
         special-comment-via-readtable?)

;; Skip most whitespace, including non-character values that are
;; `special-comment?`s --- but return a special comment (always
;; `special`-wrapped) if `(read-config-keep-comment? config)`. The
;; result is a character that has been consumed.
;; Readtable-based special comments are not skipped; those must be
;; handled directly, possibly via `special-comment-via-readtable?`.
(define (read-char/skip-whitespace-and-comments init-c read-one in config)
  (define rt (read-config-readtable config))
  (define source (read-config-source config))
  (let skip-loop ([init-c init-c])
    (define c (or init-c
                  (read-char/special in config source)))
    (define ec (readtable-effective-char rt c))
    (cond
     [(eof-object? ec) c]
     [(not (char? ec))
      (define v (special-value c))
      (cond
       [(and (special-comment? v)
             (not (read-config-keep-comment? config)))
        (skip-loop #f)]
       [else c])]
     [(or (char-whitespace? ec)
          ;; treat BOM as whitespace in the same sense as a comment:
          (eqv? #\uFEFF ec))
      (skip-loop #f)]
     [(char=? #\; ec)
      (let loop ()
        (define c (read-char/special in config source))
        (unless (or (eof-object? c)
                    (eqv? #\newline (effective-char c config)))
          (loop)))
      (if (read-config-keep-comment? config)
          (result-special-comment)
          (skip-loop #f))]
     [(and (char=? #\# ec)
           (eqv? #\| (readtable-effective-char/# rt (peek-char/special in config 0 source))))
      (skip-pipe-comment! c in config)
      (if (read-config-keep-comment? config)
          (result-special-comment)
          (skip-loop #f))]
     [(and (char=? #\# ec)
           (eqv? #\! (readtable-effective-char/# rt (peek-char/special in config 0 source)))
           (let ([c3 (peek-char/special in config 1 source)])
             (or (eqv? #\space c3)
                 (eqv? #\/ c3))))
      (skip-unix-line-comment! in config)
      (if (read-config-keep-comment? config)
          (result-special-comment)
          (skip-loop #f))]
     [(and (char=? #\# ec)
           (eqv? #\; (readtable-effective-char/# rt (peek-char/special in config 0 source))))
      (consume-char in #\;)
      (define v (read-one #f in config))
      (when (eof-object? v)
        (reader-error in config
                      #:due-to v
                      "expected a commented-out element for `~a;`, but found end-of-file"
                      ec))
      (if (read-config-keep-comment? config)
          (result-special-comment)
          (skip-loop #f))]
     [else c])))

;; For returning a comment as a result:
(define (result-special-comment)
  (special (make-special-comment #f)))

;; Skips balanced pipe comments
(define (skip-pipe-comment! init-c in config)
  (define source (read-config-source config))
  (define-values (line col pos) (port-next-location in))
  (consume-char in #\|)
  (let loop ([prev-c #f] [depth 0])
    (define c (read-char/special in config source))
    (cond
     [(eof-object? c)
      (reader-error in (reading-at config line col pos)
                    #:due-to c
                    "end of file in `#|` comment")]
     [(not (char? c))
      (loop #f depth)]
     [(and (char=? #\| c) (eqv? prev-c #\#))
      (loop #f (add1 depth))]
     [(and (char=? #\# c) (eqv? prev-c #\|))
      (when (positive? depth)
        (loop #f (sub1 depth)))]
     [else (loop c depth)])))

;; Skips a comment that starts #! and runs to the end of the line, but
;; can be continued with `\` at the end of the line
(define (skip-unix-line-comment! in config)
  (let loop ([backslash? #f])
    (define c (read-char/special in config))
    (cond
     [(eof-object? c) (void)]
     [(not (char? c)) (loop #f)]
     [(char=? c #\newline)
      (when backslash?
        (loop #f))]
     [(char=? c #\\)
      (loop #t)]
     [else (loop #f)])))

(define (special-comment-via-readtable? c read-one in config)
  ;; If we have a readtable, we may need to read ahead to make sure
  ;; that `c` doesn't start a comment. Always reading would be more
  ;; consistent in some ways, it works better in other ways to limit
  ;; reading and only read if a readtable callback that could produce
  ;; a comment.
  (define v
    (cond
      [(and (char? c)
            (let ([ec (readtable-effective-char (read-config-readtable config) c #f)])
              (or (not ec)
                  (and (char=? ec #\#)
                       (let ([c2 (peek-char in)])
                         (and (char? c2)
                              (not (readtable-effective-char/# (read-config-readtable config) c2))))))))
       (read-one c in (keep-comment config))]
      [else c]))
  (and (special-comment? v) v))
