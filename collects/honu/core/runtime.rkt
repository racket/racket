#lang racket/base

(require "read.rkt"
         "private/honu-typed-scheme.rkt"
         racket/port)

;; at the repl, honu will only read a single line at a time regardless
;; of how many expressions it contains
(define (read-one-line name input)
  (define quit? #f)
  (define one-line
    (with-output-to-string
      (lambda ()
        (let loop ()
          (define next (read-char input))
          (when (eof-object? next)
            (set! quit? #t))
          (when (not (or (eof-object? next)
                         (char=? next #\newline)))
            (display next)
            (loop))))))
  (if quit?
    eof
    (honu-read-syntax name (open-input-string one-line))))

(provide configure)
(define (configure . args)
  (current-read-interaction read-one-line))
