#lang racket/base

(require "read.rkt"
         "private/honu-typed-scheme.rkt"
         racket/port)

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
    ;; this isn't right, somehow communicate to the system that the repl should close
    #'(exit)
    (honu-read-syntax name (open-input-string one-line))))

(provide configure)
(define (configure . args)
  (current-read-interaction read-one-line))
