#lang racket/base

(provide out
         out-exact
         out-open
         out-close
         out-close+open
         out-close!
         out-margin
         out-next
         current-c-output-port)

(define current-c-output-port (make-parameter (current-output-port)))

(define indent "")

(define (out fmt . args)
  (display indent (current-c-output-port))
  (apply out-margin fmt args))

(define (out-exact s)
  (displayln s (current-c-output-port)))

(define (out-margin fmt . args)
  (define o (current-c-output-port))
  (apply fprintf o fmt args)
  (newline o))

(define (out-open fmt . args)
  (apply out fmt args)
  (set! indent (string-append indent "  ")))

(define (out-close fmt . args)
  (out-close!)
  (apply out fmt args))

(define (out-close+open fmt . args)
  (out-close!)
  (apply out fmt args)
  (set! indent (string-append indent "  ")))

(define (out-close!)
  (set! indent (substring indent 2)))

(define (out-next)
  (newline (current-c-output-port)))
