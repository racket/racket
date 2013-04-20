#lang racket/base
(require (only-in scribble/reader make-at-readtable))

(provide (rename-out [diamond-read read]
                     [diamond-read-syntax read-syntax]))

(define diamond-readtable (make-at-readtable #:command-char #\◇))

(define (diamond-read p)
  (parameterize ([current-readtable diamond-readtable])
    (read p)))

(define (diamond-read-syntax name p)
  (parameterize ([current-readtable diamond-readtable])
    (read-syntax name p)))
