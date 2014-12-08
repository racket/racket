#lang racket/base

(provide configure)

(require (only-in scribble/reader make-at-readtable))

(define (configure data)
  (define old-read (current-read-interaction))
  (define (new-read src in)
    (parameterize ([current-readtable (make-at-readtable #:readtable (current-readtable))])
      (old-read src in)))
  (current-read-interaction new-read))
