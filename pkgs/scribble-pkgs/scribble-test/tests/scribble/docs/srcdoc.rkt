#lang racket
(require scribble/srcdoc
         (for-doc racket/base
                  scribble/manual))

(provide
 (proc-doc f (-> integer?) ["Stuff"])
 (form-doc #:id a #:literals (foo) (expr foo a) ["Returns " (racket expr) "."]))

(define (f) 5)

(define-syntax-rule (a x) x)
