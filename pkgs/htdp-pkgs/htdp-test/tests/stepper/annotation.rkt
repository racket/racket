#lang racket/base

(require stepper/private/annotate
         "test-engine.rkt"
         "language-level-model.rkt")

(define (try-annotating str)
  (define expanded 
    (car (string->expanded-syntax-list intermediate str)))
  (printf "expanded: ~s\n" expanded)
  (annotate expanded (lambda (a b c) 'bogus) #f))

(try-annotating "(check-expect 2 2)")