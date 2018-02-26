#lang racket/base

(provide union! find!)

(define (find! reps key)
  (define rep-key (hash-ref reps key key))
  (if (equal? rep-key key)
      key
      (let ([rep-key (find! reps rep-key)])
        (hash-set! reps key rep-key)
        rep-key)))

(define (union! reps a-key b-key)
  (define rep-a-key (find! reps a-key))
  (define rep-b-key (find! reps b-key))
  (unless (equal? rep-a-key rep-b-key)
    (hash-set! reps rep-b-key rep-a-key))
  rep-a-key)
