#lang typed/racket

(: f (Pair 'bad 'worse))
(define f
  (letrec ((y y)) y))
