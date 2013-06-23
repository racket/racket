#lang typed/racket/base

;; Test for PR 13124

(: foo (-> Flonum))
(define (foo)
  (: bn (Flonum -> Flonum))
  (define (bn n)
    (cond [(= n 0.0)
           1.0]
          [else (let loop ([s  0.0] [i  0.0])
                  (cond [(i . < . n)
                         (loop (+ s (bn i))
                               (+ i 1.0))]
                        [else  s]))]))
  ;; we want this `v` to type-check without extra annotation
  (define v 0.0)
  v)

;; simpler version
(: foo2 (-> Integer))
(define (foo2)
  (: bn (Integer -> Integer))
  (define (bn n)
    (if (= n 0)
        1 (bn (- n 1))))
  (define v 1)
  0)
