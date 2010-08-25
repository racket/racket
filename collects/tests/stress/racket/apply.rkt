#lang racket/base
(require tests/stress/stress)

(fit "apply to atoms"
     23
     (lambda (n)
       (apply void (build-list (expt 2 n) (λ (x) x)))))