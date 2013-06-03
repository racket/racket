#lang racket/base

(provide (all-defined-out))

(struct contract-profile
  (total-time n-samples n-contract-samples
   ;; (pairof blame? profile-sample)
   ;; samples taken while a contract was running
   live-contract-samples
   ;; (listof blame?)
   ;; all the blames that were observed during sampling
   all-blames
   ;; profile?
   ;; regular time profile
   regular-profile))

(define (samples-time samples)
  (for/sum ([s (in-list samples)])
    (cadr s)))

(define output-file-prefix "tmp-contract-profile-")
