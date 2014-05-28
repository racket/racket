#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "guarded mark reduction doesn't wrap results with a list/c")

;; note: this bug was found and fixed during the development of this model
;; with commit: 4b848777d12a2e5b59b43c8e77f9f68b747d1151

(define-rewrite bug1
  (monitor (list/c ctc) (ccm mk t) k l j)
  ==> 
  (monitor ctc (ccm mk t) k l j)
  #:context (reduction-relation)
  #:once-only)

(include/rewrite (lib "redex/benchmark/models/delim-cont/delim-cont.rkt") delim-cont bug1)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example 
  '(ccm 
    (monitor 
     (mark/c 
      (mark/c (flat (λ (p : Num) #t)) 
              Num)
      (Mark Num)) 
     (make-cm-key (Mark Num))
     "" "" "") 
    (Mark Num)))

(test small-counter-example)
