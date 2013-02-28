#lang racket

(provide LBase L1 L2 LMergeUntagged LMergeTagged)
(require redex/reduction-semantics)

;; ------------------------------------------------------------------------

(define-language LBase
  (e (+ e e) number))

(define-extended-language L1 LBase
  (e ....  (- e e)))

(define-extended-language L2 LBase
  (e ....  (* e e)))

;; Untagged union of two languages that define the same nonterminal
(define-union-language LMergeUntagged L1 L2)

;; Tagged merge of two extended languages that define the same
;; nonterminal
(define-union-language LMergeTagged (f. L1) (d. L2))

;; ------------------------------------------------------------------------

(module+ test 

  (for ([t (list (term 1) (term (* 1 1)) (term (+ 1 1)) (term (- 1 1)))])
       (test-equal (redex-match? LMergeUntagged e t) #t))

  (test-equal (redex-match? LMergeTagged f.e 1) #t)
  (test-equal (redex-match? LMergeTagged d.e 1) #t)

  (test-equal (redex-match? LMergeTagged f.e (term (+ 1 1))) #t)
  (test-equal (redex-match? LMergeTagged f.e (term (- 1 1))) #t)
  (test-equal (redex-match? LMergeTagged f.e (term (* 1 1))) #f)

  (test-equal (redex-match? LMergeTagged d.e (term (+ 1 1))) #t)
  (test-equal (redex-match? LMergeTagged d.e (term (* 1 1))) #t)
  (test-equal (redex-match? LMergeTagged d.e (term (- 1 1))) #f))
