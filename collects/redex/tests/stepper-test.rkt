#lang racket

(require framework
         "test-util.rkt"
         "../reduction-semantics.rkt"
         "../private/stepper.rkt"
         "../private/size-snip.rkt")

(reset-count)

;; diff : term term -> (cons range range)
;; range = (listof (cons nat nat))
(define (diff from to)
  (define (make-node t)
    (new node%
         [pp default-pretty-printer]
         [all-nodes-ht 'dont-care]
         [term t]
         [red 'dont-care]
         [change-path 'dont-care]
         [init-cw (initial-char-width)]))
  (define (ranges node)
    (map (λ (range) (cons (text:range-start range)
                          (text:range-end range)))
         (send (send (send node get-big-snip) get-editor)
               get-highlighted-ranges)))
  (define from-node (make-node from))
  (define to-node (make-node to))
  (show-diff from-node to-node)
  (cons (ranges from-node) (ranges to-node)))

(test (diff (term (hole a)) (term (hole b)))
      (cons (list (cons 6 7)) (list (cons 6 7))))
(test (diff (term (,'hole a)) (term (,'hole b)))
      (cons (list (cons 8 9)) (list (cons 8 9))))

(print-tests-passed 'stepper-test.ss)