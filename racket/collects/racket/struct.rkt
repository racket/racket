#lang racket/base
(require "private/custom-write.rkt"
         racket/contract/base)
(provide (contract-out
          [make-constructor-style-printer
           (-> (-> any/c (or/c symbol? string?))
               (-> any/c sequence?)
               (-> any/c output-port? (or/c #t #f 0 1) void?))])
         struct->list)

(define dummy-value (box 'dummy))

;; struct->list : struct?
;;                #:on-opaque (or/c 'error 'return-false 'skip)
;;             -> (listof any/c)
(define (struct->list s
                      #:on-opaque [on-opaque 'error])
  (define error-on-opaque? (eq? on-opaque 'error))
  (let ([vec (struct->vector s dummy-value)])
    ;; go through vector backwards, don't traverse 0 (struct name)
    (let loop ([index (sub1 (vector-length vec))]
               [elems null]
               [any-opaque? #f])
      (cond [(positive? index)
             (let ([elem (vector-ref vec index)])
               (cond [(eq? elem dummy-value)
                      (when error-on-opaque?
                        (raise-type-error 'struct->list "non-opaque struct" s))
                      (loop (sub1 index) elems #t)]
                     [else (loop (sub1 index) (cons elem elems) any-opaque?)]))]
            [else
             (cond [(and any-opaque? (eq? on-opaque 'return-false))
                    #f]
                   [else elems])]))))
