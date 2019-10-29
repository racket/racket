#lang racket/base

(require "guts.rkt"
         "prop.rkt"
         "blame.rkt"
         "generate.rkt")

(provide property/c)

(struct property/c (accessor val-ctc prop-name)
  #:constructor-name make-property/c
  #:omit-define-syntaxes
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:custom-print-quotable 'never
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc)
     `(property/c ,(contract-name (property/c-accessor ctc))
                  ,(contract-name (property/c-val-ctc ctc))))
   #:first-order
   (λ (ctc)
     (define accessor (property/c-accessor ctc))
     (define val-ctc-first-order (contract-first-order (property/c-val-ctc ctc)))
     (λ (val)
       (val-ctc-first-order (accessor val))))
   #:late-neg-projection
   (λ (ctc)
     (define accessor (property/c-accessor ctc))
     (define val-ctc-proj (contract-late-neg-projection (property/c-val-ctc ctc)))
     (define prop-name (property/c-prop-name ctc))
     (define ctx-str (format "the ~a of" prop-name))
     (λ (orig-blame)
       (define blame (blame-add-context orig-blame ctx-str))
       (define val-ctc-proj/blame (val-ctc-proj blame))
       (λ (val neg-party)
         (val-ctc-proj/blame (accessor val) neg-party)
         val)))
   #:stronger
   (λ (ctc-a ctc-b)
     (and (contract-stronger? (property/c-accessor ctc-a)
                              (property/c-accessor ctc-b))
          (contract-stronger? (property/c-val-ctc ctc-a)
                              (property/c-val-ctc ctc-b))))
   #:equivalent
   (λ (ctc-a ctc-b)
     (and (contract-equivalent? (property/c-accessor ctc-a)
                                (property/c-accessor ctc-b))
          (contract-equivalent? (property/c-val-ctc ctc-a)
                                (property/c-val-ctc ctc-b))))
   #:generate
   (λ (ctc)
     ; It’s very unlikely that `accessor` will be a contract, much less a contract with a generator,
     ; but if it is, we can try to generate values for it.
     (define accessor (property/c-accessor ctc))
     (define val-ctc-first-order (contract-first-order (property/c-val-ctc ctc)))
     (λ (fuel)
       (define sub-fuel (inexact->exact (ceiling (sqrt fuel))))
       (define val-generate (contract-random-generate/choose accessor sub-fuel))
       (and val-generate
            (λ () (let loop ([i sub-fuel])
                    (if (zero? i)
                        contract-random-generate-fail
                        (let ([val (accessor (val-generate))])
                          (if (val-ctc-first-order val)
                              val
                              (loop (sub1 i))))))))))
   #:list-contract?
   (λ (ctc) (list-contract? (property/c-accessor ctc)))))

(define/subexpression-pos-prop (property/c accessor ctc-v #:name [name (object-name accessor)])
  (unless (and (procedure? accessor) (procedure-arity-includes? accessor 1))
    (raise-argument-error 'property/c "(procedure-arity-includes/c 1)" accessor))
  (make-property/c accessor (coerce-flat-contract 'property/c ctc-v) name))
