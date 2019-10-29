#lang racket/base

(require (for-syntax racket/base)
         "arrow-common.rkt"
         "blame.rkt"
         "guts.rkt"
         "prop.rkt"
         "misc.rkt")

(provide (rename-out [_unconstrained-domain-> unconstrained-domain->]))

(define-syntax (_unconstrained-domain-> stx)
  (syntax-case stx ()
    [(_ rngs ...)
     (with-syntax ([(res-x ...) (generate-temporaries #'(rngs ...))]
                   [(p-app-x ...) (generate-temporaries #'(rngs ...))])
       #`(build-unconstrained-domain->
          (list rngs ...)
          (λ (val blame+neg-party rngs-list blame-party-info neg-party p-app-x ...)
            (define res-checker
              (case-lambda
                [(res-x ...) (values/drop (p-app-x res-x neg-party) ...)]
                [results
                 (bad-number-of-results (car blame+neg-party)
                                        val
                                        #,(length (syntax->list #'(rngs ...)))
                                        results
                                        #:missing-party neg-party)]))
            (make-keyword-procedure
             (λ (kwds kwd-vals . args)
               (with-contract-continuation-mark
                blame+neg-party
                #,(check-tail-contract
                   #'rngs-list
                   #'blame-party-info
                   #'neg-party
                   (list #'res-checker)
                   (λ (s) #`(apply values #,@s kwd-vals args))
                   #'blame+neg-party)))
             (λ args
               (with-contract-continuation-mark
                blame+neg-party
                #,(check-tail-contract
                   #'rngs-list
                   #'blame-party-info
                   #'neg-party
                   (list #'res-checker)
                   (λ (s) #`(apply values #,@s args))
                   #'blame+neg-party)))))))]))

(define (build-unconstrained-domain-> range-maybe-contracts wrapper-proc)
  (define range-contracts (coerce-contracts 'unconstrained-domain-> range-maybe-contracts))
  (define chaperone? (andmap chaperone-contract? range-contracts))
  (cond
    [chaperone?
     (make-chaperone-unconstrained-domain-> range-contracts wrapper-proc)]
    [else
     (make-impersonator-unconstrained-domain-> range-contracts wrapper-proc)]))

(define (unconstrained-domain->-projection ctc)
  (define range-contracts (unconstrained-domain->-ranges ctc))
  (define make-wrapper-proc (unconstrained-domain->-make-wrapper-proc ctc))
  (define late-neg-projections (map get/build-late-neg-projection range-contracts))
  (define can-check-procedure-result-arity? (andmap any/c? range-contracts))
  (define desired-procedure-result-arity (length range-contracts))
  (define chaperone-or-impersonate-procedure (if (chaperone-unconstrained-domain->? ctc)
                                                 chaperone-procedure
                                                 impersonate-procedure))
  (λ (orig-blame)
    (define blame-party-info (get-blame-party-info orig-blame))
    (define range-blame (blame-add-range-context orig-blame))
    (define projs (for/list ([late-neg-projection (in-list late-neg-projections)])
                    (late-neg-projection range-blame)))
    (λ (val neg-party)
      (check-is-a-procedure orig-blame neg-party val)
      (define blame+neg-party (cons orig-blame neg-party))
      (if (and can-check-procedure-result-arity?
               (equal? desired-procedure-result-arity
                       (procedure-result-arity val)))
          val
          (chaperone-or-impersonate-procedure
           val
           (apply make-wrapper-proc
                  val
                  blame+neg-party
                  range-contracts
                  blame-party-info
                  neg-party
                  projs)
           impersonator-prop:contracted ctc
           impersonator-prop:blame (blame-add-missing-party orig-blame neg-party)
           impersonator-prop:application-mark
           (cons tail-contract-key (list* neg-party blame-party-info range-contracts)))))))

(define (unconstrained-domain->-name ud)
  (apply build-compound-type-name 'unconstrained-domain->
         (map contract-name (unconstrained-domain->-ranges ud))))

(define (unconstrained-domain->-first-order ud)
  (λ (val)
    (procedure? val)))

(define (unconstrained-domain->-stronger this that)
  (and (unconstrained-domain->? that)
       (pairwise-stronger-contracts? (unconstrained-domain->-ranges this)
                                     (unconstrained-domain->-ranges that))))

(define-struct unconstrained-domain-> (ranges make-wrapper-proc)
  #:property prop:custom-write custom-write-property-proc)

(define-struct (chaperone-unconstrained-domain-> unconstrained-domain->) ()
  #:property
  prop:chaperone-contract
  (build-chaperone-contract-property
   #:trusted trust-me
   #:name unconstrained-domain->-name
   #:first-order unconstrained-domain->-first-order
   #:late-neg-projection unconstrained-domain->-projection
   #:stronger unconstrained-domain->-stronger))

(define-struct (impersonator-unconstrained-domain-> unconstrained-domain->) ()
  #:property
  prop:chaperone-contract
  (build-chaperone-contract-property
   #:trusted trust-me
   #:name unconstrained-domain->-name
   #:first-order unconstrained-domain->-first-order
   #:late-neg-projection unconstrained-domain->-projection
   #:stronger unconstrained-domain->-stronger))

(define (check-is-a-procedure orig-blame neg-party val)
  (unless (procedure? val)
    (raise-blame-error orig-blame #:missing-party neg-party
                       val
                       '(expected: "a procedure" given: "~v")
                       val)))
