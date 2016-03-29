#lang racket/base

(require (for-syntax racket/base)
         "arrow-common.rkt"
         "blame.rkt"
         "guts.rkt"
         "prop.rkt")

(provide unconstrained-domain->)

(define-syntax (unconstrained-domain-> stx)
  (syntax-case stx ()
    [(_ rngs ...)
     (with-syntax ([(rngs-x ...) (generate-temporaries #'(rngs ...))]
                   [(proj-x ...) (generate-temporaries #'(rngs ...))]
                   [(p-app-x ...) (generate-temporaries #'(rngs ...))]
                   [(res-x ...) (generate-temporaries #'(rngs ...))])
       #`(let ([rngs-x (coerce-contract 'unconstrained-domain-> rngs)] ...)
           (let ([rngs-list (list rngs-x ...)]
                 [proj-x (get/build-late-neg-projection rngs-x)] ...)
             (define (projection wrapper get-ctc)
               (λ (orig-blame)
                 (define blame-party-info (get-blame-party-info orig-blame))
                 (define ctc (get-ctc))
                 (let ([rng-blame (blame-add-range-context orig-blame)])
                   (let* ([p-app-x (proj-x rng-blame)] ...)
                     (λ (val neg-party)
                       (check-is-a-procedure orig-blame neg-party val)
                       (define (res-checker res-x ...) (values/drop (p-app-x res-x neg-party) ...))
                       (define blame+neg-party (cons orig-blame neg-party))
                       (wrapper
                        val
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
                               #'blame+neg-party))))
                        impersonator-prop:contracted ctc
                        impersonator-prop:application-mark
                        (cons tail-contract-key (list neg-party blame-party-info rngs-x ...))))))))
             (make-unconstrained-domain-> (list rngs-x ...) 
                                          projection))))]))

(define (check-is-a-procedure orig-blame neg-party val)
  (unless (procedure? val)
    (raise-blame-error orig-blame #:missing-party neg-party
                       val
                       '(expected: "a procedure" given: "~v")
                       val)))

(define (make-unconstrained-domain-> ctcs late-neg-projection)
  (define name
    (apply build-compound-type-name 'unconstrained-domain->
           (map contract-name ctcs)))
  (define ctc
    (if (andmap chaperone-contract? ctcs)
        (make-chaperone-contract
         #:name name
         #:late-neg-projection (late-neg-projection chaperone-procedure (λ () ctc))
         #:first-order procedure?)
        (make-contract
         #:name name
         #:late-neg-projection (late-neg-projection impersonate-procedure (λ () ctc))
         #:first-order procedure?)))
  ctc)
