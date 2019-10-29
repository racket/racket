#lang racket/base
(require "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "base.rkt"
         (for-syntax racket/base)
         syntax/location)
(provide (rename-out [struct-type-property/c* struct-type-property/c])
         struct-guard/c)

(define (get-stpc-late-neg-proj stpc)
  (define get-late-neg-proj
    (get/build-late-neg-projection
     (struct-type-property/c-value-contract stpc)))
  (λ (input-blame)
    (define blame (blame-add-context input-blame "the struct property value of" #:swap? #t))
    (define late-neg-proj (get-late-neg-proj blame))
    (λ (x neg-party)
      (unless (struct-type-property? x)
        (raise-blame-error input-blame x #:missing-party neg-party
                           '(expected "struct-type-property" given: "~e")
                           x))
      (define blame+neg-party (cons blame neg-party))
      (define-values (nprop _pred _acc)
        (make-struct-type-property
         (wrap-name x)
         (lambda (val _info)
           (with-contract-continuation-mark
            blame+neg-party
            (late-neg-proj val neg-party)))
         (list (cons x values))))
      nprop)))

(define (wrap-name x)
  (string->symbol (format "wrapped-~a" (object-name x))))

(struct struct-type-property/c (value-contract)
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
             (build-contract-property
              #:trusted trust-me
              #:name (lambda (c)
                       (build-compound-type-name
                        'struct-type-property/c
                        (struct-type-property/c-value-contract c)))
              #:first-order (lambda (c) struct-type-property?)
              #:late-neg-projection get-stpc-late-neg-proj))

(define struct-type-property/c*
  (let ([struct-type-property/c
         (lambda (value-contract)
           (struct-type-property/c
            (coerce-contract 'struct-type-property/c value-contract)))])
    struct-type-property/c))

(define-syntax (struct-guard/c stx)
  (syntax-case stx ()
    [(_ . args)
     #`(struct-guard/c/proc (quote-srcloc #,stx)
                            (current-contract-region)
                            . args)]))

(define (struct-guard/c/proc loc blame-party . ctc-args)
  (define ctcs
    (for/list ([arg (in-list ctc-args)]
               [i (in-naturals)])
      (define ctc (coerce-contract/f arg))
      (unless ctc
        (apply raise-argument-error
               'struct-guard/c
               "contract?"
               i
               ctc-args))
      ctc))

  ;; don't want to depend on racket/list, so duplicate this
  ;; (plus we know that it will always be a non-empty list,
  ;;  so skip some checks)
  (define (last l)
    (let loop ([l l])
      (if (pair? (cdr l)) (loop (cdr l)) (car l))))
  
  (define number-of-contracts (length ctcs))
  
  ;; would like to have this be specialized to the number of
  ;; arguments there actually are, but given the fact that
  ;; we're creating blame objects and projections after getting
  ;; the arguments it doesn't seem worth bothering for now
  ;; (we are creating the projections late because we don't
  ;; get the `name` until later on)
  (λ args
    (define name (last args))
    (unless (= (length args) (+ number-of-contracts 1))
      (error 'struct-guard/c
             "given ~a contracts, but the struct ~s has ~a fields"
             number-of-contracts
             name
             (- (length args) 1)))
    (define ctc-projs
      (for/list ([ctc (in-list ctcs)]
                 [i (in-naturals 1)])
        (make-apply-contract ctc
                             blame-party blame-party
                             (if (= number-of-contracts 1)
                                 name
                                 (format "~a, field ~a" name i))
                             loc
                             #f)))
    (apply values
           (for/list ([arg (in-list args)]
                      [proj (in-list ctc-projs)])
             (proj arg)))))
