#lang racket/base

(require "private/guts.rkt")

(provide new-∃/c
         ∃?)

(define (∃-proj ctc)
  (let ([in (∃/c-in ctc)]
        [out (∃/c-out ctc)]
        [pred? (∃/c-pred? ctc)])
  (λ (blame)
    (if (blame-swapped? blame)
        (λ (val)
          (if (pred? val)
              (out val)
              (raise-blame-error blame
                                 val 
                                 "non-polymorphic value: ~e"
                                 val)))
	in))))

(define-struct ∃/c (in out pred? name)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:name (λ (ctc) (∃/c-name ctc))
   #:first-order (λ (ctc) (λ (x) #t)) ;; ???
   #:projection ∃-proj))

(define-struct ∃ ())

(define (new-∃/c raw-name)
  (define name (string->symbol (format "~a/∃" raw-name)))
  (define-values (struct-type constructor predicate accessor mutator)
    (make-struct-type name struct:∃ 1 0))
  (make-∃/c constructor (λ (x) (accessor x 0)) predicate raw-name))
