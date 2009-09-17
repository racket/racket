#lang scheme/base

(require "private/guts.ss")

(provide new-∃/c
         ∃?)

(define (∃-proj ctc)
  (let ([in (∃/c-in ctc)]
        [out (∃/c-out ctc)]
        [pred? (∃/c-pred? ctc)])
  (λ (pos-blame neg-blame src-info orig-str positive-position?)
    (if positive-position?
	in
        (λ (val)
          (if (pred? val)
              (out val)
              (raise-contract-error val src-info pos-blame orig-str 
                                    "non-polymorphic value: ~e"
                                    val)))))))

(define-struct ∃/c (in out pred? name)
  #:omit-define-syntaxes
  #:property proj-prop ∃-proj
  #:property name-prop (λ (ctc) (∃/c-name ctc))
  #:property first-order-prop
  (λ (ctc) (λ (x) #t)) ;; ???
      
  #:property stronger-prop
  (λ (this that) #f))

(define-struct ∃ ())

(define (new-∃/c raw-name)
  (define name (string->symbol (format "~a/∃" raw-name)))
  (define-values (struct-type constructor predicate accessor mutator)
    (make-struct-type name struct:∃ 1 0))
  (make-∃/c constructor (λ (x) (accessor x 0)) predicate raw-name))
