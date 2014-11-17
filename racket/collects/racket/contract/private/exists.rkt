#lang racket/base

(require "prop.rkt"
         "blame.rkt"
         "guts.rkt"
         "generate.rkt"
         "misc.rkt"
         (for-syntax racket/base syntax/name))

(provide (rename-out [_new-∃/c new-∃/c]
                     [_new-∀/c new-∀/c])
         ∀∃?)

(define (∀∃-proj ctc)
  (let ([in (∀∃/c-in ctc)]
        [out (∀∃/c-out ctc)]
        [pred? (∀∃/c-pred? ctc)]
        [neg? (∀∃/c-neg? ctc)])
    (define name (∀∃/c-name ctc))
    (λ (blame)
      (if (equal? neg? (blame-swapped? blame))
          (λ (val)
            (if (pred? val)
                (out val)
                (raise-blame-error blame val "not ~a: ~e" name val)))
          in))))

(define-struct ∀∃/c (in out pred? name neg?)
  #:omit-define-syntaxes
  #:property prop:custom-write custom-write-property-proc  
  #:property prop:contract
  (build-contract-property
   #:name (λ (ctc) (∀∃/c-name ctc))
   #:first-order (λ (ctc) (λ (x) #t)) ;; ???
   #:projection ∀∃-proj
   #:stronger (λ (this that) (equal? this that))
   #:generate (λ (ctc)
                (cond
                  [(∀∃/c-neg? ctc)
                   (λ (fuel)
                     (define env (contract-random-generate-get-current-environment))
                     (λ () (random-any/c env fuel)))]
                  [else
                   (λ (fuel) #f)]))))

(define-struct ∀∃ ())

(define-for-syntax (∀∃/trans which stx)
  (define name (or (syntax-local-name)
                   (let ([n (syntax-local-infer-name stx)])
                     (string->symbol
                      (format "∀∃-~a" (or n "unknown"))))))
  (syntax-case stx ()
    [x 
     (identifier? #'x) 
     #`(let ([which (case-lambda
                      [() (#,which '#,name)]
                      [(x) (#,which (or x '#,name))])])
         which)]
    [(f) #`(#,which '#,name)]
    [(f x) #`(#,which (or x '#,name))]
    [(f . x)
     (with-syntax ([app (datum->syntax stx '#%app stx stx)])
       #`(app #,which . x))]))

(define-syntax (_new-∀/c stx) (∀∃/trans #'new-∀/c stx))
(define-syntax (_new-∃/c stx) (∀∃/trans #'new-∃/c stx))

(define (new-∃/c raw-name) (mk raw-name #t))
(define (new-∀/c raw-name) (mk raw-name #f))

(define (mk raw-name neg?)
  (define name (string->symbol (format "~a/~a" raw-name (if neg? "∃" "∀"))))
  (define-values (struct-type constructor predicate accessor mutator)
    (make-struct-type name struct:∀∃ 1 0))
  (make-∀∃/c constructor (λ (x) (accessor x 0)) predicate raw-name neg?))
