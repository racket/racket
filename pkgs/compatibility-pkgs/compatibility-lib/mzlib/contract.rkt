#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  provide arrow contracts from our local copy (mostly)
;;

(require "private/contract-arrow.rkt")
(provide (all-from-out "private/contract-arrow.rkt"))
(require (only-in racket/contract/base unconstrained-domain->))
(provide unconstrained-domain->)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  provide contracts for objects
;;
(require "private/contract-object.rkt")
(provide (all-from-out "private/contract-object.rkt"))

(require (only-in racket/class
                  is-a?/c
                  implementation?/c
                  subclass?/c
                  mixin-contract
                  make-mixin-contract))
(provide is-a?/c
         implementation?/c
         subclass?/c
         mixin-contract
         make-mixin-contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style define/contract
;;

(require "private/contract-define.rkt")
(provide (all-from-out "private/contract-define.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style flat mutable contracts
;;
(require "private/contract-mutable.rkt")
(provide (all-from-out "private/contract-mutable.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style flat struct contracts
;;
(require "private/contract-struct.rkt")
(provide (all-from-out "private/contract-struct.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; provide everything from the racket/ implementation
;; except the arrow contracts
;;

(require (prefix-in : racket/contract))
(require (for-syntax racket/provide-transform racket/base))
(define-syntax remove-prefix
  (make-provide-transformer
   (λ (stx ctxt)
     (syntax-case stx ()
       [(_ args ...)
        (for/list ([arg (in-list (syntax->list #'(args ...)))])
          (export arg 
                  (string->symbol
                   (regexp-replace #rx"^:" (symbol->string (syntax-e arg)) ""))
                  0
                  #f
                  arg))]))))

(provide (remove-prefix
          :define-contract-struct
          :</c
          :>/c
          :chaperone-contract?
          :contract-name
          :contract-projection
          :contract?
          :false/c
          :flat-contract
          :flat-contract-predicate
          :flat-contract?
          :flat-named-contract
          :impersonator-contract?
          :make-none/c
          :n->th
          :natural-number/c
          :printable/c
          :promise/c
          :or/c
          :prompt-tag/c
          :>=/c
          :syntax/c
          :any
          :non-empty-listof
          :any/c
          :between/c
          :cons/c
          :integer-in
          :symbols
          :real-in
          :list/c
          :continuation-mark-key/c
          :one-of/c
          :procedure-arity-includes/c
          :not/c
          :flat-rec-contract
          :flat-murec-contract
          :=/c
          :and/c
          :parameter/c
          :none/c
          :<=/c
          :listof
          :contract
          :current-contract-region
          :recursive-contract
          :provide/contract
          :build-compound-type-name
          :coerce-chaperone-contract
          :coerce-chaperone-contracts
          :coerce-contract
          :coerce-contract/f
          :coerce-contracts
          :coerce-flat-contract
          :coerce-flat-contracts
          :contract-first-order
          :contract-first-order-passes?
          :contract-stronger?
          :eq-contract-val
          :eq-contract?
          :equal-contract-val
          :equal-contract?
          :has-contract?
          :impersonator-prop:contracted
          :prop:contracted
          :value-contract
          :define/subexpression-pos-prop
          :define/final-prop
          :blame-add-unknown-context
          :blame-context
          :blame-contract
          :blame-fmt->-string
          :blame-negative
          :blame-original?
          :blame-positive
          :blame-replace-negative
          :blame-source
          :blame-swap
          :blame-swapped?
          :blame-value
          :blame?
          :current-blame-format
          :exn:fail:contract:blame-object
          :exn:fail:contract:blame?
          :make-exn:fail:contract:blame
          :raise-blame-error
          :struct:exn:fail:contract:blame
          :blame-add-context
          :exn:fail:contract:blame
          :build-chaperone-contract-property
          :build-contract-property
          :build-flat-contract-property
          :chaperone-contract-property?
          :contract-property?
          :contract-struct-exercise
          :contract-struct-generate
          :flat-contract-property?
          :make-chaperone-contract
          :make-contract
          :make-flat-contract
          :prop:chaperone-contract
          :prop:contract
          :prop:flat-contract
          :prop:opt-chaperone-contract
          :prop:opt-chaperone-contract-get-test
          :prop:opt-chaperone-contract?
          :skip-projection-wrapper?
          :opt/c
          :define-opt/c))
(provide 
 (rename-out [:or/c union])
 (rename-out [:string-len/c string/len]))

(define (build-flat-contract name pred) 
  (:flat-contract (procedure-rename pred name)))
(provide build-flat-contract)

(require racket/contract/combinator)
;; exports from racket/contract/combinator as of 5.3.5
(provide blame-add-unknown-context blame-context blame-contract blame-fmt->-string blame-negative
         blame-original? blame-positive blame-replace-negative blame-source blame-swap blame-swapped?
         blame-update blame-value blame? build-chaperone-contract-property build-compound-type-name 
         build-contract-property build-flat-contract-property chaperone-contract-property? 
         coerce-chaperone-contract coerce-chaperone-contracts coerce-contract coerce-contract/f
         coerce-contracts coerce-flat-contract coerce-flat-contracts contract-first-order
         contract-first-order-passes? contract-property? contract-stronger? contract-struct-exercise
         contract-struct-generate current-blame-format eq-contract-val eq-contract? equal-contract-val
         equal-contract? exn:fail:contract:blame-object exn:fail:contract:blame? 
         flat-contract-property? impersonator-prop:contracted make-chaperone-contract make-contract
         make-exn:fail:contract:blame make-flat-contract prop:chaperone-contract prop:contract
         prop:contracted prop:flat-contract prop:opt-chaperone-contract
         prop:opt-chaperone-contract-get-test prop:opt-chaperone-contract? raise-blame-error
         skip-projection-wrapper? struct:exn:fail:contract:blame define/final-prop 
         exn:fail:contract:blame blame-add-context define/subexpression-pos-prop)
