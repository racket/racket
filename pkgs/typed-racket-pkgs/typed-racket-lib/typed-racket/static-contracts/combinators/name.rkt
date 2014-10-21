#lang racket/base

;; Static contracts for Name types
;;
;; This module keeps track of Name types global to
;; a single type->contract use, which allows the instantiation
;; process to lift out the contracts for the potentially
;; mutually recursive Name types. This reduces the amount of
;; duplication that would result if we used ordinary recursive
;; static contracts.

(require "../structures.rkt"
         "../constraints.rkt"
         racket/contract
         racket/dict
         racket/syntax
         syntax/id-table)

(provide with-new-name-tables
         (contract-out
          [get-all-name-defs
           (-> (listof (list/c (listof identifier?)
                               static-contract?
                               static-contract?
                               static-contract?)))]
          [lookup-name-sc (-> identifier? symbol? (or/c #f static-contract?))]
          [register-name-sc (-> identifier?
                                (-> static-contract?)
                                (-> static-contract?)
                                (-> static-contract?)
                                any)]))

(define name-sc-table (make-parameter (make-free-id-table)))
(define name-defs-table (make-parameter (make-free-id-table)))

(define-syntax-rule (with-new-name-tables e)
  (parameterize ([name-sc-table (make-free-id-table)]
                 [name-defs-table (make-free-id-table)])
    e))

(define (get-all-name-defs)
  (define name-scs (name-sc-table))
  (for/list ([(name defs) (in-dict (name-defs-table))])
    (define scs (free-id-table-ref name-scs name))
    (define gen-names (map name-combinator-gen-name scs))
    (cons gen-names defs)))

(define (lookup-name-sc name typed-side)
  (define result (free-id-table-ref (name-sc-table) name #f))
  (and result
       (case typed-side
         [(both)    (car result)]
         [(typed)   (cadr result)]
         [(untyped) (caddr result)])))

(define (register-name-sc name typed-thunk untyped-thunk both-thunk)
  (define-values (typed-name untyped-name both-name)
    (values (generate-temporary)
            (generate-temporary)
            (generate-temporary)))
  (free-id-table-set! (name-sc-table)
                      name
                      (list (name-combinator null typed-name)
                            (name-combinator null untyped-name)
                            (name-combinator null both-name)))
  (define typed-sc (typed-thunk))
  (define untyped-sc (untyped-thunk))
  (define both-sc (both-thunk))
  (free-id-table-set! (name-defs-table)
                      name
                      (list typed-sc untyped-sc both-sc)))

(struct name-combinator combinator (gen-name)
  #:transparent
  #:property prop:combinator-name "name/sc"
  #:methods gen:sc
  [(define (sc-map v f) v)
   (define (sc-traverse v f)
     (void))
   (define (sc->contract v f)
     (name-combinator-gen-name v))
   (define (sc->constraints v f)
     (variable-contract-restrict (name-combinator-gen-name v)))])
