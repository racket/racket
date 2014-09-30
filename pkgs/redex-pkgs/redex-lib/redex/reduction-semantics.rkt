#lang racket/base
(require racket/contract)

(require "private/reduction-semantics.rkt"
         "private/judgment-form.rkt"
         "private/matcher.rkt"
         "private/term.rkt"
         "private/rg.rkt"
         "private/error.rkt"
         "private/generate-term.rkt")

(provide exn:fail:redex?) ;; from error.rkt

(provide reduction-relation 
         --> fresh with ;; keywords for reduction-relation
         hole in-hole ;; keywords for term
         ::= ;; keywords for language definition
         I O ;; keyword for define-judgment-form
         extend-reduction-relation
         reduction-relation?
         
         compatible-closure
         context-closure
         
         define-language
         define-extended-language
         define-union-language
         plug
         compiled-lang?
         term
         term-let
         define-metafunction
         define-metafunction/extension
         define-relation
         define-judgment-form
         define-extended-judgment-form
         judgment-holds
         build-derivations
         (struct-out derivation)
         in-domain?
         caching-enabled?
         make-coverage
         coverage?
         check-redudancy)

(provide (rename-out [test-match redex-match])
         (rename-out [test-match? redex-match?])
         term-match
         term-match/single
         redex-let
         redex-let*
         define-term
         match? match-bindings
         make-bind bind? bind-name bind-exp
         
         test-equal
         test-->>
         test-->
         test-->>∃ (rename-out [test-->>∃ test-->>E])
         test-predicate
         test-results)

(provide redex-check
         generate-term
         check-metafunction
         check-reduction-relation
         redex-generator
         exn:fail:redex:generation-failure?
         (struct-out exn:fail:redex:test)
         (struct-out counterexample)
         depth-dependent-order?)

(provide variable-not-in
         variables-not-in)

(provide/contract
 [current-traced-metafunctions (parameter/c (or/c 'all (listof symbol?)))]
 [reduction-relation->rule-names (-> reduction-relation? (listof symbol?))]
 [language-nts (-> compiled-lang? (listof symbol?))]
 [set-cache-size! (-> number? void?)]
 [apply-reduction-relation (-> reduction-relation? any/c (listof any/c))]
 [apply-reduction-relation/tag-with-names
  (-> reduction-relation? any/c (listof (list/c (or/c false/c string?) any/c)))]
 [apply-reduction-relation* (->* (reduction-relation? any/c) (#:cache-all? boolean? #:stop-when (-> any/c any)) (listof any/c))]
 [current-cache-all? (parameter/c boolean?)]
 [union-reduction-relations (->* (reduction-relation? reduction-relation?)
                                 ()
                                 #:rest (listof reduction-relation?)
                                 reduction-relation?)]
 [relation-coverage (parameter/c (listof coverage?))]
 [covered-cases (-> coverage? (listof (cons/c string? natural-number/c)))]
 [redex-pseudo-random-generator (parameter/c pseudo-random-generator?)]
 [default-attempt-size attempt-size/c]
 [default-check-attempts (parameter/c natural-number/c)]
 [default-equiv (parameter/c (-> any/c any/c any/c))])
