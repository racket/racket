#lang scheme/base
(require scheme/contract)

(require "private/reduction-semantics.ss"
         "private/matcher.ss"
         "private/term.ss"
         "private/rg.ss"
         "private/error.ss")

(provide exn:fail:redex?) ;; from error.ss

(provide reduction-relation 
         --> fresh with ;; keywords for reduction-relation
         hole in-hole ;; keywords for term
         extend-reduction-relation
         reduction-relation?
         
         compatible-closure
         context-closure
         
         define-language
         define-extended-language
         plug
         compiled-lang?
         term
         term-let
         none?
         define-metafunction
         define-metafunction/extension
         define-relation
         metafunction
         in-domain?
         caching-enabled?
         make-coverage)

(provide (rename-out [test-match redex-match])
         term-match
         term-match/single
         match? match-bindings
         make-bind bind? bind-name bind-exp
         
         test-equal
         test-->>
         test-->
         test-predicate
         test-results)

(provide redex-check
         generate-term
         check-metafunction
         check-metafunction-contract
         check-reduction-relation
         exn:fail:redex:generation-failure?
         (struct-out exn:fail:redex:test)
         (struct-out counterexample))

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
 [apply-reduction-relation* (-> reduction-relation? any/c (listof any/c))]
 [union-reduction-relations (->* (reduction-relation? reduction-relation?)
                                 ()
                                 #:rest (listof reduction-relation?)
                                 reduction-relation?)]
 
 [lookup-binding (case-> 
                  (-> bindings? symbol? any)
                  (-> bindings? symbol? (-> any) any))]
 [relation-coverage (parameter/c (listof coverage?))]
 [covered-cases (-> coverage? (listof (cons/c string? natural-number/c)))])
