#lang scheme/base
(require scheme/contract)

(require "private/reduction-semantics.ss"
         "private/matcher.ss"
         "private/term.ss"
         "private/rg.ss")

#;(provide (all-from-out "private/rg.ss"))

(provide reduction-relation 
         --> fresh with ;; keywords for reduction-relation
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
         metafunction
         in-domain?)

(provide (rename-out [test-match redex-match])
         term-match
         term-match/single
         match? match-bindings
         make-bind bind? bind-name bind-exp
         
         test-equal
         test-->
         test-predicate
         test-results)

(provide/contract
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
 [variable-not-in (any/c symbol? . -> . symbol?)]
 [variables-not-in (any/c (listof symbol?) . -> . (listof symbol?))])
