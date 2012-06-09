#lang racket/base

(define-syntax-rule (standard-honu meta-level)
  (begin
    (require (for-meta meta-level
                       racket/base
                       racket/class
                       (prefix-in list: racket/list)
                       "private/macro2.rkt"
                       "private/class.rkt"
                       "private/operator.rkt"
                       (prefix-in literal: "private/literals.rkt")
                       (prefix-in syntax-parse: syntax/parse)
                       (prefix-in racket: racket/base)
                       (prefix-in parse: "private/parse2.rkt")
                       "private/honu2.rkt"))
    (provide (for-meta meta-level 
                       (rename-out [parse:honu-expression expression]
                                   [parse:honu-expression-list expression_list]
                                   [parse:honu-identifier identifier]
                                   [racket:else else]
                                   [racket:void void]
                                   [parse:honu-number number]
                                   [honu-function function]
                                   [honu-function fun]
                                   [honu-var var]
                                   [honu-== ==]
                                   [honu-not-equal !=]
                                   [honu--> %arrow]
                                   [honu-class class]
                                   [honu-require require]
                                   [honu-provide provide]
                                   [honu-new new]
                                   [honu-while while]
                                   [honu-macro macro]
                                   [honu-pattern pattern]
                                   [racket:read-line readLine]
                                   [honu-with-input-from-file withInputFromFile]
                                   [define-make-honu-operator operator]
                                   [honu-match match]
                                   [honu-with with]
                                   [literal:honu-where where]
                                   [honu-for-syntax for_syntax]
                                   [honu-var var]
                                   [honu-val val]
                                   [honu-for for]
                                   [honu-fold fold]
                                   [honu-to to]
                                   [honu-if if]
                                   [honu-quote quote]
                                   [honu-quasiquote quasiquote]
                                   [honu-+ +] [honu-- -]
                                   [honu-* *] [honu-/ /]
                                   [honu-modulo %]
                                   [honu-^ ^]
                                   [honu-> >] [honu-< <]
                                   [honu->= >=]
                                   [honu-<= <=]
                                   ;; [honu-equal =]
                                   ;; [honu-assignment :=]
                                   [honu-map map]
                                   [honu-flow \|]
                                   [honu-dot %dot]
                                   [honu--> %arrow]
                                   [honu-string=? string_equal]
                                   [honu-cons ::]
                                   [honu-and and] [honu-and &&]
                                   [honu-or or] [honu-or \|\|]
                                   [honu-not not] [honu-not !]
                                   [honu-structure structure]
                                   [honu-structure struct]
                                   [honu-syntax syntax]
                                   [honu-equal =]
                                   [honu-+= +=] [honu--= -=]
                                   [honu-*= *=] [honu-/= /=]
                                   [literal:honu-prefix prefix]
                                   [literal:honu-then then]
                                   [literal:colon %colon]
                                   [literal:honu-in in]
                                   [literal:semicolon %semicolon]
                                   [literal:honu-comma honu-comma]
                                   [literal:honu-$ honu-$]
                                   [literal:honu-<- <-]
                                   [literal:honu-in-lines inLines]
                                   [literal:#%brackets #%brackets]
                                   [literal:#%braces #%braces]
                                   [literal:#%parens #%parens])
                       (rename-out
                         [datum->syntax datum_to_syntax]
                         [syntax->datum syntax_to_datum]
                         [syntax->list syntax_to_list]
                         [list:first first]
                         [symbol->string symbol_to_string]
                         [string-append string_append])
                       print printf
                       true false
                       withSyntax
                       mergeSyntax
                       this
                       error
                       #%top
                       #%datum
                       (... ...)
                       ))))

(require "private/honu-typed-scheme.rkt")

;; Provide standard stuff at phase 1
(standard-honu 1)
(standard-honu 0)

(provide (rename-out [#%dynamic-honu-module-begin #%module-begin]
                     [honu-top-interaction #%top-interaction]))
