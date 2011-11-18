#lang racket/base

(define-syntax-rule (standard-honu meta-level)
  (begin
    (require (for-meta meta-level
                       (prefix-in racket: racket/base)
                       (prefix-in honu: "private/macro2.rkt")
                       (prefix-in honu: "private/class.rkt")
                       (prefix-in literal: "private/literals.rkt")
                       (prefix-in syntax-parse: syntax/parse)
                       (prefix-in racket: racket/base)
                       (prefix-in parse: "private/parse2.rkt")
                       (prefix-in honu: "private/honu2.rkt")))
    (provide (for-meta meta-level 
                       (rename-out [parse:honu-expression expression]
                                   [parse:honu-identifier identifier]
                                   [honu:honu-function function]
                                   [honu:honu-var var]
                                   [honu:honu-equal =]
                                   [honu:honu--> %arrow]
                                   [honu:honu-class class]
                                   [honu:honu-require require]
                                   [honu:honu-new new]
                                   [honu:honu-while while]
                                   [honu:honu-macro macro]
                                   [honu:honu-with-input-from-file with_input_from_file]
                                   [honu:define-make-honu-operator operator]
                                   [honu:honu-match match]
                                   [honu:honu-with with]
                                   [honu:honu-var var]
                                   [honu:honu-val val]
                                   [honu:honu-for for]
                                   [honu:honu-if if]
                                   [honu:honu-quote quote]
                                   [honu:honu-quasiquote quasiquote]
                                   [honu:honu-+ +] [honu:honu-- -]
                                   [honu:honu-* *] [honu:honu-/ /]
                                   [honu:honu-modulo %]
                                   [honu:honu-^ ^]
                                   [honu:honu-> >] [honu:honu-< <]
                                   [honu:honu->= >=]
                                   [honu:honu-<= <=]
                                   [honu:honu-equal =]
                                   [honu:honu-assignment :=]
                                   [honu:honu-map map]
                                   [honu:honu-flow \|]
                                   [honu:honu-dot %dot]
                                   [honu:honu--> %arrow]
                                   [honu:honu-string=? string_equal]
                                   [honu:honu-cons ::]
                                   [honu:honu-and and] [honu:honu-and &&]
                                   [honu:honu-or or] [honu:honu-or \|\|]
                                   [honu:honu-not not] [honu:honu-not !]
                                   [honu:honu-structure structure]
                                   [honu:honu-structure struct]
                                   [honu:honu-syntax syntax]
                                   [literal:honu-prefix prefix]
                                   [literal:honu-then then]
                                   [literal:colon %colon]
                                   [literal:honu-in in]
                                   [literal:semicolon %semicolon]
                                   [literal:honu-comma %comma]
                                   [literal:honu-comma %comma]
                                   [literal:honu-<- <-]
                                   [literal:#%brackets #%brackets]
                                   [literal:#%braces #%braces]
                                   [literal:#%parens #%parens])
                       racket:print racket:printf
                       honu:true honu:false
                       honu:withSyntax
                       honu:mergeSyntax
                       ))))

(require "private/honu-typed-scheme.rkt")

;; Provide standard stuff at phase 1
(standard-honu 1)
(standard-honu 0)

(provide #%top
         #%datum
         (rename-out [#%dynamic-honu-module-begin #%module-begin]
                     [honu-top-interaction #%top-interaction]))
