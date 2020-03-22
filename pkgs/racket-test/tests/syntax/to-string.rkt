#lang racket
(require syntax/to-string
         rackunit)

(check-equal? (syntax->string #'((a . 
                                      b))) "(a . \n     b)")
(check-equal? (syntax->string #'(( a b  c d))) "( a b  c d)")
(check-equal? (syntax->string #'(a 'b #(a b c) c)) "a 'b #(a b c) c")
(check-equal? (syntax->string #'((a b  _   d))) "(a b  _   d)")

(check-equal? (syntax->string #'((  a  b))) "(  a  b)")
; we can't find where . is, default to earliest position
(check-equal? (syntax->string #'((  a  b  .  c))) "(  a  b .   c)")

;; quote tests
(check-equal? (syntax->string #'('a)) "'a")
(check-equal? (syntax->string #'('  a)) "'  a")
(check-equal? (syntax->string #'((quote a))) "(quote a)")
(check-equal? (syntax->string #'((  quote  a))) "(  quote  a)")
(check-equal? (syntax->string #'((quote a b))) "(quote a b)")
(check-equal? (syntax->string #'((  quote  a  b))) "(  quote  a  b)")
(check-equal? (syntax->string #'((quote . a))) "(quote . a)")
(check-equal? (syntax->string #'((  quote  .  a))) "(  quote .   a)")
(check-equal? (syntax->string #'((quote a b . c))) "(quote a b . c)")
(check-equal? (syntax->string #'((  quote  a  b  .  c))) "(  quote  a  b .   c)")

;; unquote tests
(check-equal? (syntax->string #'(,a)) ",a")
(check-equal? (syntax->string #'(,  a)) ",  a")
(check-equal? (syntax->string #'((unquote a))) "(unquote a)")
(check-equal? (syntax->string #'((  unquote  a))) "(  unquote  a)")
(check-equal? (syntax->string #'((unquote a b))) "(unquote a b)")
(check-equal? (syntax->string #'((  unquote  a  b))) "(  unquote  a  b)")
(check-equal? (syntax->string #'((unquote . a))) "(unquote . a)")
(check-equal? (syntax->string #'((  unquote  .  a))) "(  unquote .   a)")
(check-equal? (syntax->string #'((unquote a b . c))) "(unquote a b . c)")
(check-equal? (syntax->string #'((  unquote  a  b  .  c))) "(  unquote  a  b .   c)")

;; quasiquote tests
(check-equal? (syntax->string #'(`a)) "`a")
(check-equal? (syntax->string #'(`  a)) "`  a")
(check-equal? (syntax->string #'((quasiquote a))) "(quasiquote a)")
(check-equal? (syntax->string #'((  quasiquote  a))) "(  quasiquote  a)")
(check-equal? (syntax->string #'((quasiquote a b))) "(quasiquote a b)")
(check-equal? (syntax->string #'((  quasiquote  a  b))) "(  quasiquote  a  b)")
(check-equal? (syntax->string #'((quasiquote . a))) "(quasiquote . a)")
(check-equal? (syntax->string #'((  quasiquote  .  a))) "(  quasiquote .   a)")
(check-equal? (syntax->string #'((quasiquote a b . c))) "(quasiquote a b . c)")
(check-equal? (syntax->string #'((  quasiquote  a  b  .  c))) "(  quasiquote  a  b .   c)")

;; unquote-splicing tests
(check-equal? (syntax->string #'(,@a)) ",@a")
(check-equal? (syntax->string #'(,@  a)) ",@  a")
(check-equal? (syntax->string #'((unquote-splicing a))) "(unquote-splicing a)")
(check-equal? (syntax->string #'((  unquote-splicing  a))) "(  unquote-splicing  a)")
(check-equal? (syntax->string #'((unquote-splicing a b))) "(unquote-splicing a b)")
(check-equal? (syntax->string #'((  unquote-splicing  a  b))) "(  unquote-splicing  a  b)")
(check-equal? (syntax->string #'((unquote-splicing . a))) "(unquote-splicing . a)")
(check-equal? (syntax->string #'((  unquote-splicing  .  a))) "(  unquote-splicing .   a)")
(check-equal? (syntax->string #'((unquote-splicing a b . c))) "(unquote-splicing a b . c)")
(check-equal? (syntax->string #'((  unquote-splicing  a  b  .  c))) "(  unquote-splicing  a  b .   c)")

;; syntax tests
(check-equal? (syntax->string #'(#'a)) "#'a")
(check-equal? (syntax->string #'(#'  a)) "#'  a")
(check-equal? (syntax->string #'((syntax a))) "(syntax a)")
(check-equal? (syntax->string #'((  syntax  a))) "(  syntax  a)")
(check-equal? (syntax->string #'((syntax a b))) "(syntax a b)")
(check-equal? (syntax->string #'((  syntax  a  b))) "(  syntax  a  b)")
(check-equal? (syntax->string #'((syntax . a))) "(syntax . a)")
(check-equal? (syntax->string #'((  syntax  .  a))) "(  syntax .   a)")
(check-equal? (syntax->string #'((syntax a b . c))) "(syntax a b . c)")
(check-equal? (syntax->string #'((  syntax  a  b  .  c))) "(  syntax  a  b .   c)")

;; quasisyntax tests
(check-equal? (syntax->string #'(#`a)) "#`a")
(check-equal? (syntax->string #'(#`  a)) "#`  a")
(check-equal? (syntax->string #'((quasisyntax a))) "(quasisyntax a)")
(check-equal? (syntax->string #'((  quasisyntax  a))) "(  quasisyntax  a)")
(check-equal? (syntax->string #'((quasisyntax a b))) "(quasisyntax a b)")
(check-equal? (syntax->string #'((  quasisyntax  a  b))) "(  quasisyntax  a  b)")
(check-equal? (syntax->string #'((quasisyntax . a))) "(quasisyntax . a)")
(check-equal? (syntax->string #'((  quasisyntax  .  a))) "(  quasisyntax .   a)")
(check-equal? (syntax->string #'((quasisyntax a b . c))) "(quasisyntax a b . c)")
(check-equal? (syntax->string #'((  quasisyntax  a  b  .  c))) "(  quasisyntax  a  b .   c)")

;; unsyntax tests
(check-equal? (syntax->string #'(#,a)) "#,a")
(check-equal? (syntax->string #'(#,  a)) "#,  a")
(check-equal? (syntax->string #'((unsyntax a))) "(unsyntax a)")
(check-equal? (syntax->string #'((  unsyntax  a))) "(  unsyntax  a)")
(check-equal? (syntax->string #'((unsyntax a b))) "(unsyntax a b)")
(check-equal? (syntax->string #'((  unsyntax  a  b))) "(  unsyntax  a  b)")
(check-equal? (syntax->string #'((unsyntax . a))) "(unsyntax . a)")
(check-equal? (syntax->string #'((  unsyntax  .  a))) "(  unsyntax .   a)")
(check-equal? (syntax->string #'((unsyntax a b . c))) "(unsyntax a b . c)")
(check-equal? (syntax->string #'((  unsyntax  a  b  .  c))) "(  unsyntax  a  b .   c)")

;; unsyntax tests
(check-equal? (syntax->string #'(#,a)) "#,a")
(check-equal? (syntax->string #'(#,  a)) "#,  a")
(check-equal? (syntax->string #'((unsyntax a))) "(unsyntax a)")
(check-equal? (syntax->string #'((  unsyntax  a))) "(  unsyntax  a)")
(check-equal? (syntax->string #'((unsyntax a b))) "(unsyntax a b)")
(check-equal? (syntax->string #'((  unsyntax  a  b))) "(  unsyntax  a  b)")
(check-equal? (syntax->string #'((unsyntax . a))) "(unsyntax . a)")
(check-equal? (syntax->string #'((  unsyntax  .  a))) "(  unsyntax .   a)")
(check-equal? (syntax->string #'((unsyntax a b . c))) "(unsyntax a b . c)")
(check-equal? (syntax->string #'((  unsyntax  a  b  .  c))) "(  unsyntax  a  b .   c)")

;; unsyntax-splicing tests
(check-equal? (syntax->string #'(#,@a)) "#,@a")
(check-equal? (syntax->string #'(#,@  a)) "#,@  a")
(check-equal? (syntax->string #'((unsyntax-splicing a))) "(unsyntax-splicing a)")
(check-equal? (syntax->string #'((  unsyntax-splicing  a))) "(  unsyntax-splicing  a)")
(check-equal? (syntax->string #'((unsyntax-splicing a b))) "(unsyntax-splicing a b)")
(check-equal? (syntax->string #'((  unsyntax-splicing  a  b))) "(  unsyntax-splicing  a  b)")
(check-equal? (syntax->string #'((unsyntax-splicing . a))) "(unsyntax-splicing . a)")
(check-equal? (syntax->string #'((  unsyntax-splicing  .  a))) "(  unsyntax-splicing .   a)")
(check-equal? (syntax->string #'((unsyntax-splicing a b . c))) "(unsyntax-splicing a b . c)")
(check-equal? (syntax->string #'((  unsyntax-splicing  a  b  .  c))) "(  unsyntax-splicing  a  b .   c)")
