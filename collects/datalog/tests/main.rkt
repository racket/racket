#lang racket
(require rackunit
         rackunit/text-ui
         "ast.rkt"
         
         "private/lex.rkt"
         "tool/syntax-color.rkt"
         "parse.rkt"
         "sexp.rkt"
         
         "pretty.rkt"
         
         "private/env.rkt"
         "private/subst.rkt"
         "private/unify.rkt"
         "private/variant.rkt"
         
         "runtime.rkt"
         "eval.rkt"         
         "private/compiler.rkt")

(run-tests
 (test-suite
  "Datalog"
  ast-tests
  
  lex-tests
  syntax-color-tests
  parse-tests
  sexp-tests
  
  pretty-tests
  
  env-tests
  subst-tests
  unify-tests
  variant-tests
  
  runtime-tests
  eval-tests
  
  compiler-tests))