#lang racket
(require racket/runtime-path
         rackunit
         rackunit/text-ui
         "ast.rkt"
         
         "private/lex.rkt"
         "tool/syntax-color.rkt"
         "parse.rkt"         
         
         "pretty.rkt"
         
         "private/env.rkt"
         "private/subst.rkt"
         "private/unify.rkt"
         "private/variant.rkt"
         
         "runtime.rkt"
         "eval.rkt")

(define-runtime-path racket-mod "racket.rkt")
(define stdout (current-output-port))

(run-tests
 (test-suite
  "Datalog"
  ast-tests
  
  lex-tests
  syntax-color-tests
  parse-tests  
  
  pretty-tests
  
  env-tests
  subst-tests
  unify-tests
  variant-tests
  
  runtime-tests
  eval-tests
  
  (test-case "Racket Interop"
             (parameterize ([current-output-port stdout])
               (dynamic-require racket-mod #f)))))
