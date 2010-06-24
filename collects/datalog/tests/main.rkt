#lang scheme
(require schemeunit
         schemeunit/text-ui
         "ast.ss"
         
         "private/lex.ss"
         "drscheme/syntax-color.ss"
         "parse.ss"
         "sexp.ss"
         
         "pretty.ss"
         
         "private/env.ss"
         "private/subst.ss"
         "private/unify.ss"
         "private/variant.ss"
         
         "runtime.ss"
         "eval.ss"         
         "private/compiler.ss")

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