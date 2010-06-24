#lang scheme/base
(require scheme/contract
         "../ast.ss"
         (for-syntax scheme/base))
(require (for-template scheme/base
                       "../eval.ss"))

(define (compile-module asts)
  (with-syntax ([(s ...) asts])
    (syntax
     (begin (eval-statement s) ...))))

(define (compile-stmt ast)
  (with-syntax ([s ast])
    (syntax
     (eval-statement s))))

(provide/contract
 [compile-module (list? . -> . syntax?)]
 [compile-stmt (statement/c . -> . syntax?)])