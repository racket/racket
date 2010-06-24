#lang racket/base
(require racket/contract
         "../ast.rkt"
         (for-syntax racket/base))
(require (for-template racket/base
                       "../eval.rkt"))

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