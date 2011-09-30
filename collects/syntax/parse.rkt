#lang racket/base
(require racket/contract/base
         "parse/pre.rkt"
         "parse/experimental/provide.rkt"
         "parse/experimental/contract.rkt")
(provide (except-out (all-from-out "parse/pre.rkt")
                     static)
         expr/c)
(provide-syntax-class/contract
 [static (syntax-class/c [(-> any/c any/c) (or/c string? symbol? #f)])])
