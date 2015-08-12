#lang racket/base
(require "private/custom-write.rkt"
         racket/contract/base)
(provide (contract-out
          [make-constructor-style-printer
           (-> (-> any/c (or/c symbol? string?))
               (-> any/c sequence?)
               (-> any/c output-port? (or/c #t #f 0 1) void?))]))
