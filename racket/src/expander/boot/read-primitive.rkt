#lang racket/base
(require "../common/reflect-hash.rkt"
         "../read/api.rkt"
         "../read/primitive-parameter.rkt"
         "../read/readtable.rkt"
         "../read/special-comment.rkt"
         "../read/number.rkt")

;; Reader primitives are in their own module so that they can be
;; treated specially by the bootstrapped flattened. The expanded form
;; of the expander can refer to the host's implementations, and those
;; references are replaced by these implementations.

(provide read-primitives)

(define read-primitives
  (reflect-hash read
                read/recursive
                read-language

                string->number

                current-reader-guard
                ;; read-case-sensitive - shared with printer
                read-square-bracket-as-paren
                read-curly-brace-as-paren
                read-square-bracket-with-tag
                read-curly-brace-with-tag
                read-cdot
                read-accept-graph
                read-accept-compiled
                read-accept-box
                ;; read-accept-bar-quote - shared with printer
                read-decimal-as-inexact
                read-accept-dot
                read-accept-infix-dot
                read-accept-quasiquote
                read-accept-reader
                read-accept-lang
                
                current-readtable
                readtable?
                make-readtable
                readtable-mapping
                
                special-comment?
                make-special-comment
                special-comment-value))
