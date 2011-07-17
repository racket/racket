#lang racket/base

(provide with-module-reading-parameterization)

(define (with-module-reading-parameterization thunk)
  (parameterize ([read-case-sensitive #t]
                 [read-square-bracket-as-paren #t]
                 [read-curly-brace-as-paren #t]
                 [read-accept-box #t]
                 [read-accept-compiled #t]
                 [read-accept-bar-quote #t]
                 [read-accept-graph #t]
                 [read-decimal-as-inexact #t]
                 [read-accept-dot #t]
                 [read-accept-infix-dot #t]
                 [read-accept-quasiquote #t]
                 [read-accept-reader #t]
                 [read-accept-lang #t]
                 [current-readtable #f])
    (thunk)))
