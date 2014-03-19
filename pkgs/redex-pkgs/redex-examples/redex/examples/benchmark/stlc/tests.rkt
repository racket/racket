#lang racket/base
(require "stlc-base.rkt" "tests-lib.rkt")
(stlc-tests uses-bound-var?
            typeof
            red
            reduction-step-count
            Eval
            subst)
