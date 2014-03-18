#lang racket/base
(require "stlc-sub-base.rkt" "../stlc/tests-lib.rkt")
(stlc-tests uses-bound-var?
            typeof
            red
            reduction-step-count
            Eval)
