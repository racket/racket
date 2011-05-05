#lang racket/base
(require rackunit/docs-complete)
(check-docs (quote macro-debugger/syntax-browser))
(check-docs (quote macro-debugger/stepper))
(check-docs (quote macro-debugger/stepper-text))
(check-docs (quote macro-debugger/expand))
(check-docs (quote macro-debugger/emit))
