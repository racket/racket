#lang racket/base
(require rackunit/docs-complete)
(check-docs (quote tests/zo-size))
(check-docs (quote tests/stress))
(check-docs (quote tests/run-automated-tests))
(check-docs (quote tests/eli-tester))
