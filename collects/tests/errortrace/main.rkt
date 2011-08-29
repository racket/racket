#lang racket/base

(require tests/eli-tester "wrap.rkt" "alert.rkt")

(wrap-tests)

(test do (alert-tests))
