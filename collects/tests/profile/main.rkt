#lang racket/base

(require tests/eli-tester "topsort.rkt" "analyze.rkt")

(test do (topological-sort-tests)
      do (analyze-tests))
