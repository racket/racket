#lang racket/base

(require tests/eli-tester
         "reader.rkt" "text-lang.rkt" "collect.rkt" "docs.rkt")

(test do (reader-tests)
      do (begin/collect-tests)
      do (text-lang-tests)
      do (docs-tests))
