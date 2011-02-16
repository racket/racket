#lang racket/base

(require tests/eli-tester
         "reader.rkt" "preprocessor.rkt" "collect.rkt" "docs.rkt")

(test do (reader-tests)
      do (begin/collect-tests)
      do (preprocessor-tests)
      do (docs-tests))
