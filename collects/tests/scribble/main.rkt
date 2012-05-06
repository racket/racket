#lang racket/base

(require tests/eli-tester
         "reader.rkt" "collect.rkt" "text-lang.rkt" "text-wrap.rkt"
         "docs.rkt")

(test do (reader-tests)
      do (begin/collect-tests)
      do (text-lang-tests)
      do (wrap-tests)
      do (docs-tests))
