#lang scheme/base

(require tests/eli-tester "reader.ss" "preprocessor.ss" "collect.ss")

(test do (reader-tests)
      do (begin/collect-tests)
      do (preprocessor-tests))
