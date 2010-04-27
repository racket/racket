#lang scheme/base

(require tests/eli-tester "promise.ss" "lang.ss")

(test do (lang-tests)
      do (promise-tests))
