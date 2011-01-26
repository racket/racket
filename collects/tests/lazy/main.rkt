#lang scheme/base

(require tests/eli-tester "promise.rkt" "lang.rkt" "langimpl.rkt")

(test do (lang-tests)
      do (langimpl-tests)
      do (promise-tests)
)
