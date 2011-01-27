#lang scheme/base

(require tests/eli-tester "promise.rkt" "lang.rkt" "langimpl.rkt")

(test do (lang-tests)
;      do (langimpl-tests) ; not working, so import test-take directly
      do (test-take)
      do (promise-tests)
)
