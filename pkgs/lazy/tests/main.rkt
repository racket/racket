#lang racket/base

(module+ test 
  (require tests/eli-tester "promise.rkt" "forcers.rkt" "lang.rkt" "space.rkt")
  
  (test do (promise-tests)
        do (forcer-tests)
        do (lang-tests)
        do (space-tests)))
