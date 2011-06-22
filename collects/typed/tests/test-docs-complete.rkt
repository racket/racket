#lang racket/base
(require rackunit/docs-complete)
;; these are currently undocumented in `racket' as well
(define exclude '(define-place
                  deserialize-info:set-v0
                  make-primitive-class
                  place-sleep
                  procedure-closure-contents-eq?
                  processor-count))
(check-docs (quote typed-scheme) #:skip exclude)
(check-docs (quote typed/scheme) #:skip exclude)
(check-docs (quote typed/scheme/base) #:skip exclude)
(check-docs (quote typed/racket) #:skip exclude)
(check-docs (quote typed/racket/base) #:skip exclude)
