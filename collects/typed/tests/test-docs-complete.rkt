#lang racket/base
(require rackunit/docs-complete)
(define exclude
  '(;; these are currently undocumented in `racket' as well
    define-place
    deserialize-info:set-v0
    make-primitive-class
    place-sleep
    procedure-closure-contents-eq?
    processor-count
    ;; the following are contracted proxy values that are
    ;; already documented in `racket'
    default-continuation-prompt-tag))
(check-docs (quote typed-scheme) #:skip exclude)
(check-docs (quote typed/scheme) #:skip exclude)
(check-docs (quote typed/scheme/base) #:skip exclude)
(check-docs (quote typed/racket) #:skip exclude)
(check-docs (quote typed/racket/base) #:skip exclude)
