#lang racket/base
(require rackunit/docs-complete)
;; these are currently undocumented in `racket' as well
(define exclude '(make-primitive-class
                  make-custom-set
                  make-mutable-custom-set
                  make-weak-custom-set
                  place-sleep))
(check-docs (quote typed-scheme) #:skip exclude)
(check-docs (quote typed/scheme) #:skip exclude)
(check-docs (quote typed/scheme/base) #:skip exclude)
(check-docs (quote typed/racket) #:skip exclude)
(check-docs (quote typed/racket/base) #:skip exclude)
