#lang racket/base
(require rackunit/docs-complete)
(check-docs (quote plai/web))
(check-docs (quote plai/test-harness))
(check-docs (quote plai/random-mutator))
(check-docs (quote plai/mutator) #:skip #rx"^#%")
(check-docs (quote plai))
(check-docs (quote plai/datatype))
(check-docs (quote plai/collector))
