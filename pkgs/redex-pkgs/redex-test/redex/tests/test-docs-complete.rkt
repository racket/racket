#lang racket/base
(module test racket/base) ; <- DISABLES TEST
(require rackunit/docs-complete)
(check-docs (quote redex/reduction-semantics))
(check-docs (quote redex/pict))
(check-docs (quote redex))
(check-docs (quote redex/gui))
