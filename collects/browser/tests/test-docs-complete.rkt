#lang racket/base
(require rackunit/docs-complete)
(check-docs (quote browser))
(check-docs (quote browser/htmltext))
(check-docs (quote browser/external))
(check-docs (quote browser/bullet-snip)  #:skip '(snip-class))
(check-docs (quote browser/browser))
(check-docs (quote browser/browser-unit))
(check-docs (quote browser/browser-sig))
