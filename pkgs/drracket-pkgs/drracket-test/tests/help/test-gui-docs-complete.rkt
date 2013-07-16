#lang racket/base
(require rackunit/docs-complete)
(check-docs (quote help/help))
(check-docs (quote help/bug-report))
