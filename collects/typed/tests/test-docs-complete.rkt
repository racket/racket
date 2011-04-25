#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote typed/scheme))
(check-docs (quote typed/rackunit))
(check-docs (quote typed/racket))
