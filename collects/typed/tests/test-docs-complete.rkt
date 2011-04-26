#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote typed/scheme))
(check-docs (quote typed/scheme/base))
(check-docs (quote typed/racket))
(check-docs (quote typed/racket/base))
