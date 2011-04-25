#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote lazy/mz-without-promises))
(check-docs (quote lazy))
(check-docs (quote lazy/lazy))
(check-docs (quote lazy/lazy-tool))
(check-docs (quote lazy/force))
