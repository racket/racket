#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote meta/specs))
(check-docs (quote meta/spec-reader))
(check-docs (quote meta/dist-specs))
(check-docs (quote meta/checker))
(check-docs (quote meta/check-dists))
