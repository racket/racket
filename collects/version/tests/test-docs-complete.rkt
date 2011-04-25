#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote version/utils))
(check-docs (quote version/tool))
(check-docs (quote version/patchlevel))
(check-docs (quote version/check))
