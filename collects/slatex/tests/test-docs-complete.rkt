#lang racket/base
(require rackunit/docs-complete)
(check-docs (quote slatex/slatex-wrapper))
(check-docs (quote slatex/slatex-launcher))
(check-docs (quote slatex/pdf-slatex-launcher))
