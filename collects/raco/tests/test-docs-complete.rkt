#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote raco/raco))
(check-docs (quote raco))
(check-docs (quote raco/command-name))
(check-docs (quote raco/all-tools))
