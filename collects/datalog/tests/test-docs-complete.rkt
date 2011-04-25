#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote datalog/stx))
(check-docs (quote datalog/runtime))
(check-docs (quote datalog/pretty))
(check-docs (quote datalog/parse))
(check-docs (quote datalog))
(check-docs (quote datalog/eval))
(check-docs (quote datalog/ast))
