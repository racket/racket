#lang racket/base
(require rackunit/docs-complete)
(check-docs (quote syntax-color/token-tree))
(check-docs (quote syntax-color/scribble-lexer))
(check-docs (quote syntax-color/scheme-lexer))
(check-docs (quote syntax-color/paren-tree))
(check-docs (quote syntax-color/module-lexer))
(check-docs (quote syntax-color/ml))
(check-docs (quote syntax-color/default-lexer))
