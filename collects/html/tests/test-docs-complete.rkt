#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote html/sgml-reader))
(check-docs (quote html))
(check-docs (quote html/html))
(check-docs (quote html/html-structs))
(check-docs (quote html/html-spec))
