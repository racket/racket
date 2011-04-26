#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote xml/xml))
(check-docs (quote xml/plist))
(check-docs (quote xml))
