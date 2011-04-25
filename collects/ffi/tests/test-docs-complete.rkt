#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote ffi/vector))
(check-docs (quote ffi/unsafe))
(check-docs (quote ffi/objc))
(check-docs (quote ffi/file))
(check-docs (quote ffi/cvector))
