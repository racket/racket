#lang racket/base
(provide (struct-out export))

(struct export (id ext-id))

(struct export-like export (id ext-id [referenced? #:mutable]))
