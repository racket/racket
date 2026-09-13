#lang racket/base
(provide (struct-out export))

(struct export (id ext-id) #:authentic)

(struct export-like export (id ext-id [referenced? #:mutable]) #:authentic)
