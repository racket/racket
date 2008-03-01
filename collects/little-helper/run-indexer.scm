#lang scheme
(provide run-indexer)

(require "indexer/control.scm")

(define (run-indexer)
  (delete-and-generate-scribblings-indices))


