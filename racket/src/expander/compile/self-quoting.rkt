#lang racket/base

(provide self-quoting-in-linklet?)

(define (self-quoting-in-linklet? datum)
  (or (number? datum) (boolean? datum) (string? datum) (bytes? datum)))
