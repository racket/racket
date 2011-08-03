#lang racket/base

;; This module provides some functions used for metadata about the language

(provide honu-info)
(define (honu-info key default default-filter)
  (case key
    [(color-lexer) (dynamic-require 'honu/core/read
                                    'color-lexer)]
    [else
      (default-filter key default)]))
