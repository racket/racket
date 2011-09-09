#lang racket/base

;; This module provides some functions used for metadata about the language

(provide honu-info)
(define (honu-info key default default-filter)
  ; (printf "get info for ~a\n" key)
  (case key
    [(color-lexer) (dynamic-require 'honu/core/read
                                    'color-lexer)]
    [else
      (default-filter key default)]))

(provide honu-language-info)
(define (honu-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime) '(#(honu/core/runtime configure #f))]
      [else default])))
