#lang racket/base

(require honu/core/private/syntax
         honu/core/private/parse2
         (for-syntax honu/core/private/parse2)
         honu/core/private/literals
         (for-syntax honu/core/private/compile)
         (for-syntax racket/base)
         (for-syntax (prefix-in parse: syntax/parse))
         (prefix-in parse: syntax/parse))

(define-honu-syntax syntax-parse
  (lambda (code context)

    (parse:define-splicing-syntax-class a-pattern #:literals (cruft)
      [parse:pattern (parse:~seq var:parse:id %colon class:parse:id)
                     #:with pattern #'(parse:~var var class)])

    (parse:syntax-parse code #:literals (cruft)
      [(_ data:honu-expression (#%braces (#%brackets something:a-pattern action:honu-delayed) ...) . rest)
       (define output
         (racket-syntax (parse:syntax-parse data.result
                           [(something.pattern) action.result] ...)))
       (values output #'rest #t)])))

(provide syntax-parse)
