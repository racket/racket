#lang racket/base

(define (configure data)
  (current-read-interaction the-read))
(provide configure)

(require datalog/parse
         datalog/private/compiler)

(define (the-read src ip)
  (cond
    [(or (not (char-ready? ip))
         (eof-object? (peek-char ip)))
     eof]
    [else
     (compile-statement
      (parameterize ([current-source-name src])
        (parse-statement ip)))]))
