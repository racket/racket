#lang typed-scheme

(require scheme/match)

(: parse-sexpr : Any -> Number)
(define (parse-sexpr sexpr)
  (match sexpr
    [(list #{(? symbol? a) : (Listof Symbol)} ...) 1]))
