#lang racket/base
(require racket/match
         web-server/dispatch/coercion
         web-server/dispatch/bidi-match)

(define-syntax define-bidi-match-expander/coercions
  (syntax-rules ()
    [(_ id in-test? in out-test? out)
     (begin (define-coercion-match-expander in/m in-test? in)
            (define-coercion-match-expander out/m out-test? out)
            (define-bidi-match-expander id in/m out/m))]))

; number arg
(define string->number? (make-coerce-safe? string->number))
(define-bidi-match-expander/coercions number-arg
  string->number? string->number
  number? number->string)

; integer arg
(define (string->integer x)
  (define nx (string->number x))
  (if (integer? nx)
      nx
      (error 'string->integer "Not an integer string")))
(define string->integer? (make-coerce-safe? string->integer))
(define-bidi-match-expander/coercions integer-arg
  string->integer? string->integer
  integer? number->string)

; real arg
(define (string->real x)
  (define nx (string->number x))
  (if (real? nx)
      nx
      (error 'string->real "Not an real string")))
(define string->real? (make-coerce-safe? string->real))
(define-bidi-match-expander/coercions real-arg
  string->real? string->real
  real? number->string)

; string arg
(define-match-expander string->string/m
  (syntax-rules ()
    [(_ str) (? string? str)]))

(define-bidi-match-expander string-arg string->string/m string->string/m)

; symbol arg
(define-bidi-match-expander/coercions symbol-arg
  string? string->symbol
  symbol? symbol->string)

(provide number-arg
         integer-arg
         real-arg
         string-arg
         symbol-arg)
