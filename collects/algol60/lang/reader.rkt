#lang s-exp syntax/module-reader
algol60/lang/algol60

#:read algol60-read
#:read-syntax algol60-read-syntax
#:info algol60-get-info
#:whole-body-readers? #t


(require "../parse.rkt"
         ;; Parses to generate an AST. Identifiers in the AST
         ;; are represented as syntax objects with source location.

         "../simplify.rkt"
         ;; Desugars the AST, transforming `for' to `if'+`goto',
         ;; and flattening `if' statements so they are always
         ;; of the for `if <exp> then goto <label> else goto <label>'

         "../compile.rkt"
         ;; Compiles a simplified AST to Scheme.

         racket/file
         syntax/strip-context)


(define (algol60-read in)
  (map syntax->datum (algol60-read-syntax #f in)))


(define (algol60-read-syntax src in)
  (define parsed (parse-a60-port in src))
  (define simplified (simplify parsed #'here))
  (define compiled (compile-simplified simplified #'here))
  (define stripped (strip-context compiled))

  (list stripped))



;; Extension: cooperate with DrRacket and tell it to use the default,
;; textual lexer and color scheme when editing algol programs.
(define (algol60-get-info key default default-filter)
  (case key
    [(color-lexer)
     (dynamic-require 'syntax-color/default-lexer
                      'default-lexer)]
    [else
     (default-filter key default)]))
