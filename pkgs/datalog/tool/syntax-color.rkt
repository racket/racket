#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "../private/lex.rkt")

(provide get-syntax-token)

(define (syn-val lex a b c d)
  (values lex a b (position-offset c) (position-offset d)))

(define (colorize-string my-start-pos)
  (define lxr
    (lexer
     [(:~ #\" #\\) (lxr input-port)]
     [(:: #\\ #\\) (lxr input-port)]
     [(:: #\\ #\newline) (lxr input-port)]
     [(:: #\\ #\") (lxr input-port)]
     [(eof) (syn-val "" 'error #f my-start-pos end-pos)]
     [#\" (syn-val "" 'string #f my-start-pos end-pos)]))
  lxr)

(define get-syntax-token
  (lexer
   [(:+ whitespace)
    (syn-val lexeme 'whitespace #f start-pos end-pos)]
   [comment-re
    (syn-val lexeme 'comment #f start-pos end-pos)]
   [variable-re
    (syn-val lexeme 'symbol #f start-pos end-pos)]
   [identifier-re
    (syn-val lexeme 'identifier #f start-pos end-pos)]
   [(:or #\) #\() (syn-val lexeme 'parenthesis #f start-pos end-pos)]
   [(:or "!=" #\= #\? #\~ #\. #\, ":-") (syn-val lexeme 'parenthesis #f start-pos end-pos)]
   [(eof) (syn-val lexeme 'eof #f start-pos end-pos)]
   [#\" ((colorize-string start-pos) input-port)]
   [any-char (syn-val lexeme 'error #f start-pos end-pos)]))
