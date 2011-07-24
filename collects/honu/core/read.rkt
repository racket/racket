#lang racket/base

(require rackunit)
(require parser-tools/lex)
(require racket/match)

(define-tokens honu-tokens (number))

(define-empty-tokens honu-empty-tokens (eof fail whitespace))

(define honu-lexer
  (lexer-src-pos
    [(eof) (token-eof)]
    [(char-range #\0 #\9)
     (token-number (string->number lexeme))]
    [(union " ") (token-whitespace)]
    ))

(define (token-eof? token)
  (equal? 'eof (token-name token)))

(define (token-whitespace? token)
  (equal? 'whitespace (token-name token)))

(define (lex-string input)
  (define port (open-input-string input))
  (let loop ([tokens '()])
    (define next (honu-lexer port))
    ;; (printf "next ~a\n" next)
    (match next
	   [(struct* position-token ([token (? token-eof?)] [start-pos start] [end-pos end]))
	    ;; (printf "done lexing\n")
	    (reverse tokens)]
	   [(struct* position-token ([token (? token-whitespace?)] [start-pos start] [end-pos end]))
	    (loop tokens)]
	   [(position-token token start end)
	    ;; (printf "next is ~a eof? ~a\n" token (token-eof? token))
	    (loop (cons token tokens))])))

(test-case
  "Basic tests"
  (check-equal? (lex-string "5")
		(list (token-number 5)))
  (check-equal? (lex-string "5 8")
		(list (token-number 5) (token-number 8)))
  )
