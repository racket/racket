#lang racket/base

(require rackunit)
(require parser-tools/lex
	 (prefix-in : parser-tools/lex-sre))
(require racket/match)

(define-tokens honu-tokens (number identifier))

(define-empty-tokens honu-empty-tokens (eof fail whitespace
					left-parens right-parens
					left-bracket right-bracket
					left-brace right-brace))

(define-lex-abbrev identifier-first-character (:or (:/ #\a #\z)
						   (:/ #\A #\Z)))
(define-lex-abbrev identifier-character identifier-first-character)
(define-lex-abbrev identifier (:: identifier-first-character
				  (:+ identifier-character)))

(define honu-lexer
  (lexer-src-pos
    [(eof) (token-eof)]
    [(char-range #\0 #\9)
     (token-number (string->number lexeme))]
    ["(" (token-left-parens)]
    [")" (token-right-parens)]
    ["[" (token-left-bracket)]
    ["]" (token-right-bracket)]
    ["{" (token-left-brace)]
    ["}" (token-right-brace)]
    [identifier (token-identifier (string->symbol lexeme))]
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
  (check-equal? (lex-string "hello")
		(list (token-identifier 'hello)))
  (check-equal? (lex-string "()")
		(list (token-left-parens)
		      (token-right-parens)))
  (check-equal? (lex-string "()[]{}")
		(list (token-left-parens)
		      (token-right-parens)
		      (token-left-bracket)
		      (token-right-bracket)
		      (token-left-brace)
		      (token-right-brace)))
  )
