#lang racket/base

(require rackunit)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(define-tokens honu-tokens (number identifier))

(define-empty-tokens honu-empty-tokens
                     [eof fail whitespace
                      left-parens right-parens
                      left-bracket right-bracket
                      left-brace right-brace
                      end-of-line-comment])

(define-lex-abbrev digit (:/ #\0 #\9))
(define-lex-abbrev identifier-first-character (:or (:/ #\a #\z)
                                                   (:/ #\A #\Z)))
(define-lex-abbrev identifier-character (:or identifier-first-character
                                             digit))
(define-lex-abbrev identifier (:: identifier-first-character
                                  (:* identifier-character)))
(define-lex-abbrev number (:+ digit))

(define honu-lexer
  (lexer-src-pos
    [(eof) (token-eof)]
    [(:or "#" "//") (token-end-of-line-comment)]
    [number (token-number (string->number lexeme))]
    ["." (token-identifier '|.|)]
    ["(" (token-left-parens)]  [")" (token-right-parens)]
    ["[" (token-left-bracket)] ["]" (token-right-bracket)]
    ["{" (token-left-brace)]   ["}" (token-right-brace)]
    [identifier (token-identifier (string->symbol lexeme))]
    [(union " " "\t") (token-whitespace)]
    ))

(define-syntax (define-token? stx)
  (syntax-parse stx
    [(_ name)
     (define name? (datum->syntax #'name (string->symbol
                                           (format "token-~a?"
                                                 (symbol->string
                                                   (syntax->datum #'name))))
                                  #'name))
     (with-syntax ([name? name?])
       #'(define (name? token)
           (equal? 'name (token-name token))))]))

(define-syntax-rule (define-tokens? name ...)
                    (begin
                      (define-token? name) ...))

(define-tokens? eof whitespace end-of-line-comment number
                identifier left-parens right-parens)

(define (read-until-end-of-line input) (define (finish? what)
    (or (eof-object? what)
	(= (char->integer #\newline) what)))
  (let loop ()
    (define what (read-byte input))
    (when (not (finish? what))
      (loop))))

(define (read-tokens port)
  (let loop ([tokens '()])
    (define next (honu-lexer port))
    ;; (printf "next ~a\n" next)
    (match next
	   [(struct* position-token ([token (? token-eof?)] [start-pos start] [end-pos end]))
	    ;; (printf "done lexing\n")
	    (reverse tokens)]
	   [(struct* position-token ([token (? token-end-of-line-comment?)]
				     [start-pos start]
				     [end-pos end]))
	    (read-until-end-of-line port)
	    (loop tokens)]
	   [(struct* position-token ([token (? token-whitespace?)] [start-pos start] [end-pos end]))
	    (loop tokens)]
       [else (loop (cons next tokens))]
       #;
	   [(position-token token start end)
	    ;; (printf "next is ~a eof? ~a\n" token (token-eof? token))
	    (loop (cons token tokens))])))

;; convert a string to a stream of tokens
(define (lex-string input)
  (read-tokens (open-input-string input)))

;; converts a stream of tokens to a tree
(define (parse tokens)
  (let loop ([current '()]
             [tokens tokens]
             [stop '()])
    (if (null? tokens)
      (reverse current)
      (match (car tokens)
        [(position-token token start end)
         (cond
           [(or (token-number? token)
                (token-identifier? token))
            (loop (cons token current)
                  (cdr tokens)
                  stop)]
           [(token-right-parens? token)
            (match stop
              [(list 'right-parens rest ...)
               (values (reverse current)
                       rest)]
              [else (error 'parse "expected a left parentheses before the right parentheses")])]
           [(token-left-parens? token)
            (define-values (parsed more-tokens)
                           (loop '(#%parens)
                                 (cdr tokens)
                                 (cons 'right-parens stop)))
            (loop (cons parsed current)
                  more-tokens
                  stop)]
           [else (error 'parse "cannot parse ~a" token)])]))))

;; strip the source location from the position tokens
(define (strip tokens)
  (for/list ([token tokens])
    (match token
      [(position-token token start end) token])))

(define (honu-read-syntax [port (current-input-port)])
  (read-tokens (parse port)))

(define (honu-read [port (current-input-port)])
  (syntax->datum (honu-read-syntax port)))

(test-case
  "Basic tests"
  (check-equal? (strip (lex-string "5"))
                (list (token-number 5)))
  (check-equal? (strip (lex-string "5 8"))
                (list (token-number 5) (token-number 8)))
  (check-equal? (strip (lex-string "hello"))
                (list (token-identifier 'hello)))
  (check-equal? (strip (lex-string "()"))
                (list (token-left-parens)
                      (token-right-parens)))
  (check-equal? (strip (lex-string "()[]{}"))
                (list (token-left-parens)
                      (token-right-parens)
                      (token-left-bracket)
                      (token-right-bracket)
                      (token-left-brace)
                      (token-right-brace)))
  (check-equal? (strip (lex-string "foo // 5"))
                (list (token-identifier 'foo)))
  (check-equal? (strip (lex-string "foo // 5
                            bar"))
                (list (token-identifier 'foo)
                      (token-identifier 'bar)))
  (check-equal? (strip (lex-string "f(2)"))
                (list (token-identifier 'f)
                      (token-left-parens)
                      (token-number 2)
                      (token-right-parens)))
  )

(parse (lex-string "f(5)"))
