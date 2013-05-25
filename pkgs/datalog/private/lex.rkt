#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens dtokens (VARIABLE IDENTIFIER STRING))
(define-empty-tokens dpunct (LPAREN COMMA RPAREN TSTILE DOT EQUAL NEQUAL TILDE QMARK EOF))
(define-lex-abbrev line-break #\newline)
(define-lex-abbrev id-chars (char-complement (char-set "(,)=:.~?\"% \n")))
(define-lex-abbrev variable-re (:: upper-case (:* (:or upper-case lower-case (char-set "0123456789_")))))
(define-lex-abbrev identifier-re (:: id-chars (:* (:or upper-case id-chars))))
(define-lex-abbrev comment-re (:: "%" (complement (:: any-string line-break any-string)) line-break))

(define get-string-token
  (lexer
   [(:~ #\" #\\) (cons (car (string->list lexeme))
                       (get-string-token input-port))]
   [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
   [(:: #\\ #\newline) (cons #\newline (get-string-token input-port))]
   [(:: #\\ #\") (cons #\" (get-string-token input-port))]
   [#\" null]))

(define dlexer
  (lexer-src-pos
   [whitespace
    (return-without-pos (dlexer input-port))]
   [comment-re
    (return-without-pos (dlexer input-port))]
   [variable-re
    (token-VARIABLE lexeme)]
   [identifier-re
    (token-IDENTIFIER lexeme)]
   [":-" (token-TSTILE)]
   [#\" (token-STRING (list->string (get-string-token input-port)))]
   [#\( (token-LPAREN)]
   [#\, (token-COMMA)]
   [#\) (token-RPAREN)]
   [#\. (token-DOT)]
   [#\~ (token-TILDE)]
   [#\? (token-QMARK)]
   [#\= (token-EQUAL)]
   ["!=" (token-NEQUAL)]
   [(eof) (token-EOF)]))

(provide dtokens dpunct
         line-break id-chars variable-re identifier-re comment-re
         dlexer)
