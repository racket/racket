#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

#|
5 Syntax

In Datalog input, whitespace characters are ignored except when they separate adjacent tokens or when they occur in strings. Comments are also considered to be whitespace. The character `%' introduces a comment, which extends to the next line break. Comments do not occur inside strings.

The characters in Datalog input are collected into tokens according to the rules that follow. There are four classes of tokens: punctuations, variables, identifiers, and strings. The punctuation tokens are: `(', `,', `)', `=', `:-', `.', `~', `?', and `"'.

A variable is a sequence of Latin capital and small letters, digits, and the underscore character. A variable must begin with a Latin capital letter.

An identifier is a sequence of printing characters that does not contain any of the following characters: `(', `,', `)', `=', `:', `.', `~', `?', `"', `%', and space. An identifier must not begin with a Latin capital letter. Note that the characters that start punctuation are forbidden in identifiers, but the hyphen character is allowed.

A string is a sequence of characters enclosed in double quotes. Characters other than double quote, newline, and backslash may be directly included in a string. The remaining characters may be specified using escape characters, `\"', `\n', and `\\' respectively.

Other escape characters can be used to improve the readability of the input. If a string is too long to fit conveniently on one line, all but the final line containing the string can be ended with a backslash character, and each backslash newline pair is ignored. The character escape codes from the C programming language are allowed—`\a', `\b', `\f', `\n', `\r', `\t', `\v', `\'', and `\?'. The numeric escape codes consist of exactly two uppercase hex digits. Thus the ASCII character newline is `\0A', and zero is `\00'.
|#

(define-tokens dtokens (VARIABLE IDENTIFIER STRING))
(define-empty-tokens dpunct (LPAREN COMMA RPAREN TSTILE DOT EQUAL TILDE QMARK EOF))
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
   [(eof) (token-EOF)]))

(provide dtokens dpunct 
         line-break id-chars variable-re identifier-re comment-re
         dlexer)