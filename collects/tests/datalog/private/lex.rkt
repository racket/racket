#lang racket
(require rackunit
         parser-tools/lex
         datalog/private/lex)

(provide lex-tests)

(define (test-lexer str tok-name [tok-value str])
  (define pv (dlexer (open-input-string str)))
  (define v (position-token-token pv))
  (test-equal? (format "lexer: ~a: <~a,~a>" str tok-name tok-value)
               (cons tok-name tok-value)
               (cons (token-name v) (token-value v))))

(define lex-tests
  (test-suite
   "lex"
   
   (test-lexer "=" 'EQUAL #f)
   (test-lexer "!=" 'NEQUAL #f)
   (test-lexer "?" 'QMARK #f)
   (test-lexer "~" 'TILDE #f)
   (test-lexer "." 'DOT #f)
   (test-lexer ")" 'RPAREN #f)
   (test-lexer "," 'COMMA #f)
   (test-lexer "(" 'LPAREN #f)
   (test-lexer "\"\"" 'STRING "")
   (test-lexer "\"foo\"" 'STRING "foo")
   (test-lexer "\"\\\"\"" 'STRING "\"")
   (test-lexer ":-" 'TSTILE #f)
   (test-lexer "" 'EOF #f)
   (test-lexer "Va1_" 'VARIABLE)
   (test-lexer "val_" 'IDENTIFIER)
   (test-lexer "912Kadf" 'IDENTIFIER)
   (test-lexer "    =" 'EQUAL #f)
   (test-lexer "% 12453\n=" 'EQUAL #f)
   
   ))
