#lang racket
(require rackunit
         datalog/tool/syntax-color)

(provide syntax-color-tests)

(define (test-color str key)
  (define-values (lex color b start end) (get-syntax-token (open-input-string str)))
  (test-equal? (format "Syntax Color: ~a: ~a" key str) color key))

(define syntax-color-tests
  (test-suite
   "syntax-color"
   
   (test-color " " 'whitespace)
   (test-color "   " 'whitespace)
   (test-color "\t" 'whitespace)
   (test-color "\n" 'whitespace)
   (test-color "% \n" 'comment)
   (test-color "% 12 31 2 6\n" 'comment)
   (test-color "Var" 'symbol)
   (test-color "V124_3" 'symbol)
   (test-color "var" 'identifier)
   (test-color "123var" 'identifier)
   (test-color "(" 'parenthesis)
   (test-color ")" 'parenthesis)
   (test-color "!=" 'parenthesis)
   (test-color "=" 'parenthesis)
   (test-color "?" 'parenthesis)
   (test-color "~" 'parenthesis)
   (test-color "." 'parenthesis)
   (test-color "," 'parenthesis)
   (test-color ":-" 'parenthesis)
   (test-color "\"foo\"" 'string)
   (test-color "\"fo\\\"o\"" 'string)
   (test-color "\"fo\no\"" 'string)
   (test-color "\"foo" 'error)
   (test-color ":" 'error)))
