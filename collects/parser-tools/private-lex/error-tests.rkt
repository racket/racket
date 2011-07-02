#lang scheme/base
(require (for-syntax scheme/base)
         "../lex.rkt"
         rackunit)

(define-syntax (catch-syn-error stx)
  (syntax-case stx ()
    [(_ arg)
     (datum->syntax
      #'here
      (with-handlers ((exn:fail:syntax? exn-message))
        (syntax-local-expand-expression  #'arg)
        "not-an-error"))]))

(check-regexp-match #rx"lex-abbrev" (catch-syn-error (define-lex-abbrev)))
(check-regexp-match #rx"lex-abbrev" (catch-syn-error (define-lex-abbrev a)))
(check-regexp-match #rx"lex-abbrev" (catch-syn-error (define-lex-abbrev (a b) v)))
(check-regexp-match #rx"lex-abbrev" (catch-syn-error (define-lex-abbrev 1 1)))
(check-regexp-match #rx"lex-abbrevs" (catch-syn-error (define-lex-abbrevs ())))

(check-regexp-match #rx"lex-trans" (catch-syn-error (define-lex-trans)))

(check-regexp-match #rx"lexer" (catch-syn-error (lexer)))
(check-regexp-match #rx"lexer" (catch-syn-error (lexer ("a" "b" "c"))))
(check-regexp-match #rx"lexer" (catch-syn-error (lexer ())))
(check-regexp-match #rx"lexer" (catch-syn-error (lexer (""))))

(check-regexp-match #rx"regular-expression" (catch-syn-error (lexer (a 1))))
(check-regexp-match #rx"regular-expression" (catch-syn-error (lexer ((a) 1))))
(check-regexp-match #rx"regular-expression" (catch-syn-error (let ((a 1)) (lexer ((a) 1)))))

(check-regexp-match #rx"regular-expression"
                    (catch-syn-error (let-syntax ((a 1))
                                       (lexer ((a) 1)))))

(check-regexp-match #rx"define-lex-trans"
                    (catch-syn-error 
                     (let ()
                       (define-lex-trans a 1)
                       (let ()
                         (lexer ((a) 1))))))

(check-regexp-match #rx"regular-expression" (catch-syn-error (lexer (1 1))))
(check-regexp-match #rx"repetition" (catch-syn-error (lexer ((repetition) 1))))
(check-regexp-match #rx"repetition" (catch-syn-error (lexer ((repetition #\1 #\1 "3") 1))))
(check-regexp-match #rx"repetition" (catch-syn-error (lexer ((repetition 1 #\1 "3") 1))))
(check-regexp-match #rx"repetition" (catch-syn-error (lexer ((repetition 1 0 "3") 1))))
(check-regexp-match #rx"complement" (catch-syn-error (lexer ((complement) 1))))
(check-regexp-match #rx"char-range" (catch-syn-error (lexer ((char-range) 1))))
(check-regexp-match #rx"char-range" (catch-syn-error (lexer ((char-range #\9 #\0) 1))))
(check-regexp-match #rx"char-complement" (catch-syn-error (lexer ((char-complement) 1))))
(check-regexp-match #rx"char-complement" (catch-syn-error (lexer ((char-complement (concatenation "1" "2")) 1))))
