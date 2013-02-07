#lang at-exp racket/base
(require rackunit
         syntax-color/racket-lexer
         unstable/2d/private/lexer)

(check-equal? (cropped-regions 0 10 '()) '())
(check-equal? (cropped-regions 0 10 '((0 . 10))) '((0 . 10)))
(check-equal? (cropped-regions 0 10 '((0 . 5) (7 . 10))) '((7 . 10) (0 . 5)))
(check-equal? (cropped-regions 0 10 '((-1 . 4))) '((0 . 4)))
(check-equal? (cropped-regions 0 10 '((-4 . -3))) '())
(check-equal? (cropped-regions 0 10 '((20 . 30))) '())
(check-equal? (cropped-regions 0 10 '((1 . 4) (5 . 20))) '((5 . 10) (1 . 4)))
(check-equal? (cropped-regions 0 10 '((-5 . 10))) '((0 . 10)))
(check-equal? (cropped-regions 13 37 '((11 . 13))) '())

(define (run-lexer . strs)
  (define port (open-input-string (apply string-append strs)))
  (port-count-lines! port)
  (let loop ([mode #f])
    (define-values (val tok paren start end backup new-mode) 
      ((lexer racket-lexer) port 0 mode))
    (cons (list val tok paren start end backup)
          (cond
            [(equal? tok 'eof) '()]
            [else (loop new-mode)]))))


(check-equal?
 @run-lexer{#2d
            ╔══╦═══╗
            ║+ ║"a"║
            ╠══╬═══╣
            ║34║"b"║
            ╚══╩═══╝}
 `(("#2d" hash-colon-keyword #f 1 4 0)
   ("\n" white-space #f 4 5 4)
   ("╔══╦═══╗" parenthesis #f 5 13 5)
   (" " white-space #f 13 14 13)
   ("║" parenthesis #f 14 15 14)
   ("+" symbol #f 15 16 15)
   (" " white-space #f 16 17 16)
   ("║" parenthesis #f 17 18 17)
   ("\"a\"" string #f 18 21 18)
   ("║" parenthesis #f 21 22 21)
   (" " white-space #f 22 23 22)
   ("╠══╬═══╣" parenthesis #f 23 31 23)
   (" " white-space #f 31 32 31)
   ("║" parenthesis #f 32 33 32)
   ("34" constant #f 33 35 33)
   ("║" parenthesis #f 35 36 35)
   ("\"b\"" string #f 36 39 36)
   ("║" parenthesis #f 39 40 39)
   (" " white-space #f 40 41 40)
   ("╚══╩═══╝" parenthesis #f 41 49 41)
   (,eof eof #f #f #f 0)))

(check-equal?
 @run-lexer["#2d\r\n"]{╔══╦═══╗
                       ║+ ║"a"║
                       ╠══╬═══╣
                       ║34║"b"║
                       ╚══╩═══╝}
 `(("#2d" hash-colon-keyword #f 1 4 0)
   ("\r\n" white-space #f 4 5 4)
   ("╔══╦═══╗" parenthesis #f 5 13 5)
   (" " white-space #f 13 14 13)
   ("║" parenthesis #f 14 15 14)
   ("+" symbol #f 15 16 15)
   (" " white-space #f 16 17 16)
   ("║" parenthesis #f 17 18 17)
   ("\"a\"" string #f 18 21 18)
   ("║" parenthesis #f 21 22 21)
   (" " white-space #f 22 23 22)
   ("╠══╬═══╣" parenthesis #f 23 31 23)
   (" " white-space #f 31 32 31)
   ("║" parenthesis #f 32 33 32)
   ("34" constant #f 33 35 33)
   ("║" parenthesis #f 35 36 35)
   ("\"b\"" string #f 36 39 36)
   ("║" parenthesis #f 39 40 39)
   (" " white-space #f 40 41 40)
   ("╚══╩═══╝" parenthesis #f 41 49 41)
   (,eof eof #f #f #f 0)))

;; test tokens that cross lines (and thus need cropping)
(check-equal?
 @run-lexer{#2d
            ╔══╦═══╗
            ║+ ║"a ║
            ║+ ║ a"║
            ╠══╬═══╣
            ║34║"b"║
            ╚══╩═══╝}
 `(("#2d" hash-colon-keyword #f 1 4 0)
   ("\n" white-space #f 4 5 4)
   ("╔══╦═══╗" parenthesis #f 5 13 5)
   (" " white-space #f 13 14 13)
   ("║" parenthesis #f 14 15 14)
   ("+" symbol #f 15 16 15)
   (" " white-space #f 16 17 16)
   ("║" parenthesis #f 17 18 17)
   ("\"a " string #f 18 21 18)
   ("║" parenthesis #f 21 22 21)
   (" " white-space #f 22 23 22)
   ("║" parenthesis #f 23 24 23)
   ("+" symbol #f 24 25 24)
   (" " white-space #f 25 26 25)
   ("║" parenthesis #f 26 27 26)
   (" a\"" string #f 27 30 27)
   ("║" parenthesis #f 30 31 30)
   (" " white-space #f 31 32 31)
   ("╠══╬═══╣" parenthesis #f 32 40 32)
   (" " white-space #f 40 41 40)
   ("║" parenthesis #f 41 42 41)
   ("34" constant #f 42 44 42)
   ("║" parenthesis #f 44 45 44)
   ("\"b\"" string #f 45 48 45)
   ("║" parenthesis #f 48 49 48)
   (" " white-space #f 49 50 49)
   ("╚══╩═══╝" parenthesis #f 50 58 50)
   (,eof eof #f #f #f 0)))

(check-equal?
 @run-lexer{#2d
            ╔══╦═══╗
            ║+ ║ "a"║
            ╠══╬═══╣
            ║34║"b"║
            ╚══╩═══╝}
 `(("#2d" hash-colon-keyword #f 1 4 0)
   ("\n" white-space #f 4 5 4)
   ("╔══╦═══╗\n║+ ║ \"a" no-color #f 5 21 5)
   ("\"║\n╠══╬═══╣\n║34║\"b\"║\n╚══╩═══╝" error #f 21 50 21)
   (,eof eof #f #f #f 0)))

(check-equal?
 @run-lexer["#2d\r\n"]{╔══╦═══╗
                       ║+ ║ "a"║
                       ╠══╬═══╣
                       ║34║"b"║
                       ╚══╩═══╝}
 `(("#2d" hash-colon-keyword #f 1 4 0)
   ("\r\n" white-space #f 4 5 4)
   ("╔══╦═══╗\n║+ ║ \"a" no-color #f 5 21 5)
   ("\"║\n╠══╬═══╣\n║34║\"b\"║\n╚══╩═══╝" error #f 21 50 21)
   (,eof eof #f #f #f 0)))

