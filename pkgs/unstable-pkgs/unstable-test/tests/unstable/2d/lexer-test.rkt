#lang at-exp racket/base
(require rackunit
         syntax-color/racket-lexer
         syntax-color/scribble-lexer
         syntax-color/lexer-contract
         unstable/options
         unstable/2d/private/lexer
         racket/port)

(check-equal? (cropped-regions 0 10 '()) '())
(check-equal? (cropped-regions 0 10 '((0 . 10))) '((0 . 10)))
(check-equal? (cropped-regions 0 10 '((0 . 5) (7 . 10))) '((7 . 10) (0 . 5)))
(check-equal? (cropped-regions 0 10 '((-1 . 4))) '((0 . 4)))
(check-equal? (cropped-regions 0 10 '((-4 . -3))) '())
(check-equal? (cropped-regions 0 10 '((20 . 30))) '())
(check-equal? (cropped-regions 0 10 '((1 . 4) (5 . 20))) '((5 . 10) (1 . 4)))
(check-equal? (cropped-regions 0 10 '((-5 . 10))) '((0 . 10)))
(check-equal? (cropped-regions 13 37 '((11 . 13))) '())

(define (run-lexer #:sub-lexer [sub-lexer/no-ex racket-lexer] . strs/specials)
  (define sub-lexer (if (has-option? sub-lexer/no-ex)
                        (exercise-option sub-lexer/no-ex)
                        sub-lexer/no-ex))
  (define-values (in out) (make-pipe-with-specials))
  (thread
   (λ ()
     (let loop ([s strs/specials])
       (cond
         [(list? s)
          (for ([s (in-list strs/specials)])
            (loop s))]
         [(string? s) (display s out)]
         [else (write-special s out)]))
     (close-output-port out)))
  (port-count-lines! in)
  (define the-lexer (exercise-option (2d-lexer sub-lexer)))
  (let loop ([mode #f])
    (define-values (val tok paren start end backup new-mode) 
      (the-lexer in 0 mode))
    (cons (list val tok paren start end backup)
          (cond
            [(equal? tok 'eof) '()]
            [else (loop (if (dont-stop? new-mode)
                            (dont-stop-val new-mode)
                            new-mode))]))))

(check-equal?
 (run-lexer "1234\n#2d\n")
 `(("1234" constant #f 1 5 0)
   ("\n" white-space #f 5 6 0) 
   ("#2d\n" error #f 6 10 0) 
   (,eof eof #f #f #f 0)))

(check-equal?
 (run-lexer "#2dsomething")
 `(("#2dsomething" error #f 1 13 0)
   (,eof eof #f #f #f 0)))

(check-equal?
 (run-lexer "#2dsomething\n")
 `(("#2dsomething\n" error #f 1 14 0)
   (,eof eof #f #f #f 0)))

(check-equal?
 (run-lexer "#2dsomething\n╔═══╗\n║   ║")
 `(("#2dsomething\n╔═══╗\n║   ║" error #f 1 25 0)
   (,eof eof #f #f #f 0)))

(check-equal?
 (run-lexer "#2dsomething\n  \n")
 `(("#2dsomething" hash-colon-keyword #f 1 13 0)
   ("\n" white-space #f 13 14 13) 
   ("  \n" error #f 14 17 14)
   (,eof eof #f #f #f 0)))

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

(printf "skipping the \\r\\n test: see the cracks-filled-in-tokens definition for where the bug lies\n")
#;
(check-equal?
 (run-lexer "#2d\r\n"
            "╔══╦═══╗\r\n"
            "║+ ║abc║\r\n"
            "╠══╬═══╣\r\n"
            "║34║def║\r\n"
            "╚══╩═══╝\r\n")
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
   ("  " white-space #f 31 32 31)
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

(check-equal?
 (run-lexer "   #2d\n"
            "   ╔═╦═╗\n"
            "   ║1║2║\n"
            "   ╠═╬═╣\n"
            "   ║3║4║\n"
            "   ╚═╩═╝\n")
  `(("   " white-space #f 1 4 0)
    ("#2d" hash-colon-keyword #f 4 7 0)
    ("\n" white-space #f 7 8 7)
    ("   " white-space #f 8 11 8)
    ("╔═╦═╗" parenthesis #f 11 16 11)
    ("    " white-space #f 16 20 16)
    ("║" parenthesis #f 20 21 20)
    ("1" constant #f 21 22 21)
    ("║" parenthesis #f 22 23 22)
    ("2" constant #f 23 24 23)
    ("║" parenthesis #f 24 25 24)
    ("    " white-space #f 25 29 25)
    ("╠═╬═╣" parenthesis #f 29 34 29)
    ("    " white-space #f 34 38 34)
    ("║" parenthesis #f 38 39 38)
    ("3" constant #f 39 40 39)
    ("║" parenthesis #f 40 41 40)
    ("4" constant #f 41 42 41)
    ("║" parenthesis #f 42 43 42)
    ("    " white-space #f 43 47 43) ("╚═╩═╝" parenthesis #f 47 52 47)
    ("\n" white-space #f 52 53 0)
    (,eof eof #f #f #f 0)))

(define-values (dont-care dont-care?)
  (let ()
    (struct dont-care ())
    (values (dont-care) dont-care?)))

(define (equal?/dont-care x y)
  (let loop ([x x][y y])
    (cond
      [(or (dont-care? x) (dont-care? y))
       #t]
      [(and (pair? x) (pair? y))
       (and (loop (car x) (car y))
            (loop (cdr x) (cdr y)))]
      [else (equal? x y)])))

(check-pred
 (λ (x) 
   (equal?/dont-care 
    x
    `(("#2d" hash-colon-keyword #f 1 4 0)
      ("\n" white-space #f 4 5 4)
      ("╔═════╦═══════╗" parenthesis #f 5 20 5)
      (" " white-space #f 20 21 20)
      ("║" parenthesis #f 21 22 21)
      ("@" parenthesis #f 22 23 22)
      ("f" symbol #f 23 24 23)
      ("{" parenthesis |{| 24 25 24)
      (,dont-care text #f 25 26 25)
      ("}" parenthesis |}| 26 27 26)
      ("║" parenthesis #f 27 28 27)
      (" " white-space #f 28 29 28)
      ("@" parenthesis #f 29 30 29)
      ("g" symbol #f 30 31 30)
      ("{" parenthesis |{| 31 32 31)
      (,dont-care text #f 32 33 32)
      ("}" parenthesis |}| 33 34 33)
      (" " white-space #f 34 35 34)
      ("║" parenthesis #f 35 36 35)
      (" " white-space #f 36 37 36)
      ("╠═════╬═══════╣" parenthesis #f 37 52 37)
      (" " white-space #f 52 53 52)
      ("║" parenthesis #f 53 54 53)
      ("@" parenthesis #f 54 55 54)
      ("h" symbol #f 55 56 55)
      ("{" parenthesis |{| 56 57 56)
      (,dont-care text #f 57 58 57)
      ("}" parenthesis |}| 58 59 58)
      ("║" parenthesis #f 59 60 59)
      (" " white-space #f 60 61 60)
      ("@" parenthesis #f 61 62 61)
      ("i" symbol #f 62 63 62)
      ("{" parenthesis |{| 63 64 63)
      (,dont-care text #f 64 65 64)
      ("}" parenthesis |}| 65 66 65)
      (" " white-space #f 66 67 66)
      ("║" parenthesis #f 67 68 67)
      (" " white-space #f 68 69 68)
      ("╚═════╩═══════╝" parenthesis #f 69 84 69)
      ("\n" white-space #f 84 85 0)
      (,eof eof #f 85 85 0))))
 (run-lexer #:sub-lexer scribble-lexer
            "#2d\n"
            "╔═════╦═══════╗\n"
            "║@f{x}║ @g{y} ║\n"
            "╠═════╬═══════╣\n"
            "║@h{z}║ @i{w} ║\n"
            "╚═════╩═══════╝\n"))

(check-equal?
 (run-lexer "#2" 'not-a-char)
 `(("#2" error #f 1 3 0) 
   ("" no-color #f 3 4 0) 
   (,eof eof #f #f #f 0)))

(check-equal?
 (run-lexer "#2d\n" 'not-a-char)
 `(("#2d" hash-colon-keyword #f 1 4 0)
   ("\n" white-space #f 4 5 4)
   (" " error #f 5 6 5)
   (,eof eof #f #f #f 0)))

(check-equal?
 (run-lexer "#2d\n╔" 'not-a-char)
 `(("#2d" hash-colon-keyword #f 1 4 0)
   ("\n" white-space #f 4 5 4)
   ("╔" no-color #f 5 6 5)
   (" " error #f 6 7 6)
   (,eof eof #f #f #f 0)))


(check-equal?
 (run-lexer "#2dsomething\n"
            "╔═══╗\n"
            "║ " 'special " ║\n"
            "╚═══╝")
 `(("#2dsomething" hash-colon-keyword #f 1 13 0) 
   ("\n" white-space #f 13 14 13) 
   ("╔═══╗\n║ " no-color #f 14 22 14)
   ("  ║\n╚═══╝" error #f 22 31 22)
   (,eof eof #f #f #f 0)))

(check-equal?
 (run-lexer "#2dsomething\n"
            "╔═══╗\n"
            'special
             "   ║\n"
            "╚═══╝")
 `(("#2dsomething" hash-colon-keyword #f 1 13 0)
   ("\n" white-space #f 13 14 13)
   ("╔═══╗\n" no-color #f 14 20 14)
   ("    ║\n╚═══╝" error #f 20 31 20)
   (,eof eof #f #f #f 0)))

