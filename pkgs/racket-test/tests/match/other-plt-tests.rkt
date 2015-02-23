(module other-plt-tests mzscheme
  
  (require rackunit net/uri-codec mzlib/pregexp mzlib/plt-match
           mzlib/list mzlib/etc)
  
  (define-struct shape (color))
  (define-struct (ovoid shape) (x-diam y-diam))
  (define-struct (circle ovoid) (radius))
  (define-struct (rectangle shape) (height width))
  (define-struct (square rectangle) (width))
  (define c (make-circle 5 4 3 2))
  
  (define (set-equal? set-1 set-2)
    (null? (set-diff set-1 set-2)))
  (define (set-diff set-1 set-2)
    (filter (lambda (a) (if (member a set-2)
                            #f
                            a)) set-1) )
  
  
  
  (provide other-plt-tests)
  
  (define-syntax (mytest stx)
    (syntax-case stx ()
      [(mytest tst exp)
       #`(test-case (format "test: ~a" (syntax-object->datum (quote-syntax tst)))
                         #,(syntax/loc stx (check-equal? tst exp)))]))
  
  (define-syntax mytest-no-order
    (syntax-rules ()
      [(mytest tst exp)
       (test-case (format "no-order test: ~a" (syntax-object->datum (quote-syntax tst)))
                       (check set-equal? tst exp))]))
  
  (define other-plt-tests 
    (test-suite 
     "Tests copied from plt-match-test.rkt"
     
     (mytest (match "hello"
               ((pregexp (pregexp "hello")) #t)
               (else #f))
             #t)
     
     (mytest (match 123
               ((pregexp "123") #t)
               (else #f))
             #f)
     (mytest (match 123
               ((regexp "123") #t)
               (else #f))
             #f)
     (mytest (match 123
               ((pregexp "123" (list a ...)) #t)
               (else #f))
             #f)
     (mytest (match 123
               ((regexp "123" (list a ...)) #t)
               (else #f))
             #f)
     
     (mytest (match "hello"
               ((regexp "hello") #t)
               (else #f))
             #t)
     
     (mytest (match "frank"
               ((regexp "hello") #t)
               ((regexp "frank") 2)
               (else #f))
             2)
     
     (mytest (match "frank"
               ((pregexp "hello") #t)
               ((pregexp "frank") 2)
               (else #f))
             2)
     
     (mytest (match "frank"
               ((regexp "hello") #t)
               (else #f))
             #f)
     
     (mytest (match "hello"
               ((regexp "(hel)lo" (list whol a rest ...)) a)
               (else #f))
             "hel")
     
     (mytest (match "hello"
               ((pregexp "(hel)lo" (list whole a rest ...)) a)
               (else #f))
             "hel")
     
     (mytest (match-let*
                 ((a 1)
                  (c 2)
                  (d 3))
               (list a c d))
             '(1 2 3))
     
     
     (mytest (match (list (cons "a" "b"))
               [(list) ""]
               [(list (list-rest name value))
                (string-append name
                               "="
                               value)]
               [(list-rest (list-rest name value) rest)
                (string-append name
                               "="
                               value
                               "&"
                               (alist->form-urlencoded rest))])
             "a=b")
     
     (mytest (match '(case->)
               [(list 'case-> types ...) 1]
               [(list '->) 2]
               [(list '-> types ...) 3]
               [else 4])
             1)
     
     (mytest (match '(->)
               [(list 'case-> types ...) 1]
               [(list '->) 2]
               [(list '-> types ...) 3]
               [else 4])
             2)
     
     (mytest (match '(-> a b)
               [(list 'case-> types ...) 1]
               [(list '->) 2]
               [(list '-> types ...) 3]
               [else 4])
             3)
     
     (mytest (match 'x
               [(list 'case-> types ...) 1]
               [(list '->) 2]
               [(list '-> types ...) 3]
               [else 4])
             4)
     
     (mytest (match '((r a)) 
               ((list (and (list 'a) (list 'b)) ...) 1)
               ((list (list 'r 'a) ...) 2)
               )
             2)
     
     (mytest (match '((r a)) 
               ((list (and (list 'a) (list 'b)) ... g ...) 1) ;; everything gets matched to g
               ((list (list 'r 'a) ...) 2)
               )
             1)
             
     
     (mytest (match '((r a)) 
               ((vector (and (list 'a) (list 'b)) ...) 1)
               ((list (list 'r 'a) ...) 2)
               )
             2)
     
     (mytest (match '((r a)) 
               ((vector (and (list 'a) (list 'b)) ... g ...) 1)
               ((list (list 'r 'a) ...) 2)
               )
             2)
     
     (mytest (match '((a 1) (b 1)) ((list (or (list 'a x) (list 'b x)) ..1) #t)) #t)
     (mytest (match '((a 1) (b 1)) ((list (or (list 'a x) (list 'b x)) ..1 g ...) #t)) #t)
     (mytest (match (vector '(a 1) '(b 1)) ((vector (or (list 'a x) (list 'b x)) ..1) #t)) #t)
     (mytest (match (vector '(a 1) '(b 1)) ((vector (or (list 'a x) (list 'b x)) ..1 g ...) #t)) #t)
     
     (mytest (match '(1 2 (3 4 5 6))
               ((list 1 2 (or (list 3 4 5 3)
                              (list 7 8 9 0))) 1)
               ((list 1 2 (list 3 4 5 a)) 2))
             2)
     
     
     
     ;; list-no-oreder tests
     
     
     (mytest-no-order (match '(1 2 3 4 5 6 7)
                        ((list-no-order 5 4 2 3 1 c b) (list c b)))
                      '(6 7))
     
     (mytest-no-order (match '(1 2 3 4 5 6 7)
                        ((list-no-order 5 a 2 b 1 c 7) (list a c b)))
                      '(3 4 6))
     
     (mytest-no-order (match '(1 2 3 4 5 6 7)
                        ((list-no-order 5 4 2 3 1 g ...) g))
                      '(6 7))
     
     (mytest-no-order (match '(3 2 3 4 3 6 3)
                        ((list-no-order a b c 3 ...) (list a b c)))
                      '(2 4 6))
     
     (mytest-no-order (match '(3 3 3 3 3 2 4 6)
                        ((list-no-order a b c 3 ...) (list a b c)))
                      '(2 4 6))
     
     (mytest (match '(3 2 3 4 3 6 3)
               ((list-no-order a b c ...) #t))
             #t)
     
     (mytest-no-order 
      (match '(3 s 4 g 5 e s 7)
        ((list-no-order (and (? number?) a)
                        (and (? number?) b)
                        (and (? number?) c)
                        (and (? number?) d)
                        (and (? symbol?) e)
                        f ...) (list a b c d)))
      '(4 5 3 7))
     
     (mytest-no-order 
      (match '(3 s 4 g 5 e s 7)
        ((list-no-order (and (? number?) a)
                        (and (? symbol?) b)
                        (and (? symbol?) c)
                        (and (? symbol?) d)
                        (and (? symbol?) e)
                        f ...) (list b c d e)))
      '(s g e s))
     
     (mytest-no-order 
      (match '("yes" a y 3 "no" e s "maybe")
        ((list-no-order (and (? number?) a)
                        (and (? symbol?) b)
                        (and (? symbol?) c)
                        (and (? symbol?) d)
                        (and (? symbol?) e)
                        f ...) f))
      '("yes" "no" "maybe"))
     
     (mytest
      (match '()
        ((list-no-order) 5))
      5)
     
     
     
     (mytest (match (hash-table ('a 1) ('b 2) ('c 3))
               ((hash-table ('a a) b ('c c)) (list a b c)))
             '(1 (b 2) 3))
     
     (mytest (match (hash-table ('a 1) ('b 2) ('c 3))
               ((hash-table ('c c) ('a a) ('b b)) (list a b c )))
             '(1 2 3))
     
     (mytest (match (hash-table ('a 1) ('b 2) ('c 3))
               ((hash-table (a 3) (b 1) (c 2)) (list a b c)))
             '(c a b))
     
     (mytest (match (hash-table ('a 1) ('b 2) ('c 3))
               ((hash-table (c 3) ('b b) (a 1)) (list a b c)))
             '(a 2 c))
     
     (mytest (match (hash-table ('a 1) ('b 2) ('c 3) ('d 1) ('e 1) ('f 1))
               ((hash-table (c 3) ('b b) (a 1) ...) (list b c)))
             '(2 c))
     
     (mytest-no-order (match (hash-table ('a 1) ('b 2) ('c 3) ('d 1) ('e 1) ('f 1))
                        ((hash-table (c 3) ('b b) (a 1) ...)  a))
                      '(a d e f))
     
     (mytest (match (hash-table ('a 1) ('b 2) ('c 3) ('d 1) ('e 1) ('f 1))
               ((hash-table (c 3) ('b b) (a 1) ..4) (list b c)))
             '(2 c))
     
     (mytest-no-order (match (hash-table ('a 1) ('b 2) ('c 3) ('d 1) ('e 1) ('f 1))
                        ((hash-table (c 3) ('b b) (a 1) ..4)  a))
                      '(a d e f))
     
     (mytest (match (hash-table ('a 1) ('b 2) ('c 3) ('d 1) ('e 1) ('f 1))
               ((hash-table (c 3) ('b b) (a 1) ..5) (list b c))
               (_ #f))
             #f)
     
     (mytest (match (hash-table ('a 1) ('b 2) ('c "hello") ('d 1) ('e 1) ('f 1))
               ((hash-table (a1 a2) (a (? number?)) ...) (list a1 a2)))
             '(c "hello"))
     
     (mytest-no-order (match (hash-table ('a 1) ('b 2) ('c "hello") ('d 1) ('e 1) ('f 1))
                        ((hash-table (a1 a2) (a (? number?)) ...)  a))
                      '(a b d e f))
     
     (mytest (match (hash-table ('a "hey") ('b "ma") ('c "hello") ('d "sup") ('e "down") ('f "dat"))
               ((hash-table (a1 a2) (a (and (not "hello") b)) ...) (list a1 a2)))
             '(c "hello"))
     
     (mytest-no-order (match (hash-table ('a "hey") ('b "ma") ('c "hello") ('d "sup") ('e "down") ('f "dat"))
                        ((hash-table (a1 a2) (a (and (not "hello") b)) ...) a))
                      '(a b d e f))
     
     (mytest-no-order (match (hash-table ('a "hey") ('b "ma") ('c "hello") ('d "sup") ('e "down") ('f "dat"))
                        ((hash-table (a1 a2) (a (and (not "hello") b)) ...) b))
                      '("sup" "hey" "ma" "dat" "down"))
     
     
     (mytest-no-order (match (hash-table ('a "hey") ('b "ma") ('c "hello") ('d "sup") ('e "down") ('f "dat"))
                        ((hash-table (k v)  ...) k))
                      '(d a b f e c))
     
     (mytest-no-order (match (hash-table ('a "hey") ('b "ma") ('c "hello") ('d "sup") ('e "down") ('f "dat"))
                        ((hash-table (k v)  ...) v))
                      '("sup" "hey" "ma" "dat" "down" "hello"))
     
     (mytest (match (hash-table ('a "hey") ('b "ma") ('c "hello") ('d "sup") ('e "down") ('f "dat"))
               ((hash-table ('e v1) ('b v2) (_ _)  ...) (list v1 v2)))
             '("down" "ma"))
     
     (mytest-no-order (match (hash-table ('a "hey") ('b "ma") ('c "hello") ('d "sup") ('e "down") ('f "dat"))
                        ((hash-table ('e v1) ('b v2) rest  ...) rest))
                      '((d "sup") (a "hey") (f "dat") (c "hello")))
     
     (mytest (match (hash-table)
               ((hash-table) 5))
             5)
     
     ; These cases work but I need a better way of testing them.
     ; (mytest (match (hash-table ('a "hey") ('b "sup") ('c "sup") ('d "sup") ('e "down") ('f "dat"))
     ;                ((hash-table (k "sup") (k2 "sup") (k3 "dat") rest ...) (list k k2 k3)))
     ;         '(c b f))
     
     ; (mytest-no-order (match (hash-table ('a "hey") ('b "sup") ('c "sup") ('d "sup") ('e "down") ('f "dat"))
     ;                         ((hash-table (k "sup") (k2 "sup") (k3 "dat") rest ...) rest))
     ;                  '((d "sup") (a "hey") (e "down")))
     
     
     
     (mytest (match (list c 5)
               ((list (struct shape (a)) 2) 1)
               ((list (struct ovoid (a b c)) 2) 2)
               ((list (struct circle (a b c d)) 5) 3))
             3)
     
     (mytest (match (list c 5)
               ((list (struct shape (a)) 2) 1)
               ((list (struct ovoid (a b c)) 5) 2)
               ((list (struct circle (a b c d)) 5) 3))
             2)
     
     (mytest (match (list c 5)
               ((list (struct shape (a)) 5) 1)
               ((list (struct ovoid (a b c)) 5) 2)
               ((list (struct circle (a b c d)) 5) 3))
             1)
     
     (mytest (match (list c 6)
               ((list (struct shape (a)) 5) 1)
               ((list (struct ovoid (a b c)) 5) 2)
               ((list (struct circle (a b c d)) 5) 3)
               ((list (struct shape (a)) 6) 4))
             4)
     
     (mytest (match (list c 5)
               ((list (struct rectangle (a b c)) 5) 0)
               ((list (struct shape (a)) 2) 1)
               ((list (struct ovoid (a b c)) 2) 2)
               ((list (struct circle (a b c d)) 5) 3))
             3)
     
     (mytest (match (list c 5)
               ((list (struct rectangle (a b c)) 5) 0)
               ((list (struct shape (a)) 2) 1)
               ((list (struct ovoid (a b c)) 5) 2)
               ((list (struct circle (a b c d)) 5) 3))
             2)
     
     (mytest (match (list c 5)
               ((list (struct rectangle (a b c)) 5) 0)
               ((list (struct shape (a)) 5) 1)
               ((list (struct ovoid (a b c)) 5) 2)
               ((list (struct circle (a b c d)) 5) 3))
             1)
     
     (mytest (match (list c 5)
               ((list (struct square (a b c d)) 5) 0)
               ((list (struct shape (a)) 2) 1)
               ((list (struct ovoid (a b c)) 2) 2)
               ((list (struct circle (a b c d)) 5) 3))
             3)
     
     (mytest (match (list c 5)
               ((list (struct square (a b c d)) 5) 0)
               ((list (struct shape (a)) 2) 1)
               ((list (struct ovoid (a b c)) 5) 2)
               ((list (struct circle (a b c d)) 5) 3))
             2)
     
     (mytest (match (list c 5)
               ((list (struct square (a b c d)) 5) 0)
               ((list (struct shape (a)) 5) 1)
               ((list (struct ovoid (a b c)) 5) 2)
               ((list (struct circle (a b c d)) 5) 3))
             1)
     
     
     ;;quasi-quote tests 
     
     (mytest (match '(a ()) (`(a ()) #t))
             #t)
     
     (mytest (match '(1 2 3 4 . 5)
               (`(1 2 ,@(list 3 4) . ,b) b))
             5)
     
     (mytest (match '(1 2 3 4 . b)
               (`(,b 2 ,@(list 3 4) . b) b))
             1)
     
     (mytest (match '(1 2 3 4)
               (`(,@`(,x ,y) ,@`(,a ,b)) (list x y a b)))
             '(1 2 3 4))
     
     (mytest (match '(1 2 3)
               (`(,a ,b ,c) (list a b c)))
             '(1 2 3))
     
     ;(unquote-splicing (list a b c))
     
     (mytest (match '(c a b 1 2 3 r f i) 
               (`(c a b ,@(list a b c) r f i) (list a b c)))
             '(1 2 3))
     
     (mytest (match '(c a b 1 2 3 r f i) 
               (`(c a b (unquote-splicing (list a b c)) r f i) (list a b c)))
             '(1 2 3))
     
     (mytest (match '(3 4 #\c a b 1 (2 (c d)))
               (`(3 4 #\c a b ,a ,(list b `(c e))) 'not-good)
               (`(3 4 #\c a b ,a ,(list b `(c d))) (list a b)))
             '(1 2))
     
     (mytest (match #(x 2 x)        ;remember that the x's are symbols here
               (`#(x ,x x) (list x)))
             '(2))
     
     (mytest (match #(c a b 1 2 3 r f i) 
               (`#(c a b ,@(list derby b c) r f i) (list derby b c)))
             '(1 2 3))
     
     (mytest (match #&(c a b 1 2 3 r f i) 
               (`#&(c a b ,@(list a b c) r f i) (list a b c)))
             '(1 2 3))
     
     (mytest (match (list 
                     "hi" 
                     1 
                     'there 
                     #\c 
                     #t 
                     #f 
                     '(a b c) 
                     '(a b . c) 
                     '(a b c c c c) 
                     #(a b c) 
                     #(a b c c c c) 
                     #&(a b c) 
                     '(1 2 3) 
                     '(4 5 . 6) 
                     '(7 8 9) 
                     #(10 11 12)
                     #&(13 14 15 16) 
                     1 
                     2 
                     3 
                     4 
                     17
                     )
               (`(
                  "hi" 
                  1 
                  there 
                  #\c 
                  #t 
                  #f 
                  (a b c) 
                  (a b . c) 
                  (a b c ..2) 
                  #(a b c) 
                  #(a b c ..2) 
                  #&(a b c) 
                  ,(list a b c) 
                  ,(list-rest c1 d  e) 
                  ,(list f g h ...) 
                  ,(vector i j k)
                  ,(box (list l m n o)) 
                  ,@(list 1 2 3 4 p)
                  )
                 (list 
                  a b c 
                  c1 d e 
                  f g h 
                  i j k 
                  l m n o 
                  p
                  )))
             '(1 2 3 4 5 6 7 8 (9) 10 11 12 13 14 15 16 17))
     
     (mytest (match (vector 
                     "hi" 
                     1 
                     'there 
                     #\c 
                     #t 
                     #f 
                     '(a b c) 
                     '(a b . c) 
                     '(a b c c c c) 
                     #(a b c) 
                     #(a b c c c c) 
                     #&(a b c) 
                     '(1 2 3) 
                     '(4 5 . 6) 
                     '(7 8 9) 
                     #(10 11 12)
                     #&(13 14 15 16) 
                     1 
                     2 
                     3 
                     4 
                     17
                     )
               (`#(
                   "hi" 
                   1 
                   there 
                   #\c 
                   #t 
                   #f 
                   (a b c) 
                   (a b . c) 
                   (a b c ..2) 
                   #(a b c) 
                   #(a b c ..2) 
                   #&(a b c) 
                   ,(list a b c) 
                   ,(list-rest c1 d e) 
                   ,(list f g h ...) 
                   ,(vector i j k)
                   ,(box (list l m n o))
                   ,@(list 1 2 3 4 p)
                   )
                 (list 
                  a b c 
                  c1 d e 
                  f g h 
                  i j k 
                  l m n o 
                  p
                  )))
             '(1 2 3 4 5 6 7 8 (9) 10 11 12 13 14 15 16 17))
     
     (mytest (match (box (list 
                          "hi" 
                          1 
                          'there 
                          #\c 
                          #t 
                          #f 
                          '(a b c) 
                          '(a b . c) 
                          '(a b c c c c) 
                          #(a b c) 
                          #(a b c c c c) 
                          #&(a b c) 
                          '(1 2 3) 
                          '(4 5 . 6) 
                          '(7 8 9) 
                          #(10 11 12)
                          #&(13 14 15 16) 
                          1 
                          2 
                          3 
                          4 
                          17
                          ))
               (`#&(
                    "hi" 
                    1 
                    there 
                    #\c 
                    #t 
                    #f 
                    (a b c) 
                    (a b . c) 
                    (a b c ..2) 
                    #(a b c) 
                    #(a b c ..2) 
                    #&(a b c) 
                    ,(list a b c) 
                    ,(list-rest c1 d e) 
                    ,(list f g h ...) 
                    ,(vector i j k)
                    ,(box (list l m n o)) 
                    ,@(list 1 2 3 4 p)
                    )
                 (list 
                  a b c 
                  c1 d e 
                  f g h 
                  i j k 
                  l m n o 
                  p
                  )))
             '(1 2 3 4 5 6 7 8 (9) 10 11 12 13 14 15 16 17))
     
     
     ;; hand made match tests
     
     (mytest
      (letrec ((z
                (lambda (x)
                  (match
                      x
                    ((list a b c)
                     (if (= a 10)
                         (list a b c)
                         (begin (cons a (z (list (add1 a) 2 3))))))))))
        (z '(1 2 3)))
      '(1 2 3 4 5 6 7 8 9 10 2 3))
     (mytest
      (letrec ((z
                (match-lambda
                  ((list a b c)
                   (if (= a 10) (list a b c) (cons a (z (list (add1 a) 2 3))))))))
        (z '(1 2 3)))
      '(1 2 3 4 5 6 7 8 9 10 2 3))
     (mytest
      (letrec ((z
                (match-lambda*
                  ((list a b c)
                   (if (= a 10) (list a b c) (cons a (z (add1 a) 2 3)))))))
        (z 1 2 3))
      '(1 2 3 4 5 6 7 8 9 10 2 3))
     (mytest
      (match-let
          (((list a b c) '(1 2 3)) ((list d e f) '(4 5 6)))
        (list a b c d e f))
      '(1 2 3 4 5 6))
     (mytest
      (match-let
          hey
        (((list a b c) '(1 2 3)) ((list d e f) '(4 5 6)))
        (list a b c d e f))
      '(1 2 3 4 5 6))
     (mytest
      (match-let
          hey
        (((list a b c) '(1 2 3)) ((list d e f) '(4 5 6)))
        (if (= a 10) '() (cons a (hey (list (add1 a) b c) '(d e f)))))
      '(1 2 3 4 5 6 7 8 9))
     (mytest
      (let ((f 7))
        (match-let
            (((list a b c) (list 1 2 f)) ((list d e) '(5 6)))
          (list a d c f)))
      '(1 5 7 7))
     (mytest
      (match-let*
          (((list a b c) '(1 2 3)) ((list d e f) '(4 5 6)))
        (list a b c d e f))
      '(1 2 3 4 5 6))
     (mytest
      (match-let*
          (((list a b c) '(1 2 3)) ((list d e f) (list a b c)))
        (list d e f))
      '(1 2 3))
     (mytest
      (let ((f 7))
        (match-let*
            (((list a b c) (list 1 2 f)) ((list d e) '(5 6)))
          (list a d c f)))
      '(1 5 7 7))
     (mytest
      (match-letrec
          (((list a b c) '(1 2 3)) ((list d e f) '(4 5 6)))
        (list a b c d e f))
      '(1 2 3 4 5 6))
     (mytest
      (match-letrec
          (((list a b)
            (list
             (lambda (x) (if (zero? x) '() (cons x (a (sub1 x)))))
             (lambda (x) (if (= x 10) '() (cons x (b (add1 x))))))))
        (a 10))
      '(10 9 8 7 6 5 4 3 2 1))
     (mytest
      (match-letrec
          (((list a b)
            (list
             (lambda (x) (if (zero? x) '() (cons (b x) (a (sub1 x)))))
             (lambda (x) (if (= x 10) '() (cons x (b (add1 x))))))))
        (a 10))
      '(()
        (9)
        (8 9)
        (7 8 9)
        (6 7 8 9)
        (5 6 7 8 9)
        (4 5 6 7 8 9)
        (3 4 5 6 7 8 9)
        (2 3 4 5 6 7 8 9)
        (1 2 3 4 5 6 7 8 9)))
     (mytest
      (let ((f 7))
        (match-letrec
            (((list a b c) (list 1 2 f)) ((list d e) '(5 6)))
          (list a d c f)))
      '(1 5 7 7))
     (mytest
      (let ((f 7)) ((match-lambda ((list a b) (list a b f))) '(4 5)))
      '(4 5 7))
     (mytest (let ((f 7)) ((match-lambda* ((list a b) (list a b f))) 4 5)) '(4 5 7))
     (mytest
      (let ((f 7)) (match-define (list a b c) (list 1 2 f)) (list a b c f))
      '(1 2 7 7))
     (test-case "match-define"
                     (let ()
                       (match-define
                        (list a b)
                        (list
                         (lambda (x) (if (zero? x) '() (cons (b x) (a (sub1 x)))))
                         (lambda (x) (if (= x 10) '() (cons x (b (add1 x)))))))
                       (check-equal?
                        (a 10)
                        '(()
                          (9)
                          (8 9)
                          (7 8 9)
                          (6 7 8 9)
                          (5 6 7 8 9)
                          (4 5 6 7 8 9)
                          (3 4 5 6 7 8 9)
                          (2 3 4 5 6 7 8 9)
                          (1 2 3 4 5 6 7 8 9)))))
     
     (mytest (match '((1) (2) (3)) ((list (list _) ...) 'hey)) 'hey)
     (mytest
      (match
          '(1 2 3)
        ((list a b c) (=> fail) (if (= a 1) (fail) 'bad))
        ((list a b c) (=> fail) (if (= a 1) (fail) 'bad))
        ((list a b c) (=> fail) (if (= a 1) (fail) 'bad))
        ((list a b c) (=> fail) (if (= a 1) (fail) 'bad))
        ((list a b c) (=> fail) (if (= a 1) (fail) 'bad))
        ((list a b c) (=> fail) (if (= a 1) (fail) 'bad))
        ((list a b c) (list a b c)))
      '(1 2 3))
     #|
     (mytest
      (let ((x '(1 2 (3 4))))
        (match x ((list _ _ (list (set! set-it) _)) (set-it 17)))
        x)
      '(1 2 (17 4)))
     (mytest
      (let ((x '(1 2 (3 4))))
        (match x ((list _ _ (list _ (set! set-it))) (set-it 17)))
        x)
      '(1 2 (3 17)))
     (mytest
      (let ((x '(1 2 (3 4))))
        (match x ((list (set! set-it) _ (list _ _)) (set-it 17)))
        x)
      '(17 2 (3 4)))
     (mytest
      (let ((x '(1 2 (3 4))))
        (match x ((list _ (set! set-it) (list _ _)) (set-it 17)))
        x)
      '(1 17 (3 4)))
     (mytest
      (let ((x '(1 2 (3 . 4) . 5)))
        (match x ((list-rest (set! set-it) _ (list-rest _ _) _) (set-it 17)))
        x)
      '(17 2 (3 . 4) . 5))
     (mytest
      (let ((x '(1 2 (3 . 4) . 5)))
        (match x ((list-rest _ (set! set-it) (list-rest _ _) _) (set-it 17)))
        x)
      '(1 17 (3 . 4) . 5))
     (mytest
      (let ((x '(1 2 (3 . 4) . 5)))
        (match x ((list-rest _ _ (list-rest (set! set-it) _) _) (set-it 17)))
        x)
      '(1 2 (17 . 4) . 5))
     (mytest
      (let ((x '(1 2 (3 . 4) . 5)))
        (match x ((list-rest _ _ (list-rest _ (set! set-it)) _) (set-it 17)))
        x)
      '(1 2 (3 . 17) . 5))
     (mytest
      (let ((x '(1 2 (3 . 4) . 5)))
        (match x ((list-rest _ _ (list-rest _ _) (set! set-it)) (set-it 17)))
        x)
      '(1 2 (3 . 4) . 17))
|#
     #;(mytest
      (let ((x (vector 1 2))) (match x ((vector _ (set! set-it)) (set-it 17))) x)
      #2(1 17))
     #;(mytest
      (let ((x (vector 1 2))) (match x ((vector (set! set-it) _) (set-it 17))) x)
      #2(17 2))
     #;
     (mytest (let ((x (box 1))) (match x ((box (set! set-it)) (set-it 17))) x) #&17)
     #;
     (mytest
      (let ((x (vector 1 2))) (match x ((box (list _ (set! set-it))) (set-it 17))) x)
      #&(1 17))
     #;
     (mytest
      (let ((x (box (vector 1 2))))
        (match x ((box (vector _ (set! set-it))) (set-it 17)))
        x)
      #&#2(1 17))
     #|
     (mytest
      (let* ((x '(1 2 (3 4)))
             (f (match x ((list _ _ (list (get! get-it) _)) get-it))))
        (match x ((list _ _ (list (set! set-it) _)) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x '(1 2 (3 4)))
             (f (match x ((list _ _ (list _ (get! get-it))) get-it))))
        (match x ((list _ _ (list _ (set! set-it))) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x '(1 2 (3 4)))
             (f (match x ((list (get! get-it) _ (list _ _)) get-it))))
        (match x ((list (set! set-it) _ (list _ _)) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x '(1 2 (3 4)))
             (f (match x ((list _ (get! get-it) (list _ _)) get-it))))
        (match x ((list _ (set! set-it) (list _ _)) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x '(1 2 (3 . 4) . 5))
             (f (match x ((list-rest (get! get-it) _ (list-rest _ _) _) get-it))))
        (match x ((list-rest (set! set-it) _ (list-rest _ _) _) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x '(1 2 (3 . 4) . 5))
             (f (match x ((list-rest _ (get! get-it) (list-rest _ _) _) get-it))))
        (match x ((list-rest _ (set! set-it) (list-rest _ _) _) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x '(1 2 (3 . 4) . 5))
             (f (match x ((list-rest _ _ (list-rest (get! get-it) _) _) get-it))))
        (match x ((list-rest _ _ (list-rest (set! set-it) _) _) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x '(1 2 (3 . 4) . 5))
             (f (match x ((list-rest _ _ (list-rest _ (get! get-it)) _) get-it))))
        (match x ((list-rest _ _ (list-rest _ (set! set-it)) _) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x '(1 2 (3 . 4) . 5))
             (f (match x ((list-rest _ _ (list-rest _ _) (get! get-it)) get-it))))
        (match x ((list-rest _ _ (list-rest _ _) (set! set-it)) (set-it 17)))
        (f))
      17)

     
     (mytest
      (let* ((x (vector 1 2)) (f (match x ((vector _ (get! get-it)) get-it))))
        (match x ((vector _ (set! set-it)) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x (vector 1 2)) (f (match x ((vector (get! get-it) _) get-it))))
        (match x ((vector (set! set-it) _) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x (box 1)) (f (match x ((box (get! get-it)) get-it))))
        (match x ((box (set! set-it)) (set-it 17)))
        (f))
      17)
     #;
     (mytest
      (let* ((x #&(1 2)) (f (match x ((box (list _ (get! get-it))) get-it))))
        (match x ((box (list _ (set! set-it))) (set-it 17)))
        (f))
      17)
     (mytest
      (let* ((x (box (vector 1 2))) (f (match x ((box (vector _ (get! get-it))) get-it))))
        (match x ((box (vector _ (set! set-it))) (set-it 17)))
        (f))
      17)
|#
     (mytest
      (match
          #2(#3(#3(1 2 3) #3(1 2 3) #3(2 3 4)) #3(#3(1 2 3) #3(1 2 3) #3(2 3 4)))
        ((vector (vector (vector a ...) ...) ...) a))
      '(((1 2 3) (1 2 3) (2 3 4)) ((1 2 3) (1 2 3) (2 3 4))))
     (mytest
      (match
          '(((1 2 3) (1 2 3) (2 3 4)) ((1 2 3) (1 2 3) (2 3 4)))
        ((list (list (list a ...) ...) ...) a))
      '(((1 2 3) (1 2 3) (2 3 4)) ((1 2 3) (1 2 3) (2 3 4))))
     (mytest
      (match
          '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
             ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
            (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
             ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
        ((list (list (list (list (list (list a ...) ...) ...) ...) ...) ...) a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
        (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))
     (mytest
      (match
          #2(#2(#2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))
                #2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))))
             #2(#2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))
                #2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))))
        ((vector
          (vector (vector (vector (vector (vector a ...) ...) ...) ...) ...)
          ...)
         a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
        (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))
     (mytest
      (match
          '(#2((#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))
               (#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))))
            #2((#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))
               (#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))))
        ((list (vector (list (vector (list (vector a ...) ...) ...) ...) ...) ...)
         a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
        (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))
     (mytest
      (match
          '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
             ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
            (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
             ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
        ((list (list (list (list (list (list a ..2) ..2) ..2) ..2) ..2) ..2) a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
        (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))
     (mytest
      (match
          #2(#2(#2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))
                #2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))))
             #2(#2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))
                #2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))))
        ((vector
          (vector (vector (vector (vector (vector a ..2) ..2) ..2) ..2) ..2)
          ..2)
         a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
        (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))
     (mytest
      (match
          '(#2((#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))
               (#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))))
            #2((#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))
               (#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))))
        ((list (vector (list (vector (list (vector a ..2) ..2) ..2) ..2) ..2) ..2)
         a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
        (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
         ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))
     (mytest
      (match
          '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
             ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
            (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
             ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
        ((list (list (list (list (list (list _ ...) ...) ...) ...) ...) ...) #t)
        (_ #f))
      #t)
     (mytest
      (match
          #2(#2(#2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))
                #2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))))
             #2(#2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))
                #2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))))
        ((vector
          (vector (vector (vector (vector (vector _ ...) ...) ...) ...) ...)
          ...)
         #t)
        (_ #f))
      #t)
     (mytest
      (match
          '(#2((#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))
               (#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))))
            #2((#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))
               (#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))))
        ((list (vector (list (vector (list (vector _ ...) ...) ...) ...) ...) ...)
         #t)
        (_ #f))
      #t)
     (mytest
      (match
          '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
             ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
            (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
             ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
        ((list (list (list (list (list (list a b) ...) ...) ...) ...) ...)
         (list a b)))
      '((((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7))))
         ((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))))
        (((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8))))
         ((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))))))
     (mytest
      (match
          #2(#2(#2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))
                #2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))))
             #2(#2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))
                #2(#2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8)))
                   #2(#2(#2(1 2) #2(3 4)) #2(#2(5 6) #2(7 8))))))
        ((vector
          (vector (vector (vector (vector (vector a b) ...) ...) ...) ...)
          ...)
         (list a b)))
      '((((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7))))
         ((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))))
        (((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8))))
         ((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))))))
     (mytest
      (match
          '(#2((#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))
               (#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))))
            #2((#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))
               (#2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8)))
                #2((#2(1 2) #2(3 4)) (#2(5 6) #2(7 8))))))
        ((list (vector (list (vector (list (vector a b) ...) ...) ...) ...) ...)
         (list a b)))
      '((((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7))))
         ((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))))
        (((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8))))
         ((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))))))
     (mytest
      (match
          '((1 1 2 2) (1 1 2 2) 5 5 5)
        ((list (list 1 ... a ...) ... 7 ...) #f)
        ((list (list 1 ... a ...) ... 6 ...) #f)
        ((list (list 1 ... a ...) ... 5 ...) a))
      '((2 2) (2 2)))
     (mytest (match '(1 1 1 1 1 2 2 2 2) ((list 1 ... 2 2 2 2) #t)) #t)
     (mytest (match '(1 1 1 1 1 2 2 2 2) ((list 1 ... 2 ...) #t)) #t)
     (mytest
      (match '(1 1 1 1 1 2 2 2 2) ((list (and (not 2) a) ... 2 ...) a))
      '(1 1 1 1 1))
     (mytest
      (match '(1 1 1 1 1 2 2 2 2) ((list a ... 2 ...) a))
      '(1 1 1 1 1 2 2 2 2))
     (mytest (match '(1 1 1 1 1 2 2 2 2) ((list _ ... 2 ...) #t)) #t)
     (mytest
      (match
          '(pattern matching in scheme is very cool)
        ((list (and (not 'in) a) ... (and (not 'is) b) ... c ...) (list a c b)))
      '((pattern matching) (is very cool) (in scheme)))
     (mytest
      (match '((1 1 2 2) (1 1 2 2) 5 5 5) ((list (list 1 ... 2 ...) ... 5 ...) #t))
      #t)
     (mytest
      (match
          #13(1 3 1 9 8 4 2 2 4 7 a b c)
        ((vector (and (? odd?) a) ... 8 (and (? even?) b) ... 7 r ...)
         (list a b r)))
      '((1 3 1 9) (4 2 2 4) (a b c)))
     (mytest
      (match
          #5(#4(1 1 2) #4(1 1 2) 5)
        ((vector (vector 1 ... 2 ...) ... 5 ...) #t))
      #t)
     (mytest
      (match
          #5(#4(1 1 2) #4(1 1 2) 5)
        ((vector (vector 1 ... a ...) ... 7 ...) #f)
        ((vector (vector 1 ... a ...) ... 6 ...) #f)
        ((vector (vector 1 ... a ...) ... 5 ...) a))
      '((2 2) (2 2)))
     (mytest
      (match #5(#2(1 2) #2(1 2) #2(1 2) 5 6) ((vector (vector _ _) ..3 a ...) a))
      '(5 6))
     (mytest
      (match #5(1 2 3 4 5) ((vector a b (and c (not 5)) ... d) (list a b c d)))
      '(1 2 (3 4) 5))
     
     (mytest (match '(1 2 3 4) ((list-no-order a b 1 c) (list a b c))) '(2 3 4))
     
     ;; ddk-patterns for list-rest
     (mytest (match '(1 1 1 1 . 2)
               ((list-rest 1 ... a) a))
             2)
     
     (mytest (match '(1 1 1 1 . 2)
               ((list-rest (and 1 b) ... a) (list b a)))
             '((1 1 1 1) 2))
     
     (mytest (match '(1 1 1 1 2 . 4)
               ((list-rest (and 1 b) ... a (and c 4)) (list b a c)))
             '((1 1 1 1) 2 4))
     
     ;the new var pattern
     ; this allows one to use 
     ; var, $, =, and, or, not, ?, set!, or get!
     ; as pattern variables
     (mytest (match '(1 2 3)
               ((list (var _) b c) (list _ b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list (var ..3) b c) (list ..3 b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list (var quasiquote) b c) (list quasiquote b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list (var quote) b c) (list quote b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list (var unquote) b c) (list unquote b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list (var unquote-splicing) b c) (list unquote-splicing b c)))
             '(1 2 3))
     
     ;the new var pattern
     ; this allows one to use 
     ; var, $, =, and, or, not, ?, set!, or get!
     ; as pattern variables
     (mytest (match '(1 2 3)
               ((list $ b c) (list $ b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list var b c) (list var b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list  = b c) (list = b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list and b c) (list and b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list or b c) (list or b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list not b c) (list not b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list  ? b c) (list ? b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list set! b c) (list set! b c)))
             '(1 2 3))
     
     (mytest (match '(1 2 3)
               ((list get! b c) (list get! b c)))
             '(1 2 3))
     
     ))
  
  )

