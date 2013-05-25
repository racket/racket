(module other-tests mzscheme
  (require mzlib/match rackunit)
    
  (provide other-tests)

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

  (define other-tests
    (test-suite "Tests copied from match-test.rkt"

(mytest (letrec ((z
                  (lambda (x)
                    (match x
                      ((a b c)
                       (if (= a 10)
                           (list a b c)
                           (begin (cons a (z (list (add1 a) 2 3)))))))))) 
          (z '(1 2 3)))
        '(1 2 3 4 5 6 7 8 9 10 2 3))

; this is the same test for match-lambda

(mytest (letrec ((z (match-lambda ((a b c)
                                   (if (= a 10)
                                       (list a b c)
                                       (cons a (z (list (add1 a) 2 3)))))))) 
          (z '(1 2 3)))
        '(1 2 3 4 5 6 7 8 9 10 2 3))

(mytest (letrec ((z (match-lambda* ((a b c)
                                    (if (= a 10)
                                        (list a b c)
                                        (cons a (z (add1 a) 2 3))))))) 
          (z 1 2 3))
        '(1 2 3 4 5 6 7 8 9 10 2 3))
; matchlet tests

(mytest (match-let (((a b c) '(1 2 3))
                    ((d e f) '(4 5 6)))
                   (list a b c d e f))
        '(1 2 3 4 5 6))


; match: syntax error in (match (hey (((a b c) (d e f)) (list a b c d e f))))
(mytest (match-let hey (((a b c) '(1 2 3))
                        ((d e f) '(4 5 6)))
                   (list a b c d e f))
        '(1 2 3 4 5 6))

(mytest (match-let hey (((a b c) '(1 2 3))
                        ((d e f) '(4 5 6)))
                   (if (= a 10)
                       '()
                       (cons a  (hey (list (add1 a) b c) '(d e f)))))
        '(1 2 3 4 5 6 7 8 9))

(mytest (let ((f 7))
          (match-let ([(a b c) (list 1 2 f)] [(d e) '(5 6)]) (list a d c f)))
        '(1 5 7 7))

; match-let*

(mytest (match-let* (((a b c) '(1 2 3))
                     ((d e f) '(4 5 6)))
                    (list a b c d e f))
        '(1 2 3 4 5 6))

(mytest (match-let* ([(a b c) '(1 2 3)]
                     [(d e f) (list a b c)])
                    (list d e f)) ; should be (1 2 3)
        '(1 2 3))


(mytest (let ((f 7))
          (match-let* ([(a b c) (list 1 2 f)] [(d e) '(5 6)]) (list a d c f)))
        '(1 5 7 7))
; match-letrec

;; let rec does not work this well on chez or plt
;(match-letrec ([(a b) (list (lambda (x) (if (zero? x) '() (cons x (a (sub1 x)))))
;                            (lambda (x) (if (= x 10) '() (cons x (b (add1 x))))))]
;               [(c d) (list (a 10) (b 0))])
;              (list c d))

(mytest (match-letrec (((a b c) '(1 2 3))
                       ((d e f) '(4 5 6)))
                      (list a b c d e f))
        '(1 2 3 4 5 6))

(mytest (match-letrec ([(a b) (list (lambda (x) (if (zero? x) '() (cons x (a (sub1 x)))))
                                    (lambda (x) (if (= x 10) '() (cons x (b (add1 x))))))])
                      (a 10))
        '(10 9 8 7 6 5 4 3 2 1))

(mytest (match-letrec ([(a b) (list (lambda (x) (if (zero? x) '() (cons (b x) (a (sub1 x)))))
                            (lambda (x) (if (= x 10) '() (cons  x (b (add1 x))))))])
                      (a 10))
        '(() (9) (8 9) (7 8 9) (6 7 8 9) (5 6 7 8 9) (4 5 6 7 8 9)
          (3 4 5 6 7 8 9) (2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)))


(mytest (let ((f 7))
          (match-letrec ([(a b c) (list 1 2 f)] [(d e) '(5 6)]) (list a d c f)))
        '(1 5 7 7))


; match-lambda


(mytest (let ((f 7))
          ((match-lambda ((a b) (list a b f))) '(4 5)))
        '(4 5 7))

(mytest (let ((f 7))
          ((match-lambda* ((a b) (list a b f))) 4 5))
        '(4 5 7))

; match-define

(mytest (let ((f 7))
          (match-define (a b c) (list 1 2 f))
          (list a b c f))
        '(1 2 7 7))


(test-case "match-define"
           (let () (match-define (a b) (list (lambda (x) (if (zero? x) '() (cons (b x) (a (sub1 x)))))
                                             (lambda (x) (if (= x 10) '() (cons  x (b (add1 x)))))))
                (check-equal? (a 10)
                              '(() (9) (8 9) (7 8 9) (6 7 8 9) (5 6 7 8 9) (4 5 6 7 8 9) 
                                (3 4 5 6 7 8 9) (2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)))))


; this is some thing that I missed before

(mytest (match '((1) (2) (3)) (((_) ...) 'hey))
        'hey)

; failure tests

(mytest (match '(1 2 3)
          ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
          ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
          ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
          ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
          ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
          ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
          ((a b c)  (list a b c)))
        '(1 2 3))

; (mytest (match '(1 2 3)
;           ((a b c) (=> fail) (if (= a 1) (fail) 'bad)))
;         '()) ; this should through a different exception



; set! tests

; set! for lists
#|
(mytest (let ((x '(1 2 (3 4))))
          (match x
            ((_ _ ((set! set-it) _)) (set-it 17)))
          x)
        '(1 2 (17 4)))

(mytest (let ((x '(1 2 (3 4))))
          (match x
            ((_ _ (_ (set! set-it))) (set-it 17)))
          x)
        '(1 2 (3 17)))

(mytest (let ((x '(1 2 (3 4))))
          (match x
            (((set! set-it) _ (_ _)) (set-it 17)))
          x)
        '(17 2 (3 4)))

(mytest (let ((x '(1 2 (3 4))))
          (match x
            ((_ (set! set-it) (_ _)) (set-it 17)))
          x)
        '(1 17 (3 4)))

;set! for improper lists

(mytest (let ((x '(1 2 (3 . 4) . 5)))
          (match x
            (((set! set-it) _ (_ . _) . _) (set-it 17)))
          x)
        '(17 2 (3 . 4) . 5))

(mytest (let ((x '(1 2 (3 . 4) . 5)))
          (match x
            ((_ (set! set-it) (_ . _) . _) (set-it 17)))
          x)
        '(1 17 (3 . 4) . 5))

(mytest (let ((x '(1 2 (3 . 4) . 5)))
          (match x
            ((_ _ ((set! set-it) . _) . _) (set-it 17)))
          x)
        '(1 2 (17 . 4) . 5))

(mytest (let ((x '(1 2 (3 . 4) . 5)))
          (match x
            ((_ _ (_ . (set! set-it)) . _) (set-it 17)))
          x)
        '(1 2 (3 . 17) . 5))

(mytest (let ((x '(1 2 (3 . 4) . 5)))
          (match x
            ((_ _ (_ . _) . (set! set-it)) (set-it 17)))
          x)
        '(1 2 (3 . 4) . 17))

;; set! for vectors

(mytest (let ((x (vector 1 2)))
          (match x (#(_ (set! set-it)) (set-it 17)))
          x)
        #(1 17))

(mytest (let ((x (vector 1 2)))
          (match x (#((set! set-it) _) (set-it 17)))
          x)
        #(17 2))

;; set! for boxes

(mytest (let ((x (box 1)))
          (match x (#&(set! set-it) (set-it 17)))
          x)
        #&17)
#;
(mytest (let ((x #&(1 2)))
          (match x (#&(_ (set! set-it)) (set-it 17)))
          x)
        #&(1 17))

(mytest (let ((x (box (vector 1 2))))
          (match x (#&#(_ (set! set-it)) (set-it 17)))
          x)
        #&#(1 17))


; get! tests

; get! for lists
#|
(mytest (let* ((x '(1 2 (3 4)))
               (f (match x ((_ _ ((get! get-it) _)) get-it))))
          (match x ((_ _ ((set! set-it) _)) (set-it 17)))
          (f))
        17)

(mytest (let* ((x '(1 2 (3 4)))
               (f (match x ((_ _ (_ (get! get-it))) get-it))))
          (match x ((_ _ (_ (set! set-it))) (set-it 17))) 
          (f))
        17)

(mytest (let* ((x '(1 2 (3 4)))
               (f (match x (((get! get-it) _ (_ _)) get-it))))
          (match x (((set! set-it) _ (_ _)) (set-it 17)))
          (f))
        17)

(mytest (let* ((x '(1 2 (3 4)))
               (f (match x ((_ (get! get-it) (_ _)) get-it))))
          (match x ((_ (set! set-it) (_ _)) (set-it 17)))
          (f))
        17)


;get! for improper lists

(mytest (let* ((x '(1 2 (3 . 4) . 5))
               (f (match x (((get! get-it) _ (_ . _) . _) get-it))))
          (match x (((set! set-it) _ (_ . _) . _) (set-it 17)))
          (f))
        17)

(mytest (let* ((x '(1 2 (3 . 4) . 5))
               (f (match x ((_ (get! get-it) (_ . _) . _) get-it))))
          (match x ((_ (set! set-it) (_ . _) . _) (set-it 17)))
          (f))
        17)

(mytest (let* ((x '(1 2 (3 . 4) . 5))
               (f (match x ((_ _ ((get! get-it) . _) . _) get-it))))
          (match x ((_ _ ((set! set-it) . _) . _) (set-it 17)))
          (f))
        17)

(mytest (let* ((x '(1 2 (3 . 4) . 5))
               (f (match x ((_ _ (_ . (get! get-it)) . _) get-it))))
          (match x ((_ _ (_ . (set! set-it)) . _) (set-it 17)))
          (f))
        17)

(mytest (let* ((x '(1 2 (3 . 4) . 5))
               (f (match x ((_ _ (_ . _) . (get! get-it)) get-it))))
          (match x ((_ _ (_ . _) . (set! set-it)) (set-it 17)))
          (f))
        17)

|#
;; get! for vectors

(mytest (let* ((x (vector 1 2))
               (f (match x (#(_ (get! get-it)) get-it))))
          (match x (#(_ (set! set-it)) (set-it 17)))
          (f))
        17)

(mytest (let* ((x (vector 1 2))
               (f (match x (#((get! get-it) _) get-it))))
          (match x (#((set! set-it) _) (set-it 17)))
          (f))
        17)


;; get! for boxes

(mytest (let* ((x (box 1))
               (f (match x (#&(get! get-it) get-it))))
          (match x (#&(set! set-it) (set-it 17)))
          (f))
        17)

#;
(mytest (let* ((x #&(1 2))
               (f (match x (#&(_ (get! get-it)) get-it))))
          (match x (#&(_ (set! set-it)) (set-it 17)))
          (f))
        17)

(mytest (let* ((x (box (vector 1 2)))
               (f (match x (#&#(_ (get! get-it)) get-it))))
          (match x (#&#(_ (set! set-it)) (set-it 17)))
          (f))
        17)


|#
;; quasi quote tests


(mytest (match '(1 2 3 4 . b)
          (`(,b 2 ,@(3 4) . b) b))
        1)

(mytest (match '(1 2 3 4 . 5)
          (`(1 2 ,@(3 4) . ,b) b))
        5)

(mytest (match '(a ()) (`(a ()) #t))
        #t)

(mytest (match '(1 2 3)
          (`(,a ,b ,c) (list a b c)))
      '(1 2 3))

(mytest (match '(c a b 1 2 3 r f i)
          (`(c a b ,@(a b c) r f i) (list a b c)))
        '(1 2 3))

(mytest (match '(3 4 #\c a b 1 (2 (c d)))
          (`(3 4 #\c a b ,a ,(b `(c e))) 'not-good)
          (`(3 4 #\c a b ,a ,(b `(c d))) (list a b)))
        '(1 2))

(mytest (match #(x 2 x)
          (`#(x ,x x) (list x)))
        '(2))

(mytest (match #(x 2 x)        ;remember that the x's are symbols here
          (`#(x ,x x) (list x)))
        '(2))

(mytest (match #(c a b 1 2 3 r f i)
          (`#(c a b ,@(a b c) r f i) (list a b c)))
        '(1 2 3))

(mytest (match #&(c a b 1 2 3 r f i)
               (`#&(c a b ,@(a b c) r f i) (list a b c)))
        '(1 2 3))

(mytest (match (list "hi" 1 'there #\c #t #f '(a b c) '(a b . c)
                     '(a b c c c c) #(a b c) #(a b c c c c) #&(a b c)
                     '(1 2 3) '(4 5 . 6) '(7 8 9) #(10 11 12) #&(13 14 15 16) 
                     1 2 3 4 17)
          (`("hi" 1 there #\c #t #f (a b c) (a b . c) (a b c ..2) #(a b c)
             #(a b c ..2) #&(a b c) ,(a b c) ,(c1 d . e) ,(f g h ...)
             ,#(i j k) ,#&(l m n o) ,@(1 2 3 4 p))
  (list a b c c1 d e f g h i j k l m n o p)))
        '(1 2 3 4 5 6 7 8 (9) 10 11 12 13 14 15 16 17))

(mytest (match (vector "hi" 1 'there #\c #t #f '(a b c) '(a b . c)
                       '(a b c c c c) #(a b c) #(a b c c c c) #&(a b c)
                       '(1 2 3) '(4 5 . 6) '(7 8 9) #(10 11 12)
                       #&(13 14 15 16) 1 2 3 4 17)
          (`#("hi" 1 there #\c #t #f (a b c) (a b . c) (a b c ..2) #(a b c)
              #(a b c ..2) #&(a b c) ,(a b c) ,(c1 d . e) ,(f g h ...)
              ,#(i j k) ,#&(l m n o) ,@(1 2 3 4 p))
           (list a b c c1 d e f g h i j k l m n o p)))
        '(1 2 3 4 5 6 7 8 (9) 10 11 12 13 14 15 16 17))

(mytest (match (box (list "hi" 1 'there #\c #t #f '(a b c) '(a b . c)
                          '(a b c c c c) #(a b c) #(a b c c c c) #&(a b c)
                          '(1 2 3) '(4 5 . 6) '(7 8 9) #(10 11 12)
                          #&(13 14 15 16) 1 2 3 4 17))
          (`#&("hi" 1 there #\c #t #f (a b c) (a b . c) (a b c ..2) #(a b c)
               #(a b c ..2) #&(a b c) ,(a b c) ,(c1 d . e) ,(f g h ...)
               ,#(i j k) ,#&(l m n o) ,@(1 2 3 4 p))
              (list a b c c1 d e f g h i j k l m n o p)))
        '(1 2 3 4 5 6 7 8 (9) 10 11 12 13 14 15 16 17))

(mytest (match '(1 2 3 4)
          (`(,@`(,x ,y) ,@`(,a ,b)) (list x y a b)))
        '(1 2 3 4))


;; deep nesting

(mytest (match #(#(#(1 2 3) #(1 2 3) #(2 3 4)) #(#(1 2 3) #(1 2 3) #(2 3 4)))
          (#(#(#(a ...) ...) ...) a))
        '(((1 2 3) (1 2 3) (2 3 4)) ((1 2 3) (1 2 3) (2 3 4))))

(mytest (match '(((1 2 3) (1 2 3) (2 3 4)) ((1 2 3) (1 2 3) (2 3 4)))
          ((((a ...) ...) ...) a))
        '(((1 2 3) (1 2 3) (2 3 4)) ((1 2 3) (1 2 3) (2 3 4))))

(mytest (match '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                  ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
                 (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                  ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
          (((((((a ...) ...) ...) ...) ...) ...) a))
        '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
          (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))


(mytest (match #(#(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                   #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8)))))
                 #(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                   #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))))
          (#(#(#(#(#(#(a ...) ...) ...) ...) ...) ...) a))
        '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
          (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))

(mytest (match '(#((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                   (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8)))))
                 #((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                   (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))))
          ((#((#((#(a ...) ...) ...) ...) ...) ...) a))
        '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
          (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))

(mytest (match '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                  ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
                 (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                  ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
          (((((((a ..2) ..2) ..2) ..2) ..2) ..2) a))
        '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
          (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))



(mytest (match #(#(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                   #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8)))))
                 #(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                   #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))))
          (#(#(#(#(#(#(a ..2) ..2) ..2) ..2) ..2) ..2) a))
        '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
          (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))

(mytest (match '(#((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                   (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8)))))
                 #((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                   (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))))
          ((#((#((#(a ..2) ..2) ..2) ..2) ..2) ..2) a))
        '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
          (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
           ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))


(mytest (match '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                  ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
                 (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                  ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
          (((((((_ ...) ...) ...) ...) ...) ...) #t)
          (_ #f))
        #t)

(mytest (match #(#(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                   #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8)))))
                 #(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                   #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))))
          (#(#(#(#(#(#(_ ...) ...) ...) ...) ...) ...) #t)
          (_ #f))
        #t)

(mytest (match '(#((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                   (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8)))))
                 #((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                   (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))))
          ((#((#((#(_ ...) ...) ...) ...) ...) ...) #t)
          (_ #f))
        #t)

(mytest (match '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                  ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
                 (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                  ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
          (((((((a b) ...) ...) ...) ...) ...) (list a b)))
        '((((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))) ((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))))
          (((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))) ((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8))))))) 


(mytest (match #(#(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                   #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8)))))
                 #(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                 #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))))
          (#(#(#(#(#(#(a b) ...) ...) ...) ...) ...) (list a b)))
        '((((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))) ((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))))
          (((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))) ((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))))))

(mytest (match '(#((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                   (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8)))))
                 #((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                   (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))))
          ((#((#((#(a b) ...) ...) ...) ...) ...) (list a b)))
        '((((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))) ((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))))
          (((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))) ((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))))))


;the new var pattern
; this allows one to use
; var, $, =, and, or, not, ?, set!, or get!
; as pattern variables
; (mytest (match '(1 2 3)
;           (((var $) b c) (list $ b c)))
;         '(1 2 3))

; (mytest (match '(1 2 3)
;           (((var var) b c) (list var b c)))
;         '(1 2 3))

; (mytest (match '(1 2 3)
;           (((var =) b c) (list = b c)))
;         '(1 2 3))

; (mytest (match '(1 2 3)
;           (((var and) b c) (list and b c)))
;         '(1 2 3))

; (mytest (match '(1 2 3)
;           (((var or) b c) (list or b c)))
;         '(1 2 3))

; (mytest (match '(1 2 3)
;           (((var not) b c) (list not b c)))
;         '(1 2 3))

; (mytest (match '(1 2 3)
;           (((var ?) b c) (list ? b c)))
;         '(1 2 3))

; (mytest (match '(1 2 3)
;           (((var set!) b c) (list set! b c)))
;         '(1 2 3))

; (mytest (match '(1 2 3)
;           (((var get!) b c) (list get! b c)))
;         '(1 2 3))


(mytest (match '((1 1 2 2) (1 1 2 2) 5 5 5)
          (((1 ... a ...) ... 7 ...) #f)
          (((1 ... a ...) ... 6 ...) #f)
          (((1 ... a ...) ... 5 ...) a))
        '((2 2) (2 2)))

(mytest (match '(1 1 1 1 1 2 2 2 2)
          ((1 ... 2 2 2 2) #t))
        #t)
(mytest (match '(1 1 1 1 1 2 2 2 2)
          ((1 ... 2 ...) #t))
        #t)

(mytest (match '(1 1 1 1 1 2 2 2 2)
          (((and (not 2) a) ... 2 ...) a))
        '(1 1 1 1 1))

(mytest (match '(1 1 1 1 1 2 2 2 2)
          ((a ... 2 ...) a))
        '(1 1 1 1 1 2 2 2 2))

(mytest (match '(1 1 1 1 1 2 2 2 2)
          ((_ ... 2 ...) #t))
        #t)

(mytest (match '(pattern matching in scheme is very cool)
          (((and (not 'in) a) ... (and (not 'is) b) ... c ...) (list a c b)))
        '((pattern matching) (is very cool) (in scheme)))

(mytest (match '((1 1 2 2) (1 1 2 2) 5 5 5)
          (((1 ... 2 ...) ... 5 ...) #t))
        #t)

(mytest (match #(1 3 1 9 8 4 2 2 4 7 a b c) (#((and (? odd?) a) ... 8 (and (? even?) b) ... 7 r ...) (list a b r)))
        '((1 3 1 9) (4 2 2 4) (a b c)))

(mytest (match #(#(1 1 2 2) #(1 1 2 2) 5 5 5)
          (#(#(1 ... 2 ...) ... 5 ...) #t))
        #t)


(mytest (match #(#(1 1 2 2) #(1 1 2 2) 5 5 5)
          (#(#(1 ... a ...) ... 7 ...) #f)
          (#(#(1 ... a ...) ... 6 ...) #f)
          (#(#(1 ... a ...) ... 5 ...) a))
        '((2 2) (2 2)))

(mytest (match #(#(1 2) #(1 2) #(1 2) 5 6)
          [#(#(_ _) ..3 a ...) a])
        '(5 6))
; should not work
; (match x ((... ...) #t))


; should not work
; (match x ((pat ... ... pat) #t))

(mytest (match #(1 2 3 4 5) (#(a b (and c (not 5)) ... d) (list a b c d)))
        '(1 2 (3 4) 5))

)))
