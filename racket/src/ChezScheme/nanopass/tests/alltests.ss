;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (tests alltests)
  (export main-tests final-tests)
  (import (rnrs)) 
  
  (define main-tests
    '(
      '()
      (- 2 4)
      (* -6 7)
      (cons 0 '())
      (cons (cons 0 '()) (cons 1 '()))
      (void)
      (if #f 3)
      (let ((x 0)) x)
      (let ([x 0]) x x)
      (let ([q (add1 (add1 2))]) q)
      (+ 20 (if #t 122))
      (if #t
          (+ 20
             (if #t 122))
          10000)
      (not (if #f #t (not #f)))
      (let ([x 0][y 4000]) x)
      (begin (if #f 7) 3)
      (begin (if (zero? 4) 7) 3)
      (let ([x 0]) (begin (if (zero? x) 7) x))
      (let ([x 0]) (begin (if (zero? x) (begin x 7)) x))
      (let ([x 0] [z 9000])
        (begin (if (zero? x) (begin x 7)) z))
      (let ([x 0] [z 9000])
        (begin (if (zero? x) (begin (set! x x) 7))
               (+ x z)))
      (let ([x (cons 0 '())])
        (begin (if x (set-car! x (car x))) x))
      (let ([x (cons 0 '())])
        (begin (if x (set-car! x (+ (car x) (car x)))) x))
      (let ([x (cons 0 '())])
        (if (zero? (car x)) (begin (set-car! x x) 7) x))
      (let ([x (cons 0 '())])
        (let ([q x]) (if (zero? (car x)) (begin (set-car! q x) 7) x)))
      (let ([x 0]) (if (zero? x) (begin (set! x (+ x 5000)) x) 20))
      (let ([y 0]) (begin (if #t (set! y y)) y))
      (begin (if #t #t #t) #f)
      (begin (if (if #t #t #f) (if #t #t #f) (if #t #t #f)) #f)
      (let ([x 0] [y 4000] [z 9000])
        (let ((q (+ x z)))
          (begin
            (if (zero? x) (begin (set! q (+ x x)) 7))
            (+ y y)
            (+ x z))))
      (let ([x (let ([y 2]) y)] [y 5])
        (add1 x))
      (let ([y 4000]) (+ y y))
      ((lambda (y) y) 4000)
      (let ([f (lambda (x) x)])
        (add1 (f 0)))
      (let ([f (lambda (y) y)]) (f (f 4)))
      ((lambda (f) (f (f 4))) (lambda (y) y))
      ((let ([a 4000])
         (lambda (b) (+ a b)))
       5000)
      (((lambda (a)
          (lambda (b)
            (+ a b)))
        4000)
       5000)
      (let ([f (lambda (x) (add1 x))]) (f (f 0)))
      ((lambda (f) (f (f 0))) (lambda (x) (add1 x)))
      (let ([x 0] [f (lambda (x) x)])
        (let ([a (f x)] [b (f x)] [c (f x)]) (+ (+ a b) c)))
      (let ([x 0][y 1][z 2][f (lambda (x) x)])
        (let ([a (f x)][b (f y)][c (f z)])
          (+ (+ a b) c)))
      (let ([f (lambda (x y) x)])
        (f 0 1))
      (let ([f (lambda (x y) x)])
        (let ([a (f 0 1)]) (f a a)))
      (let ([x 0] [y 1] [z 2] [f (lambda (x y z) x)])
        (let ([a (f x y z)]) (f a a a)))
      (let ([x 0] [y 1] [z 2] [f (lambda (x y z) x)])
        (let ([a (f x y z)] [b y] [c z]) (f a b c)))
      (let ([f (lambda (a b c d)
                 (+ a d))])
        (f 0 1 2 3))
      (let ([f (lambda (x) x)])
        (+ (f 0)
           (let ([a 0] [b 1] [c 2])
             (+ (f a) (+ (f b) (f c))))))
      (let ([f (lambda (x) x)])
        (+ (f 0)
           (let ([a 0] [b 1] [c 2])
             (add1 (f a)))))
      (let ([f (lambda (x) x)])
        (+ (f 0) (let ([a 0][b 1][c 2][d 3])
                   (+ (f a)
                      (+ (f b)
                         (+ (f c)
                            (f d)))))))
      (let ([a 0])(letrec ([a (lambda () 0)][b (lambda () 11)]) (set! a 11)))
      (let ([a 0])(letrec ([a (lambda () (set! a 0))][b 11]) (a)))
      (let ([a 0])(let ([a (set! a 0)][b 11]) a))
      (let ([a 5]) (let ([a 0] [b (set! a (+ a 11))]) a))
      (letrec ([a (lambda () 0)]) (a))
      (letrec ([a (lambda () 0)] [b (lambda () 11)]) (a))
      (let ([x 0]) (letrec ([a (lambda () 0)] [b (lambda () 11)]) (set! x 11)))
      (let ([a 0]) (let ([b (set! a 0)]) a))
      (let ([a 0])(let ([a (set! a 0)]) (let ([b 11]) a)))
      (let ([a 0])(let ([a 0]) (let ([b (set! a 11)]) a)))
      (let ([a 0])(let ([a 0]) (let ([b 11]) (set! a 11))))
      (let ([f (let ([x 1]) (lambda (y) (+ x y)))])
        (let ([x 0]) (f (f x))))
      ((let ([t (lambda (x) (+ x 50))])
         (lambda (f) (t (f 1000))))
       (lambda (y) (+ y 2000)))
      (let ([x 0])
        (let ([f (let ([x 1] [z x])
                   (lambda (y)
                     (+ x (+ z y))))])
          (f (f x))))
      (((lambda (t)
          (lambda (f) (t (f 1000))))
        (lambda (x) (+ x 50)))
       (lambda (y) (+ y 2000)))
      ((let ([t 50])
         (lambda (f)
           (+ t (f))))
       (lambda () 2000))
      (((lambda (t)
          (lambda (f)
            (+ t (f))))
        50)
       (lambda () 2000))
      ((let ([x 300])
         (lambda (y) (+ x y)))
       400)
      (let ([x 3] [f (lambda (x y) x)])
        (f (f 0 0) x))
      (let ([x 3] [f (lambda (x y) x)])
        (if (f 0 0) (f (f 0 0) x) 0))
      (let ([x02 3] [f01 (lambda (x04 y03) x04)])
        (if (not x02) (f01 (f01 0 0) x02) 0))
      (let ((f (lambda (x) (if (if (pair? x) (not (eq? (car x) 0)) #f) x #f))))
        (f (cons 0 0)))
      (let ((f (lambda (x)
                 (if (if x (not (if (pair? x) (not (eq? (car x) 0)) #f)) #f)
                     x #f))))
        (f 0))
      (let ((f (lambda (x) (if (if (pair? x) #t (null? x)) x '()))))
        (f 0))
      (let ([y 4])
        (let ([f (lambda (y) y)])
          (f (f y))))
      (let ([y 4])
        (let ([f (lambda (x y) 0)])
          (f (f y y) (f y y))))
      (let ([y 4])
        (let ([f (lambda (x y) 0)])
          (f (f y y) (f y (f y y)))))
      (let ([y 4])
        (let ([f (lambda (x y) 0)])
          (f (f y (f y y)) (f y (f y y)))))
      ((lambda (y) ((lambda (f) (f (f y))) (lambda (y) y))) 4)
      (let ([f (lambda (x) (+ x x))]) (f 4000))
      (let ((x (if 1000 2000 3000))) x)
      (let ([f (lambda (x) x)]) (add1 (if #f 1 (f 22))))
      (let ([f (lambda (x) x)]) (if (f (zero? 23)) 1 22))
      (let ([f (lambda (x) (if x (not x) x))] 
            [f2 (lambda (x) (* 10 x))] 
            [x 23])
        (add1 (if (f (zero? x)) 1 (* x (f2 (sub1 x))))))
      (let ([f (lambda () 0)])
        (let ([x (f)]) 1))
      (let ([f (lambda () 0)])
        (begin (f) 1))
      (let ([f (lambda (x) x)])
        (if #t (begin (f 3) 4) 5))
      (let ([f (lambda (x) x)])
        (begin (if #t (f 4) 5) 6))
      (let ([f (lambda (x) x)])
        (begin (if (f #t)
                   (begin (f 3) (f 4))
                   (f 5))
               (f 6)))
      (let ([f (lambda (x) (add1 x))])
        (f (let ([f 3]) (+ f 1))))
      (let ((x 15)
            (f (lambda (h v) (* h v)))
            (k (lambda (x) (+ x 5)))
            (g (lambda (x) (add1 x))))
        (k (g (let ((g 3)) (f g x)))))
      (let ([x 4])
        (let ([f (lambda () x)])
          (set! x 5)
          (f)))
      (let ([x (let ([y 2]) y)]) x)
      (let ([x (if #t (let ([y 2]) y) 1)]) x)
      (let ([x (let ([y (let ([z 3]) z)]) y)]) x)
      (let ([x (if #t (let ([y (if #t (let ([z 3]) z) 2)]) y) 1)]) x)
      (+ (let ([x 3]) (add1 x)) 4)
      (+ (let ([x 3][y 4]) (* x y)) 4)
      (let ([x (add1 (let ([y 4]) y))]) x)
      (let ([x (add1 (letrec ([y (lambda () 4)]) (y)))]) x)
      (let ([x (+ (let ([y 4]) y)  (let ([y 4]) y))]) (add1 x))
      (let ([z 0]) (let ([x z]) z x))
      (let ([z 0]) (let ([x (begin (let ([y 2]) (set! z y)) z)]) x))
      (let ([x (begin (let ([y 2]) (set! y y)) (let ([z 3]) z))]) x)
      (letrec ([one (lambda (n) (if (zero? n) 1 (one (sub1 n))))])
        (one 13))
      (letrec ((even (lambda (x) (if (zero? x) #t (odd (sub1 x)))))
               (odd (lambda (x) (if (zero? x) #f (even (sub1 x))))))
        (odd 13))
      (let ([t #t] [f #f])
        (letrec ((even (lambda (x) (if (zero? x) t (odd (sub1 x)))))
                 (odd (lambda (x) (if (zero? x) f (even (sub1 x))))))
          (odd 13)))
      (let ((even (lambda (x) x)))
        (even (letrec ((even (lambda (x) (if (zero? x) #t (odd (sub1 x)))))
                       (odd (lambda (x) (if (zero? x) #f (even (sub1 x))))))
                (odd 13))))
      (letrec ((fact (lambda (n) (if (zero? n) 1 (* n (fact (sub1 n)))))))
        (fact 5))
      (let ([x 5])
        (letrec ([a (lambda (u v w) (if (zero? u) (b v w) (a (- u 1) v w)))]
                 [b (lambda (q r)
                      (let ([p (* q r)])
                        (letrec
                          ([e (lambda (n) (if (zero? n) (c p) (o (- n 1))))]
                           [o (lambda (n) (if (zero? n) (c x) (e (- n 1))))])
                          (e (* q r)))))]
                 [c (lambda (x) (* 5 x))])
          (a 3 2 1)))
      (let ([f (lambda () 80)]) (let ([a (f)] [b (f)]) 0))
      (let ([f (lambda () 80)]) (let ([a (f)] [b (f)]) (* a b)))
      (let ([f (lambda () 80)] [g (lambda () 80)]) 
        (let ([a (f)] [b (g)])
          (* a b)))
      (let ((f (lambda (x) (add1 x)))
            (g (lambda (x) (sub1 x)))
            (t (lambda (x) (add1 x)))
            (j (lambda (x) (add1 x)))
            (i (lambda (x) (add1 x)))
            (h (lambda (x) (add1 x)))
            (x 80))
        (let ((a (f x)) (b (g x)) (c (h (i (j (t x))))))
          (* a (* b (+ c 0)))))
      (let ((x 3000))
        (if (integer? x)
            (let ((y (cons x '())))
              (if (if (pair? y) (null? (cdr y)) #f)
                  (+ x 5000)
                  (- x 3000)))))
      (let ((x (cons 1000 2000)))
        (if (pair? x)
            (let ((temp (car x)))
              (set-car! x (cdr x))
              (set-cdr! x temp)
              (+ (car x) (cdr x)))
            10000000))
      (let ((v (make-vector 3)))
        (vector-set! v 0 10)
        (vector-set! v 1 20)
        (vector-set! v 2 30)        
        (if (vector? v)
            (+ (+ (vector-length v) (vector-ref v 0))
               (+ (vector-ref v 1) (vector-ref v 2)))
            10000))
      (let ([fact (lambda (fact n)
                    (if (zero? n) 1 (* (fact fact (sub1 n)) n)))])
        (fact fact 5))
      (let ([s (make-vector 20)])
        (vector-set! s 19 #\z)
        (if (vector? s)
            (+ 20 (let ([c #\z]) (if (char? c) 122)))
            10000))
      (let ([s (make-vector 20)])
        (vector-set! s 19 #\z)
        (if (vector? s)
            (+ (vector-length s)
               (let ([c (vector-ref s 19)])
                 (if (char? c)
                     (char->integer (vector-ref s 19)))))
            10000))
      (let ((s (make-vector 20)) (s2 (make-vector 3)))
        (vector-set! s 19 #\z)
        (vector-set! s 18 #\t)
        (vector-set! s2 0 #\a)
        (if (vector? s)
            (+ (vector-length s)
               (let ((c (vector-ref s 18)))
                 (if (char? c)
                     (+ (char->integer (vector-ref s 19))
                        (char->integer c)))))
            10000))
      (let ([f (lambda (x) (+ x 1000))])
        (if (zero? (f -2)) (f 6000) (f (f 8000))))
      (let ([f (lambda (x) (+ x 1000))])
        (if (zero? (f -1)) (f 6000) (f (f 8000))))
      (let ((f (lambda (x y) (+ x 1000))))
        (+ (if (f 3000 (begin 0 0 0)) (f (f 4000 0) 0) 8000) 2000))
      ((((lambda (x)
           (lambda (y)
             (lambda (z)
               (+ x (+ y (+ z y))))))
         5) 6) 7)
      ((((((lambda (x)
             (lambda (y)
               (lambda (z)
                 (lambda (w)
                   (lambda (u)
                     (+ x (+ y (+ z (+ w u)))))))))
           5) 6) 7) 8) 9)
      (let ((f (lambda (x) x)))
        (if (procedure? f) #t #f))
      (let ((sum (lambda (sum ls)
                   (if (null? ls) 0 (+ (car ls) (sum sum (cdr ls)))))))
        (sum sum (cons 1 (cons 2 (cons 3 '())))))
      (let ((v (make-vector 5))
            (w (make-vector 7)))
        (vector-set! v 0 #t)
        (vector-set! w 3 #t)
        (if (boolean? (vector-ref v 0))
            (vector-ref w 3)
            #f))
      (let ((a 5) (b 4))
        (if (< b 3) 
            (eq? a (+ b 1)) 
            (if (<= b 3)
                (eq? (- a 1) b)
                (= a (+ b 2)))))
      (let ((a 5) (b 4))
        (if #f (eq? a (+ b 1)) (if #f (eq? (- a 1) b) (= a (+ b 2)))))
      (((lambda (a) (lambda () (+ a (if #t 200)) 1500)) 1000))
      (((lambda (b) (lambda (a) (set! a (if 1 2)) (+ a b))) 100) 200)
      ((((lambda (a) 
           (lambda (b) 
             (set! a (if b 200)) 
             (lambda (c) (set! c (if 300 400))
               (+ a (+ b c)))))
         1000) 2000) 3000)
      ((((lambda (a) (lambda (b) (lambda (c) (+ a (+ b c))))) 10) 20) 30)
      (+ 2 3)
      ((lambda (a) (+ 2 a)) 3)
      (((lambda (b) (lambda (a) (+ b a))) 3) 2)
      ((lambda (b) ((lambda (a) (+ b a)) 2)) 3)
      ((lambda (f) (f (f 5))) (lambda (x) x))
      ((let ((f (lambda (x) (+ x 3000)))) (lambda (y) (f (f y)))) 2000)
      (let ((n #\newline) (s #\space) (t #\tab))
        (let ((st (make-vector 5)))
          (vector-set! st 0 n)
          (vector-set! st 1 s)
          (vector-set! st 2 t)
          (if (not (vector? st))
              10000
              (vector-length st))))
      (let ((s (make-vector 1)))
        (vector-set! s 0 #\c)
        (if (eq? (vector-ref s 0) #\c) 1000 2000))
      (not 17)
      (not #f)
      (let ([fact (lambda (fact n acc)
                    (if (zero? n) acc (fact fact (sub1 n) (* n acc))))])
        (fact fact 5 1))
      ((lambda (b c a) 
         (let ((b (+ b a)) (a (+ a (let ((a (+ b b)) (c (+ c c))) (+ a a)))))
           (* a a))) 2 3 4)
      (let ((f (lambda (x) (lambda () (x))))) ((f (lambda () 3))))
      (letrec ((f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))))
        (let ([q 17])
          (let ((g (lambda (a) (set! q 10) (lambda () (a q)))))
            ((g f)))))
      (letrec ((f (lambda (x) (if (zero? x) 1 (* x (f (- x 1)))))))
        (let ((g (lambda (a) (lambda (b) (a b)))))
          ((g f) 10)))
      (letrec ((f (lambda () (+ a b)))
               (g (lambda (y) (set! g (lambda (y) y)) (+ y y)))
               (a 17)
               (b 35)
               (h (cons (lambda () a) (lambda (v) (set! a v)))))
        (let ((x1 (f)) (x2 (g 22)) (x3 ((car h))))
          (let ((x4 (g 22)))
            ((cdr h) 3)
            (let ((x5 (f)) (x6 ((car h))))
              (cons x1 (cons x2 (cons x3 (cons x4 (cons x5 x6)))))))))
      (letrec ((f (lambda () (+ a b)))
               (a 17)
               (b 35)
               (h (cons (lambda () a) (lambda () b))))
        (cons (f) (cons a (cons b (cons ((car h)) ((cdr h)))))))
      (letrec ((f (lambda (x) (letrec ((x 3)) 3))))
        (letrec ((g (lambda (x) (letrec ((y 14)) (set! y 7) y))))
          (set! g (cons g 3))
          (letrec ((h (lambda (x) x)) (z 42))
            (cons (cdr g) (h z)))))
      (let ([t #t] [f #f])
        (let ([bools (cons t f)] [id (lambda (x) (if (not x) f t))])
          (letrec
            ([even (lambda (x) (if (zero? x) (id (car bools)) (odd (- x 1))))]
             [odd (lambda (y) (if (zero? y) (id (cdr bools)) (even (- y 1))))])
            (odd 5))))
      (letrec ([fib (lambda (x)
                      (let ([decrx (lambda () (set! x (- x 1)))])
                        (if (< x 2) 
                            1 
                            (+ (begin (decrx) (fib x))
                               (begin (decrx) (fib x))))))])
        (fib 10))
      (letrec ([fib (lambda (x)
                      (let ([decrx (lambda () (lambda (i) (set! x (- x i))))])
                        (if (< x 2)
                            1
                            (+ (begin ((decrx) 1) (fib x))
                               (begin ((decrx) 1) (fib x))))))])
        (fib 10))
      ;; Jie Li
      (let ((a 5))
        (let ((b (cons a 6)))
          (let ((f (lambda(x) (* x a))))
            (begin (if (- (f a) (car b))
                       (begin (set-car! b (if (not a) (* 2 a) (+ 2 a)))
                              (f a))
                       (if (not (not (< (f a) b))) (f a)))
                   (not 3)
                   (void)
                   (f (car b))))))
      (letrec ([f (lambda (x y) 
                    (if (not x) (g (add1 x) (add1 y)) (h (+ x y))))]
               [g (lambda (u v)
                    (let ([a (+ u v)] [b (* u v)])
                      (letrec ([e (lambda (d) 
                                    (letrec ([p (cons a b)]
                                             [q (lambda (m)
                                                  (if (< m u)
                                                    (f m d)
                                                    (h (car p))))])
                                      (q (f a b))))])
                        (e u))))]
               [h (lambda (w) w)])
        (f 4 5))
      (letrec ((f (lambda (x)
                    (+ x (((lambda (y)
                             (lambda (z)
                               (+ y z)))
                           6) 7))))
               (g (+ 5 ((lambda (w u) (+ w u)) 8 9))))
        g)
      ;; Jordan Johnson
      (let ((test (if (not (not 10)) #f 5)))
        (letrec ([num 5]
                 [length
                   (lambda (ls)
                     (let ((len (if ((lambda (ck) 
                                       (begin ck (set! num test) ck))
                                     (null? ls))
                                    (begin num (set! num 0) num)
                                    (begin (length '())
                                           (set! num 5)
                                           (+ 1 (length (cdr ls)))))))
                       (if len len)))])
          (length (cons 5 (cons (if (set! num 50) (length (cons test '())) 1)
                                '())))))
      (letrec ([quotient (lambda (x y)
                           (if (< x 0)
                               (- 0 (quotient (- 0 x) y))
                               (if (< y 0)
                                   (- 0 (quotient x (- 0 y)))
                                   (letrec ([f (lambda (x a)
                                                 (if (< x y)
                                                     a
                                                     (f (- x y) (+ a 1))))])
                                     (f x 0)))))])
        (letrec ([sub-interval 1]
                 [sub-and-continue
                   (lambda (n acc k) (k (- n sub-interval) (* n acc)))]
                 [strange-fact
                   (lambda (n acc)
                     (if (zero? n)
                         (lambda (proc) (proc acc))
                         (sub-and-continue n acc strange-fact)))])
          (let ([x 20]
                [fact (let ((seed 1)) (lambda (n) (strange-fact n seed)))])
            (let ([give-fact5-answer (fact 5)]
                  [give-fact6-answer (fact 6)]
                  [answer-user (lambda (ans) (quotient ans x))])
              (set! x (give-fact5-answer answer-user))
              (begin (set! x (give-fact6-answer answer-user)) x)))))
      (let ((y '()) (z 10))
        (let ((test-ls (cons 5 y)))
          (set! y (lambda (f)
                    ((lambda (g) (f (lambda (x) ((g g) x))))
                     (lambda (g) (f (lambda (x) ((g g) x)))))))
          (set! test-ls (cons z test-ls))
          (letrec ((length (lambda (ls)
                             (if (null? ls) 0 (+ 1 (length (cdr ls)))))))
            (let ((len (length test-ls)))
              (eq? (begin
                     (set! length (y (lambda (len)
                                       (lambda (ls)
                                         (if (null? ls)
                                             0
                                             (+ 1 (len (cdr ls))))))))
                     (length test-ls))
                   len)))))
      ;; Ryan Newton
      (letrec ((loop (lambda () (lambda () (loop))))) (loop) 0)
      (letrec ([f (lambda ()
                    (letrec ([loop
                               (lambda (link)
                                 (lambda ()
                                   (link)))])
                      (loop (lambda () 668))))])
        ((f)))
      ;; AWK - the following test uses the syntax #36rgood and #36rbad,
      ;; which the ikarus reader seems to choak on, so I'm commenting out
      ;; this test for now.
      ;    (if (lambda () 1)
      ;        (let ((a 2))
      ;          (if (if ((lambda (x)
      ;                     (let ((x (set! a (set! a 1))))
      ;                       x)) 1)
      ;                  (if (eq? a (void))
      ;                      #t
      ;                      #f)
      ;                  #f)
      ;              #36rgood        ; dyb: cannot use symbols, so use radix 36
      ;              #36rbad)))      ; syntax to make all letters digits 
      
      ; contributed by Ryan Newton
      (letrec ([dropsearch 
                 (lambda (cell tree)
                   (letrec ([create-link 
                              (lambda (node f)
                                (lambda (g)
                                  (if (not (pair? node))
                                      (f g)
                                      (if (eq? node cell)
                                          #f
                                          (f (create-link 
                                               (car node)
                                               (create-link 
                                                 (cdr node) g)))))))]
                            [loop
                              (lambda (link)
                                (lambda ()
                                  (if link
                                      (loop (link (lambda (v) v)))
                                      #f)))])
                     (loop (create-link tree (lambda (x) x)))))] 
               [racethunks
                 (lambda (thunkx thunky)
                   (if (if thunkx thunky #f)
                       (racethunks (thunkx) (thunky))
                       (if thunky
                           #t
                           (if thunkx
                               #f
                               '()))))] 
               [higher? (lambda (x y tree)
                          (racethunks (dropsearch x tree)
                                      (dropsearch y tree)))]
               [under?
                 (lambda (x y tree)
                   (racethunks (dropsearch x y)
                               (dropsearch x tree)))] 
               [explore
                 (lambda (x y tree)
                   (if (not (pair? y))
                       #t
                       (if (eq? x y)
                           #f    ; takes out anything pointing to itself
                           (let ((result (higher? x y tree)))
                             (if (eq? result #t)
                                 (if (explore y (car y) tree)
                                     (explore y (cdr y) tree)
                                     #f)
                                 (if (eq? result #f)
                                     (process-vertical-jump x y tree)
                                     (if (eq? result '())
                                         (process-horizontal-jump x y tree)
                                         )))))))] 
               [process-vertical-jump
                 (lambda (jumpedfrom jumpedto tree)
                   (if (under? jumpedfrom jumpedto tree)
                       #f
                       (fullfinite? jumpedto)))] 
               [process-horizontal-jump
                 (lambda (jumpedfrom jumpedto tree)
                   (fullfinite? jumpedto))] 
               [fullfinite?
                 (lambda (pair)
                   (if (not (pair? pair))
                       #t
                       (if (explore pair (car pair) pair)
                           (explore pair (cdr pair) pair)
                           #f)))])
        (cons (fullfinite? (cons 1 2)) 
              (cons (fullfinite? (let ((x (cons 1 2))) (set-car! x x) x))
                    (cons (fullfinite? 
                            (let ([a (cons 0 0)] [b (cons 0 0)] [c (cons 0 0)])
                              (set-car! a b) (set-cdr! a c) (set-cdr! b c)
                              (set-car! b c) (set-car! c b) (set-cdr! c b) a))
                          '())))))) 
  
  (define final-tests 
    ; extracted tests from assignment writeups
    '(75 
      (+ 16 32) 
      (* 16 128) 
      (let ((x 16) (y 128)) (* x y)) 
      (let ([x 17]) (+ x x)) (cons 16 32) (cdr (cons 16 32)) 
      (let ((x (cons 16 32))) (pair? x))
      (let ([x 3]) (let ([y (+ x (quote 4))]) (+ x y))) 
      (let ([f (lambda (x) x)]) (let ([a 1]) (* (+ (f a) a) a))) 
      (let ([k (lambda (x y) x)]) 
        (let ([b 17]) ((k (k k 37) 37) b (* b b)))) 
      (let ([f (lambda () 
                 (let ([n 256]) 
                   (let ([v (make-vector n)])
                     (vector-set! v 32 n)
                     (vector-ref v 32))))])
        (pair? (f))) 
      (let ((w 4) (x 8) (y 16) (z 32))
        (let ((f (lambda ()
                   (+ w (+ x (+ y z))))))
          (f))) 
      (let ((f (lambda (g u) (g (if u (g 37) u)))))
        (f (lambda (x) x) 75)) 
      (let ((f (lambda (h u) (h (if u (h (+ u 37)) u)))) (w 62))
        (f (lambda (x) (- w x)) (* 75 w))) 
      (let ([t #t] [f #f])
        (let ([bools (cons t f)] [id (lambda (x) (if (not x) f t))])
          (letrec
            ([even (lambda (x) (if (id (zero? x)) (car bools) (odd (- x 1))))]
             [odd (lambda (y) (if (zero? y) (id (cdr bools)) (even (- y 1))))])
            (odd 5)))) 
      ((lambda (x y z)
         (let  ((f (lambda (u v) (begin (set! x u) (+ x v))))
                (g (lambda (r s) (begin (set! y (+ z s)) y))))
           (* (f '1 '2) (g '3 '4))))
       '10 '11 '12) 
      ((lambda (x y z)
         (let ((f '#f)
               (g (lambda (r s) (begin (set! y (+ z s)) y))))
           (begin
             (set! f
               (lambda (u v) (begin (set! v u) (+ x v))))
             (* (f '1 '2) (g '3 '4)))))
       '10 '11 '12) 
      (letrec ((f (lambda (x) (+ x 1)))
               (g (lambda (y) (f (f y)))))
        (+ (f 1) (g 1))) 
      (let ((y 3))
        (letrec
          ((f (lambda (x) (if (zero? x) (g (+ x 1)) (f (- x y)))))
           (g (lambda (x) (h (* x x))))
           (h (lambda (x) x)))
          (g 39))) 
      (letrec ((f (lambda (x) (+ x 1))) (g (lambda (y) (f (f y)))))
        (set! f (lambda (x) (- x 1)))
        (+ (f 1) (g 1))) 
      (letrec ([f (lambda () (+ a b))] 
               [a 17] 
               [b 35]
               [h (cons (lambda () a) (lambda () b))])
        (cons (f) (cons a (cons b (cons ((car h)) ((cdr h))))))) 
      (let ((v (make-vector 8)))
        (vector-set! v 0 '())
        (vector-set! v 1 (void))
        (vector-set! v 2 #f)
        (vector-set! v 3 #\a)
        (vector-set! v 4 #\z)
        (vector-set! v 5 #t)
        (vector-set! v 6 2)
        (vector-set! v 7 5)
        (vector-ref v (vector-ref v 6))) 
      (let ([x 5] [th (let ((a 1)) (lambda () a))])
        (letrec ([fact (lambda (n th)
                         (if (zero? n) (th) (* n (fact (- n 1) th))))])
          (fact x th))) 
      (let ([negative? (lambda (n) (< n 0))])
        (letrec
          ([fact (lambda (n)
                   (if (zero? n) 1 (* n (fact (- n 1)))))]
           [call-fact (lambda (n) 
                        (if (not (negative? n)) 
                            (fact n) 
                            (- 0 (fact (- 0 n)))))])
          (cons (call-fact 5) (call-fact -5)))) 
      (letrec ([iota-fill! (lambda (v i n)
                             (if (not (= i n))
                               (begin
                                 (vector-set! v i i)
                                 (iota-fill! v (+ i 1) n))))])
        (let ([n 4])
          (let ([v (make-vector n)]) (iota-fill! v 0 n) v))) 
      ; make-vector with non-constant operand and improper alignment
      (let ([x 6])
        (let ([v (make-vector x)])
          (vector-set! v 0 3)
          (vector-set! v 1 (cons (vector-ref v 0) 2))
          (vector-set! v 2 (cons (vector-ref v 1) 2))
          (vector-set! v 3 (cons (vector-ref v 2) 2))
          (vector-set! v 4 (cons (vector-ref v 3) 2))
          (vector-set! v 5 (cons (vector-ref v 4) 2))
          (cons (pair? (vector-ref v 5)) (car (vector-ref v 4))))) 
      ; nest some lambdas
      (((((lambda (a)
            (lambda (b)
              (lambda (c)
                (lambda (d)
                  (cons (cons a b) (cons c d))))))
          33) 55) 77) 99) 
      ; stress the register allocator
      (let ((a 17))
        (let ((f (lambda (x)
                   (let ((x1 (+ x 1)) (x2 (+ x 2)))
                     (let ((y1 (* x1 7)) (y2 (* x2 7)))
                       (let ((z1 (- y1 x1)) (z2 (- y2 x2)))
                         (let ((w1 (* z1 a)) (w2 (* z2 a)))
                           (let ([g (lambda (b)
                                      (if (= b a)
                                          (cons x1 (cons y1 (cons z1 '())))
                                          (cons x2 (cons y2 (cons z2 '())))))]
                                 [h (lambda (c)
                                      (if (= c x) w1 w2))])
                             (if (if (= (* x x) (+ x x))
                                     #t
                                     (< x 0))
                                 (cons (g 17) (g 16))
                                 (cons (h x) (h (- x 0))))))))))))
          (cons (f 2) (cons (f -1) (cons (f 3) '()))))) 
      ; printer
      (letrec 
        ([write 
           (lambda (x)
             (let ([digits
                     (let ([v (make-vector 10)])
                       (vector-set! v 0 #\0)
                       (vector-set! v 1 #\1)
                       (vector-set! v 2 #\2)
                       (vector-set! v 3 #\3)
                       (vector-set! v 4 #\4)
                       (vector-set! v 5 #\5)
                       (vector-set! v 6 #\6)
                       (vector-set! v 7 #\7)
                       (vector-set! v 8 #\8)
                       (vector-set! v 9 #\9)
                       v)])
               (letrec 
                 ([list->vector
                    (lambda (ls)
                      (let ([v (make-vector (length ls))])
                        (letrec 
                          ([loop 
                             (lambda (ls i)
                               (if (null? ls)
                                   v
                                   (begin
                                     (vector-set!  v i (car ls))
                                     (loop (cdr ls) (+ i 1)))))])
                          (loop ls 0))))]
                  [length
                    (lambda (ls) 
                      (if (null? ls) 
                          0
                          (add1 (length (cdr ls)))))]
                  [map
                    (lambda (p ls)
                      (if (null? ls) 
                          '() 
                          (cons (p (car ls)) 
                                (map p (cdr ls)))))]
                  [wr (lambda (x p)
                        (if (eq? x #f)
                            (cons #\# (cons #\f p))
                            (if (eq? x #t)
                              (cons #\# (cons #\t p))
                              (if (eq? x '())
                                  (cons #\( (cons #\) p))
                                  (if (eq? x (void))
                                      (cons #\# (cons #\< (cons #\v 
                                        (cons #\o (cons #\i (cons #\d 
                                          (cons #\> p)))))))
                                      (if (char? x)
                                          (cons #\# (cons #\\ 
                                            (if (eq? x #\newline) 
                                                (cons #\n (cons #\e (cons #\w 
                                                  (cons #\l (cons #\i (cons #\n
                                                    (cons #\e p)))))))
                                                (if (eq? x #\space)
                                                    (cons #\s (cons #\p 
                                                      (cons #\a (cons #\c 
                                                        (cons #\e p)))))
                                                    (if (eq? x #\tab)
                                                        (cons #\t (cons #\a 
                                                          (cons #\b p)))
                                                        (cons x p))))))
                                          (if (integer? x)
                                              (if (< x 0) 
                                                  (cons #\- (wrint (- 0 x) p))
                                                  (wrint x p))
                                              (if (pair? x)
                                                  (cons #\( ; )
                                                    (letrec 
                                                      ([loop
                                                         (lambda (x)
                                                           (wr (car x)
                                                             (if (pair? (cdr x))
                                                                 (cons #\space
                                                                   (loop
                                                                     (cdr x)))
                                                                 (if 
                                                                   (null?
                                                                     (cdr x)) 
                                                                   ;(
                                                                   (cons #\) p)
                                                                   (cons 
                                                                     #\space 
                                                                     (cons 
                                                                       #\.
                                                                       (cons 
                                                                         #\space
                                                                         (wr 
                                                                           (cdr 
                                                                             x)
                                                                           ;(
                                                                           (cons 
                                                                             #\)
                                                                             p)
                                                                           ))))
                                                                   ))))])
                                                      (loop x)))
                                                  (if (vector? x)
                                                      (cons #\# (cons #\( ; )
                                                        (let 
                                                          ([n (vector-length 
                                                                x)])
                                                          (if (= n 0) ;(
                                                              (cons #\) p)
                                                              (letrec 
                                                                ([loop
                                                                   (lambda (i)
                                                                     (wr 
                                                                       (vector-ref
                                                                         x i)
                                                                       (if
                                                                         (= 
                                                                           (+
                                                                             i
                                                                             1)
                                                                           n)
                                                                         ;(
                                                                         (cons
                                                                           #\)
                                                                           p)
                                                                         (cons
                                                                           #\space
                                                                           (loop
                                                                             (+
                                                                               i
                                                                               1))
                                                                           )))
                                                                     )])
                                                                (loop 0))))))
                                                      (if (procedure? x)
                                                          (cons #\# (cons #\<
                                                            (cons #\p (cons #\r
                                                              (cons #\o
                                                                (cons #\c
                                                                  (cons #\e
                                                                    (cons #\d
                                                                      (cons #\u
                                                                        (cons
                                                                          #\r
                                                                          (cons
                                                                            #\e
                                                                            (cons
                                                                              #\>
                                                                              p)
                                                                            )))
                                                                      ))))))))
                                                          (cons #\# (cons #\<
                                                            (cons #\g (cons #\a
                                                              (cons #\r
                                                                (cons #\b
                                                                  (cons #\a
                                                                    (cons #\g
                                                                      (cons #\e
                                                                        (cons
                                                                          #\>
                                                                          p))))
                                                                  )))))))))
                                              )))))))]
                  [wrint (lambda (n p)
                           (if (< n 10)
                               (cons (vector-ref digits n) p)
                               (wrint
                                 (quotient n 10)
                                 (cons (vector-ref digits
                                                   (remainder n 10)) p))))]
                  [remainder (lambda (x y)
                               (let ([q (quotient x y)]) (- x (* y q))))]
                  [quotient (lambda (x y)
                              (if (< x 0)
                                  (- 0 (quotient (- 0 x) y))
                                  (if (< y 0)
                                      (- 0 (quotient x (- 0 y)))
                                      (letrec ([f (lambda (x a)
                                                    (if (< x y) 
                                                        a 
                                                        (f (- x y) (+ a 1))))])
                                        (f x 0)))))])
                 (list->vector (map (lambda (x) 
                                      (char->integer x))
                                    (wr x '()))))))])
        (write
          (let ([v1 (make-vector 4)] [v2 (make-vector 0)])
            (vector-set! v1 0 #\a)
            (vector-set! v1 1 #\space)
            (vector-set! v1 2 #\newline)
            (vector-set! v1 3 #\tab)
            (cons (cons 0 (cons 4 (cons 2334 -98765)))
                  (cons (cons #t (cons #f (cons (void) (cons '() '()))))
                        (cons v1 (cons v2 write))))))))))
