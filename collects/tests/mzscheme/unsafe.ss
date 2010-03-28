
(load-relative "loadtest.ss")

(Section 'unsafe)

(require scheme/unsafe/ops
         scheme/flonum
         scheme/foreign)

(let ()
  (define ((add-star str) sym)
    (string->symbol (regexp-replace str (symbol->string sym) (string-append str "*"))))
  (define (test-tri result proc x y z 
                    #:pre [pre void] 
                    #:post [post (lambda (x) x)] 
                    #:literal-ok? [lit-ok? #t])
    (pre)
    (test result (compose post (eval proc)) x y z)
    (pre)
    (test result (compose post (eval `(lambda (x y z) (,proc x y z)))) x y z)
    (when lit-ok?
      (pre)
      (test result (compose post (eval `(lambda (y z) (,proc ,x y z)))) y z))
    (pre)
    (test result (compose post (eval `(lambda (x z) (,proc x ,y z)))) x z)
    (pre)
    (test result (compose post (eval `(lambda (x y) (,proc x y ,z)))) x y)
    (pre)
    (test result (compose post (eval `(lambda (x) (,proc x ,y ,z)))) x)
    (when lit-ok?
      (pre)
      (test result (compose post (eval `(lambda (y) (,proc ,x y ,z)))) y)
      (pre)
      (test result (compose post (eval `(lambda (z) (,proc ,x ,y z)))) z)))
  (define (test-bin result proc x y 
                    #:pre [pre void] 
                    #:post [post (lambda (x) x)]
                    #:literal-ok? [lit-ok? #t])
    (pre)
    (test result (compose post (eval proc)) x y)
    (pre)
    (test result (compose post (eval `(lambda (x y) (,proc x y)))) x y)
    (when lit-ok?
      (pre)
      (test result (compose post (eval `(lambda (y) (,proc ,x y)))) y))
    (pre)
    (test result (compose post (eval `(lambda (x) (,proc x ,y)))) x))
  (define (test-un result proc x)
    (test result (eval proc) x)
    (test result (eval `(lambda (x) (,proc x))) x))

  (test-bin 3 'unsafe-fx+ 1 2)
  (test-bin -1 'unsafe-fx+ 1 -2)
  (test-bin 12 'unsafe-fx+ 12 0)
  (test-bin -12 'unsafe-fx+ 0 -12)

  (test-bin 8 'unsafe-fx- 10 2)
  (test-bin 3 'unsafe-fx- 1 -2)
  (test-bin 13 'unsafe-fx- 13 0)

  (test-bin 20 'unsafe-fx* 10 2)
  (test-bin -20 'unsafe-fx* 10 -2)
  (test-bin -2 'unsafe-fx* 1 -2)
  (test-bin -21 'unsafe-fx* -21 1)
  (test-bin 0 'unsafe-fx* 0 -2)
  (test-bin 0 'unsafe-fx* -21 0)
  
  (test-bin 3 'unsafe-fxquotient 17 5)
  (test-bin -3 'unsafe-fxquotient 17 -5)
  (test-bin 0 'unsafe-fxquotient 0 -5)
  (test-bin 18 'unsafe-fxquotient 18 1)

  (test-bin 2 'unsafe-fxremainder 17 5)
  (test-bin 2 'unsafe-fxremainder 17 -5)
  (test-bin 0 'unsafe-fxremainder 0 -5)
  (test-bin 0 'unsafe-fxremainder 10 1)

  (test-bin 2 'unsafe-fxmodulo 17 5)
  (test-bin -3 'unsafe-fxmodulo 17 -5)
  (test-bin 0 'unsafe-fxmodulo 0 -5)
  (test-bin 0 'unsafe-fxmodulo 10 1)

  (test-bin 3.4 'unsafe-fl+ 1.4 2.0)
  (test-bin -1.1 'unsafe-fl+ 1.0 -2.1)
  (test-bin +inf.0 'unsafe-fl+ 1.0 +inf.0)
  (test-bin -inf.0 'unsafe-fl+ 1.0 -inf.0)
  (test-bin +nan.0 'unsafe-fl+ +nan.0 -inf.0)
  (test-bin 1.5 'unsafe-fl+ 1.5 0.0)
  (test-bin 1.7 'unsafe-fl+ 0.0 1.7)

  (test-bin #f unsafe-fx= 1 2)
  (test-bin #t unsafe-fx= 2 2)
  (test-bin #f unsafe-fx= 2 1)

  (test-bin #t unsafe-fx< 1 2)
  (test-bin #f unsafe-fx< 2 2)
  (test-bin #f unsafe-fx< 2 1)

  (test-bin #f unsafe-fx> 1 2)
  (test-bin #f unsafe-fx> 2 2)
  (test-bin #t unsafe-fx> 2 1)

  (test-bin #t unsafe-fx<= 1 2)
  (test-bin #t unsafe-fx<= 2 2)
  (test-bin #f unsafe-fx<= 2 1)

  (test-bin #f unsafe-fx>= 1 2)
  (test-bin #t unsafe-fx>= 2 2)
  (test-bin #t unsafe-fx>= 2 1)

  (test-bin 3 unsafe-fxmin 3 30)
  (test-bin -30 unsafe-fxmin 3 -30)

  (test-bin 30 unsafe-fxmax 3 30)
  (test-bin 3 unsafe-fxmax 3 -30)

  (test-bin 7.9 'unsafe-fl- 10.0 2.1)
  (test-bin 3.7 'unsafe-fl- 1.0 -2.7)
  (test-bin 1.5 'unsafe-fl- 1.5 0.0)

  (test-bin 20.02 'unsafe-fl* 10.01 2.0)
  (test-bin -20.02 'unsafe-fl* 10.01 -2.0)
  (test-bin +nan.0 'unsafe-fl* +inf.0 0.0)
  (test-bin 1.8 'unsafe-fl* 1.0 1.8)
  (test-bin 1.81 'unsafe-fl* 1.81 1.0)
  
  (test-bin (exact->inexact 17/5) 'unsafe-fl/ 17.0 5.0)
  (test-bin +inf.0 'unsafe-fl/ 17.0 0.0)
  (test-bin -inf.0 'unsafe-fl/ -17.0 0.0)
  (test-bin 1.5 'unsafe-fl/ 1.5 1.0)

  (test-bin 3 'unsafe-fxand 7 3)
  (test-bin 2 'unsafe-fxand 6 3)
  (test-bin 3 'unsafe-fxand -1 3)

  (test-bin 7 'unsafe-fxior 7 3)
  (test-bin 7 'unsafe-fxior 6 3)
  (test-bin -1 'unsafe-fxior -1 3)

  (test-bin 4 'unsafe-fxxor 7 3)
  (test-bin 5 'unsafe-fxxor 6 3)
  (test-bin -4 'unsafe-fxxor -1 3)

  (test-un -1 'unsafe-fxnot 0)
  (test-un -4 'unsafe-fxnot 3)

  (test-bin 32 'unsafe-fxlshift 2 4)
  (test-bin 32 'unsafe-fxlshift 8 2)
  (test-bin 8 'unsafe-fxlshift 8 0)

  (test-bin 2 'unsafe-fxrshift 32 4)
  (test-bin 8 'unsafe-fxrshift 32 2)
  (test-bin 8 'unsafe-fxrshift 8 0)

  (test-un 5 unsafe-fxabs 5)
  (test-un 5 unsafe-fxabs -5)
  (test-un 5.0 unsafe-flabs 5.0)
  (test-un 5.0 unsafe-flabs -5.0)
  (test-un 0.0 unsafe-flabs -0.0)
  (test-un +inf.0 unsafe-flabs -inf.0)

  (test-un 5.0 unsafe-flsqrt 25.0)
  (test-un 0.5 unsafe-flsqrt 0.25)
  (test-un +nan.0 unsafe-flsqrt -1.0)

  (test-un 8.0 'unsafe-fx->fl 8)
  (test-un -8.0 'unsafe-fx->fl -8)

  (test-bin 3.7 'unsafe-flmin 3.7 4.1)
  (test-bin 2.1 'unsafe-flmin 3.7 2.1)
  (test-bin +nan.0 'unsafe-flmin +nan.0 2.1)
  (test-bin +nan.0 'unsafe-flmin 2.1 +nan.0)
  (test-bin 3.7 'unsafe-flmax 3.7 2.1)
  (test-bin 4.1 'unsafe-flmax 3.7 4.1)
  (test-bin +nan.0 'unsafe-flmax +nan.0 2.1)
  (test-bin +nan.0 'unsafe-flmax 2.1 +nan.0)

  ;; test unboxing:
  (test-tri 9.0 '(lambda (x y z) (unsafe-fl+ (unsafe-fl- x z) y)) 4.5 7.0 2.5)
  (test-tri 9.0 '(lambda (x y z) (unsafe-fl+ y (unsafe-fl- x z))) 4.5 7.0 2.5)
  (test-bin 10.0 '(lambda (x y) (unsafe-fl+ (unsafe-fx->fl x) y)) 2 8.0)
  (test-bin 10.0 '(lambda (x y) (unsafe-fl+ (unsafe-fx->fl x) y)) 2 8.0)
  (test-bin 9.5 '(lambda (x y) (unsafe-fl+ (unsafe-flabs x) y)) -2.0 7.5)
  (test-tri (/ 20.0 0.8) '(lambda (x y z) (unsafe-fl/ (unsafe-fl* x z) y)) 4.0 0.8 5.0)
  (test-tri (/ 0.8 20.0) '(lambda (x y z) (unsafe-fl/ y (unsafe-fl* x z))) 4.0 0.8 5.0)
  (test-tri #t '(lambda (x y z) (unsafe-fl< (unsafe-fl+ x y) z)) 1.2 3.4 5.0)
  (test-tri 'yes '(lambda (x y z) (if (unsafe-fl< (unsafe-fl+ x y) z) 'yes 'no)) 1.2 3.4 5.0)
  (test-tri #f '(lambda (x y z) (unsafe-fl> (unsafe-fl+ x y) z)) 1.2 3.4 5.0)
  (test-tri 'no '(lambda (x y z) (if (unsafe-fl> (unsafe-fl+ x y) z) 'yes 'no)) 1.2 3.4 5.0)
  
  ;; test unboxing interaction with free variables:
  (test-tri 4.4 '(lambda (x y z) (with-handlers ([exn:fail:contract:variable? 
                                                  (lambda (exn) (unsafe-fl+ x y))])
                                   (unsafe-fl- (unsafe-fl+ x y) NO-SUCH-VARIABLE)))
            1.1 3.3 5.2)

  (test-un 5 'unsafe-car (cons 5 9))
  (test-un 9 'unsafe-cdr (cons 5 9))
  (test-un 15 'unsafe-mcar (mcons 15 19))
  (test-un 19 'unsafe-mcdr (mcons 15 19))
  (let ([v (mcons 3 7)])
    (test-bin 8 'unsafe-set-mcar! v 8 
              #:pre (lambda () (set-mcar! v 0)) 
              #:post (lambda (x) (mcar v))
              #:literal-ok? #f)
    (test-bin 9 'unsafe-set-mcdr! v 9 
              #:pre (lambda () (set-mcdr! v 0)) 
              #:post (lambda (x) (mcdr v))
              #:literal-ok? #f))

  (for ([star (list values (add-star "vector"))])
    (test-un 3 (star 'unsafe-unbox) #&3)
    (let ([b (box 12)])
      (test-tri (list (void) 8) 
                `(lambda (b i val) (,(star 'unsafe-set-box!) b val))
                b 0 8
                #:pre (lambda () (set-box! b 12))
                #:post (lambda (x) (list x (unbox b)))
                #:literal-ok? #f)))

  (for ([star (list values (add-star "vector"))])
    (test-bin 5 (star 'unsafe-vector-ref) #(1 5 7) 1)
    (test-un 3 (star 'unsafe-vector-length) #(1 5 7))
    (let ([v (vector 0 3 7)])
      (test-tri (list (void) 5) (star 'unsafe-vector-set!) v 2 5 
                #:pre (lambda () (vector-set! v 2 0)) 
                #:post (lambda (x) (list x (vector-ref v 2)))
                #:literal-ok? #f)))

  (test-bin 53 'unsafe-bytes-ref #"157" 1)
  (test-un 3 'unsafe-bytes-length #"157")
  (let ([v (bytes 0 3 7)])
    (test-tri (list (void) 135) 'unsafe-bytes-set! v 2 135
              #:pre (lambda () (bytes-set! v 2 0)) 
              #:post (lambda (x) (list x (bytes-ref v 2)))
              #:literal-ok? #f))

  (test-bin #\5 'unsafe-string-ref "157" 1)
  (test-un 3 'unsafe-string-length "157")
  (let ([v (string #\0 #\3 #\7)])
    (test-tri (list (void) #\5) 'unsafe-string-set! v 2 #\5 
              #:pre (lambda () (string-set! v 2 #\0)) 
              #:post (lambda (x) (list x (string-ref v 2)))
              #:literal-ok? #f))

  (test-bin 9.5 'unsafe-flvector-ref (flvector 1.0 9.5 18.7) 1)
  (test-un 5 'unsafe-flvector-length (flvector 1.1 2.0 3.1 4.5 5.7))
  (let ([v (flvector 1.0 9.5 18.7)])
    (test-tri (list (void) 27.4) 'unsafe-flvector-set! v 2 27.4
              #:pre (lambda () (flvector-set! v 2 0.0)) 
              #:post (lambda (x) (list x (flvector-ref v 2)))
              #:literal-ok? #f))

  (test-bin 9.5 'unsafe-f64vector-ref (f64vector 1.0 9.5 18.7) 1)
  (let ([v (f64vector 1.0 9.5 18.7)])
    (test-tri (list (void) 27.4) 'unsafe-f64vector-set! v 2 27.4
              #:pre (lambda () (f64vector-set! v 2 0.0)) 
              #:post (lambda (x) (list x (f64vector-ref v 2)))
              #:literal-ok? #f))

  (for ([star (list values (add-star "star"))])
    (define-struct posn (x [y #:mutable] z))
    (test-bin 'a unsafe-struct-ref (make-posn 'a 'b 'c) 0 #:literal-ok? #f)
    (test-bin 'b unsafe-struct-ref (make-posn 'a 'b 'c) 1 #:literal-ok? #f)
    (let ([p (make-posn 100 200 300)])
      (test-tri 500 'unsafe-struct-set! p 1 500
                #:pre (lambda () (set-posn-y! p 0)) 
                #:post (lambda (x) (posn-y p))
                #:literal-ok? #f)))
  ;; test unboxing:
  (test-tri 5.4 '(lambda (x y z) (unsafe-fl+ x (unsafe-f64vector-ref y z))) 1.2 (f64vector 1.0 4.2 6.7) 1)
  (test-tri 3.2 '(lambda (x y z) 
                   (unsafe-f64vector-set! y 1 (unsafe-fl+ x z))
                   (unsafe-f64vector-ref y 1))
            1.2 (f64vector 1.0 4.2 6.7) 2.0)

  (void))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interaction of unboxing, closures, etc.
(let ([f (lambda (x)
           (let ([x (unsafe-fl+ x 1.0)])
             (let loop ([v 0.0][n 10000])
               (if (zero? n)
                   v
                   (loop (unsafe-fl+ v x)
                         (- n 1))))))])
  (test 20000.0 f 1.0))
(let ([f (lambda (x)
           (let ([x (unsafe-fl+ x 1.0)])
             (let loop ([v 0.0][n 10000][q 2.0])
               (if (zero? n)
                   (unsafe-fl+ v q)
                   (loop (unsafe-fl+ v x)
                         (- n 1)
                         (unsafe-fl- 0.0 q))))))])
  (test 20002.0 f 1.0))
(let ([f (lambda (x)
           (let loop ([a 0.0][v 0.0][n 1000000])
             (if (zero? n)
                 v
                 (if (odd? n)
                     (let ([b (unsafe-fl+ a a)])
                       (loop b v (sub1 n)))
                     ;; First arg is un place, but may need re-boxing
                     (loop a
                           (unsafe-fl+ v x)
                           (- n 1))))))])
  (test 500000.0 f 1.0))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
