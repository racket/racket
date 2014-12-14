
(load-relative "loadtest.rktl")

(Section 'unsafe)

(require racket/unsafe/ops
         racket/flonum
         racket/fixnum
         ffi/vector
         racket/extflonum)

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
    (pre)
    (when lit-ok?
      (pre)
      (test result (compose post (eval `(lambda (y) (,proc ,x y ,z)))) y)
      (pre)
      (test result (compose post (eval `(lambda (z) (,proc ,x ,y z)))) z)
      (pre)
      (test result (compose post (eval `(lambda () (,proc ,x ,y ,z)))))))
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
      (test result (compose post (eval `(lambda (y) (,proc ',x y)))) y)
      (pre)
      (test result (compose post (eval `(lambda () (,proc ',x ',y))))))
    (pre)
    (test result (compose post (eval `(lambda (x) (,proc x ',y)))) x))
  (define (test-un result proc x
                   #:pre [pre void] 
                   #:post [post (lambda (x) x)])
    (pre)
    (test result (compose post (eval proc)) x)
    (pre)
    (test result (compose post (eval `(lambda (x) (,proc x)))) x)
    (pre)
    (test result (compose post (eval `(lambda () (,proc ',x))))))

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
  (err/rt-test (unsafe-fx* 0 (error "bad")) exn:fail?) ; not 0
  (err/rt-test (unsafe-fx* (error "bad") 0) exn:fail?) ; not 0
  
  (test-bin 3 'unsafe-fxquotient 17 5)
  (test-bin -3 'unsafe-fxquotient 17 -5)
  (test-bin 0 'unsafe-fxquotient 0 -5)
  (test-bin 18 'unsafe-fxquotient 18 1)
  (err/rt-test (unsafe-fxquotient 0 (error "bad")) exn:fail?) ; not 0

  (test-bin 2 'unsafe-fxremainder 17 5)
  (test-bin 2 'unsafe-fxremainder 17 -5)
  (test-bin 0 'unsafe-fxremainder 0 -5)
  (test-bin 0 'unsafe-fxremainder 10 1)
  (err/rt-test (unsafe-fxremainder (error "bad") 1) exn:fail?) ; not 0

  (test-bin 2 'unsafe-fxmodulo 17 5)
  (test-bin -3 'unsafe-fxmodulo 17 -5)
  (test-bin 0 'unsafe-fxmodulo 0 -5)
  (test-bin 0 'unsafe-fxmodulo 10 1)
  (err/rt-test (unsafe-fxmodulo (error "bad") 1) exn:fail?) ; not 0
  (err/rt-test (unsafe-fxmodulo 0 (error "bad")) exn:fail?) ; not 0

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

  (when (extflonum-available?)
    (test-bin 3.4t0 'unsafe-extfl+ 1.4t0 2.0t0)
    (test-bin -1.0999999999999999999t0 'unsafe-extfl+ 1.0t0 -2.1t0)
    (test-bin +inf.t 'unsafe-extfl+ 1.0t0 +inf.t)
    (test-bin -inf.t 'unsafe-extfl+ 1.0t0 -inf.t)
    (test-bin +nan.t 'unsafe-extfl+ +nan.t -inf.t)
    (test-bin 1.5t0 'unsafe-extfl+ 1.5t0 0.0t0)
    (test-bin 1.7t0 'unsafe-extfl+ 0.0t0 1.7t0)

    (test-bin 7.9t0 'unsafe-extfl- 10.0t0 2.1t0)
    (test-bin 3.7t0 'unsafe-extfl- 1.0t0 -2.7t0)
    (test-bin 1.5t0 'unsafe-extfl- 1.5t0 0.0t0)

    (test-bin 20.002t0 'unsafe-extfl* 10.001t0 2.0t0)
    (test-bin -20.002t0 'unsafe-extfl* 10.001t0 -2.0t0)
    (test-bin +nan.t 'unsafe-extfl* +inf.t 0.0t0)
    (test-bin 1.8t0 'unsafe-extfl* 1.0t0 1.8t0)
    (test-bin 1.81t0 'unsafe-extfl* 1.81t0 1.0t0)
    
    (test-bin (real->extfl 17/5) 'unsafe-extfl/ 17.0t0 5.0t0)
    (test-bin +inf.t 'unsafe-extfl/ 17.0t0 0.0t0)
    (test-bin -inf.t 'unsafe-extfl/ -17.0t0 0.0t0)
    (test-bin 1.5t0 'unsafe-extfl/ 1.5t0 1.0t0)

    (test-un 5.0t0 unsafe-extflabs 5.0t0)
    (test-un 5.0t0 unsafe-extflabs -5.0t0)
    (test-un 0.0t0 unsafe-extflabs -0.0t0)
    (test-un +inf.t unsafe-extflabs -inf.t)

    (test-un 5.0t0 unsafe-extflsqrt 25.0t0)
    (test-un 0.5t0 unsafe-extflsqrt 0.25t0)
    (test-un +nan.t unsafe-extflsqrt -1.0t0)

    (test-un 8.0t0 'unsafe-fx->extfl 8)
    (test-un -8.0t0 'unsafe-fx->extfl -8)

    (test-un 8 'unsafe-extfl->fx 8.0t0)
    (test-un -8 'unsafe-extfl->fx -8.0t0)

    (test-bin 3.7t0 'unsafe-extflmin 3.7t0 4.1t0)
    (test-bin 2.1t0 'unsafe-extflmin 3.7t0 2.1t0)
    (test-bin +nan.t 'unsafe-extflmin +nan.t 2.1t0)
    (test-bin +nan.t 'unsafe-extflmin 2.1t0 +nan.t)
    (test-bin 3.7t0 'unsafe-extflmax 3.7t0 2.1t0)
    (test-bin 4.1t0 'unsafe-extflmax 3.7t0 4.1t0)
    (test-bin +nan.t 'unsafe-extflmax +nan.t 2.1t0)
    (test-bin +nan.t 'unsafe-extflmax 2.1t0 +nan.t))

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

  (test-un 8 'unsafe-fl->fx 8.0)
  (test-un -8 'unsafe-fl->fx -8.0)

  (test-bin 3.7 'unsafe-flmin 3.7 4.1)
  (test-bin 2.1 'unsafe-flmin 3.7 2.1)
  (test-bin +nan.0 'unsafe-flmin +nan.0 2.1)
  (test-bin +nan.0 'unsafe-flmin 2.1 +nan.0)
  (test-bin 3.7 'unsafe-flmax 3.7 2.1)
  (test-bin 4.1 'unsafe-flmax 3.7 4.1)
  (test-bin +nan.0 'unsafe-flmax +nan.0 2.1)
  (test-bin +nan.0 'unsafe-flmax 2.1 +nan.0)

  (test-bin 1.7+45.0i 'unsafe-make-flrectangular 1.7 45.0)
  (test-un 3.5 'unsafe-flreal-part 3.5+4.6i)
  (test-un 4.6 'unsafe-flimag-part 3.5+4.6i)

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

  (when (extflonum-available?)
    (test-tri 9.0t0 '(lambda (x y z) (unsafe-extfl+ (unsafe-extfl- x z) y)) 4.5t0 7.0t0 2.5t0)
    (test-tri 9.0t0 '(lambda (x y z) (unsafe-extfl+ y (unsafe-extfl- x z))) 4.5t0 7.0t0 2.5t0)
    (test-bin 10.0t0 '(lambda (x y) (unsafe-extfl+ (unsafe-fx->extfl x) y)) 2 8.0t0)
    (test-bin 10.0t0 '(lambda (x y) (unsafe-extfl+ (unsafe-fx->extfl x) y)) 2 8.0t0)
    (test-bin 9.5t0 '(lambda (x y) (unsafe-extfl+ (unsafe-extflabs x) y)) -2.0t0 7.5t0)
    (test-tri (unsafe-extfl/ 20.0t0 0.8t0) '(lambda (x y z) (unsafe-extfl/ (unsafe-extfl* x z) y)) 4.0t0 0.8t0 5.0t0)
    (test-tri (unsafe-extfl/ 0.8t0 20.0t0) '(lambda (x y z) (unsafe-extfl/ y (unsafe-extfl* x z))) 4.0t0 0.8t0 5.0t0)
    
    (test-tri #t '(lambda (x y z) (unsafe-extfl< (unsafe-extfl+ x y) z)) 1.2t0 3.4t0 5.0t0)
    (test-tri 'yes '(lambda (x y z) (if (unsafe-extfl< (unsafe-extfl+ x y) z) 'yes 'no)) 1.2t0 3.4t0 5.0t0)
    (test-tri #f '(lambda (x y z) (unsafe-extfl> (unsafe-extfl+ x y) z)) 1.2t0 3.4t0 5.0t0)
    (test-tri 'no '(lambda (x y z) (if (unsafe-extfl> (unsafe-extfl+ x y) z) 'yes 'no)) 1.2t0 3.4t0 5.0t0))
  
  ;; test unboxing interaction with free variables:
  (test-tri 4.4 '(lambda (x y z) (with-handlers ([exn:fail:contract:variable? 
                                                  (lambda (exn) (unsafe-fl+ x y))])
                                   (unsafe-fl- (unsafe-fl+ x y) NO-SUCH-VARIABLE)))
            1.1 3.3 5.2)

  (when (extflonum-available?)
    (test-tri 4.4t0 '(lambda (x y z) (with-handlers ([exn:fail:contract:variable? 
                                                      (lambda (exn) (unsafe-extfl+ x y))])
                                       (unsafe-extfl- (unsafe-extfl+ x y) NO-SUCH-VARIABLE)))
              1.1t0 3.3t0 5.2t0))
  
  (let ([r (make-pseudo-random-generator)]
        [seed (random 100000)])
    (define (reset)
      (parameterize ([current-pseudo-random-generator r])
        (random-seed seed)))
    (reset)
    (define val (random r))
    (test-un val 'unsafe-flrandom r
             #:pre reset))

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
  (test-bin 5 'unsafe-list-ref (cons 5 9) 0)
  (test-bin 8 'unsafe-list-ref (cons 5 (cons 8 9)) 1)
  (test-bin 9 'unsafe-list-ref (cons 5 (cons 8 (cons 9 10))) 2)
  (test-bin (cons 5 9) 'unsafe-list-tail (cons 5 9) 0)
  (test-bin 3 'unsafe-list-tail 3 0)
  (test-bin 9 'unsafe-list-tail (cons 5 9) 1)
  (test-bin 8 'unsafe-list-tail (cons 5 (cons 9 8)) 2)

  (for ([star (list values (add-star "box"))])
    (test-un 3 (star 'unsafe-unbox) #&3)
    (let ([b (box 12)])
      (test-tri (list (void) 8) 
                `(lambda (b i val) (,(star 'unsafe-set-box!) b val))
                b 0 8
                #:pre (lambda () (set-box! b 12))
                #:post (lambda (x) (list x (unbox b)))
                #:literal-ok? #f)))
  (test-un 3 'unsafe-unbox (chaperone-box (box 3)
                                          (lambda (b v) v)
                                          (lambda (b v) v)))

  (let ([b (box 0)]
        [b2 (box 1)])    
    ;; success
    (test-tri (list #true 1)
              'unsafe-box*-cas! b 0 1
              #:pre (lambda () (set-box! b 0))
              #:post (lambda (x) (list x (unbox b)))
              #:literal-ok? #f)
    ;; failure
    (test-tri (list #false 1)
              'unsafe-box*-cas! b2 0 7
              #:pre (lambda () (set-box! b2 1))
              #:post (lambda (x) (list x (unbox b2)))
              #:literal-ok? #f))
  
  (for ([star (list values (add-star "vector"))])
    (test-bin 5 (star 'unsafe-vector-ref) #(1 5 7) 1)
    (test-un 3 (star 'unsafe-vector-length) #(1 5 7))
    (let ([v (vector 0 3 7)])
      (test-tri (list (void) 5) (star 'unsafe-vector-set!) v 2 5 
                #:pre (lambda () (vector-set! v 2 0)) 
                #:post (lambda (x) (list x (vector-ref v 2)))
                #:literal-ok? #f)))
  (test-bin 5 'unsafe-vector-ref (chaperone-vector #(1 5 7)
                                                   (lambda (v i x) x)
                                                   (lambda (v i x) x))
            1)
  (test-un 3 'unsafe-vector-length (chaperone-vector #(1 5 7)
                                                     (lambda (v i x) x)
                                                     (lambda (v i x) x)))

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

  (when (extflonum-available?)
    (test-bin 9.5t0 'unsafe-extflvector-ref (extflvector 1.0t0 9.5t0 18.7t0) 1)
    (test-un 5 'unsafe-extflvector-length (extflvector 1.1t0 2.0t0 3.1t0 4.5t0 5.7t0))
    (let ([v (extflvector 1.0t0 9.5t0 18.7t0)])
      (test-tri (list (void) 27.4t0) 'unsafe-extflvector-set! v 2 27.4t0
                #:pre (lambda () (extflvector-set! v 2 0.0t0))
                #:post (lambda (x) (list x (extflvector-ref v 2)))
                #:literal-ok? #f))

    (test-bin 9.5t0 'unsafe-f80vector-ref (f80vector 1.0t0 9.5t0 18.7t0) 1)
    (let ([v (f80vector 1.0t0 9.5t0 18.7t0)])
      (test-tri (list (void) 27.4t0) 'unsafe-f80vector-set! v 2 27.4t0
                #:pre (lambda () (f80vector-set! v 2 0.0t0)) 
                #:post (lambda (x) (list x (f80vector-ref v 2)))
                #:literal-ok? #f))
    )

  (test-bin 95 'unsafe-fxvector-ref (fxvector 10 95 187) 1)
  (test-un 5 'unsafe-fxvector-length (fxvector 11 20 31 45 57))
  (let ([v (fxvector 10 95 187)])
    (test-tri (list (void) 274) 'unsafe-fxvector-set! v 2 274
              #:pre (lambda () (fxvector-set! v 2 0)) 
              #:post (lambda (x) (list x (fxvector-ref v 2)))
              #:literal-ok? #f))

  (test-bin 95 'unsafe-s16vector-ref (s16vector 10 95 187) 1)
  (let ([v (s16vector 10 95 187)])
    (test-tri (list (void) 274) 'unsafe-s16vector-set! v 2 274
              #:pre (lambda () (s16vector-set! v 2 0)) 
              #:post (lambda (x) (list x (s16vector-ref v 2)))
              #:literal-ok? #f))
  (test-bin -32768 'unsafe-s16vector-ref (s16vector 10 -32768 187) 1)
  (test-bin 32767 'unsafe-s16vector-ref (s16vector 10 32767 187) 1)

  (test-bin 95 'unsafe-u16vector-ref (u16vector 10 95 187) 1)
  (let ([v (u16vector 10 95 187)])
    (test-tri (list (void) 274) 'unsafe-u16vector-set! v 2 274
              #:pre (lambda () (u16vector-set! v 2 0)) 
              #:post (lambda (x) (list x (u16vector-ref v 2)))
              #:literal-ok? #f))
  (test-bin 65535 'unsafe-u16vector-ref (u16vector 10 65535 187) 1)

  (let ()
    (define (try-struct prop prop-val)
      (define-struct posn (x [y #:mutable] z)
        #:property prop prop-val)
      (for ([star (list values (add-star "star"))])
        (test-bin 'a unsafe-struct-ref (make-posn 'a 'b 'c) 0 #:literal-ok? #f)
        (test-bin 'b unsafe-struct-ref (make-posn 'a 'b 'c) 1 #:literal-ok? #f)
        (let ([p (make-posn 100 200 300)])
          (test-tri 500 (star 'unsafe-struct-set!) p 1 500
                    #:pre (lambda () (set-posn-y! p 0)) 
                    #:post (lambda (x) (posn-y p))
                    #:literal-ok? #f)))
      (let ([p (chaperone-struct (make-posn 100 200 300)
                                 posn-y (lambda (p v) v)
                                 set-posn-y! (lambda (p v) v))])
        (test-tri 500 'unsafe-struct-set! p 1 500
                  #:pre (lambda () (set-posn-y! p 0)) 
                  #:post (lambda (x) (posn-y p))
                  #:literal-ok? #f)))
    (define-values (prop:nothing nothing? nothing-ref) (make-struct-type-property 'nothing))
    (try-struct prop:nothing 5)
    (try-struct prop:procedure (lambda (s) 'hi!)))
    
  ;; test unboxing:
  (test-tri 5.4 '(lambda (x y z) (unsafe-fl+ x (unsafe-f64vector-ref y z))) 1.2 (f64vector 1.0 4.2 6.7) 1)
  (test-tri 3.2 '(lambda (x y z) 
                   (unsafe-f64vector-set! y 1 (unsafe-fl+ x z))
                   (unsafe-f64vector-ref y 1))
            1.2 (f64vector 1.0 4.2 6.7) 2.0)

  (when (extflonum-available?)
    (test-tri 5.3999999999999999997t0 '(lambda (x y z) (unsafe-extfl+ x (unsafe-f80vector-ref y z))) 1.2t0 (f80vector 1.0t0 4.2t0 6.7t0) 1)
    (test-tri 3.2t0 '(lambda (x y z) 
                       (unsafe-f80vector-set! y 1 (unsafe-extfl+ x z))
                       (unsafe-f80vector-ref y 1))
              1.2t0 (f80vector 1.0t0 4.2t0 6.7t0) 2.0t0))

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

(when (extflonum-available?) 
  (let ([f (lambda (x)
             (let ([x (unsafe-extfl+ x 1.0t0)])
               (let loop ([v 0.0t0][n 10000])
                 (if (zero? n)
                     v
                     (loop (unsafe-extfl+ v x)
                           (- n 1))))))])
    (test 20000.0t0 f 1.0t0))
  (let ([f (lambda (x)
             (let ([x (unsafe-extfl+ x 1.0t0)])
               (let loop ([v 0.0t0][n 10000][q 2.0t0])
                 (if (zero? n)
                     (unsafe-extfl+ v q)
                     (loop (unsafe-extfl+ v x)
                           (- n 1)
                           (unsafe-extfl- 0.0t0 q))))))])
    (test 20002.0t0 f 1.0t0))
  (let ([f (lambda (x)
             (let loop ([a 0.0t0][v 0.0t0][n 1000000])
               (if (zero? n)
                   v
                   (if (odd? n)
                       (let ([b (unsafe-extfl+ a a)])
                         (loop b v (sub1 n)))
                       ;; First arg is un place, but may need re-boxing
                       (loop a
                             (unsafe-extfl+ v x)
                             (- n 1))))))])
    (test 500000.0t0 f 1.0t0)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check that compiling a misapplication of `unsafe-car' and `unsafe-cdr'
;; (which are folding operations in the compiler ) doesn't crash:

(let ([f (lambda (x) (if x x (unsafe-car 3)))]
      [g (lambda (x) (if x x (unsafe-cdr 4)))])
  (test 5 f 5)
  (test 5 g 5))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A regression test to check that unsafe-fl/ doesn't
;; reorder its arguments when it isn't safe to do so, where the
;; unsafeness of the reordering has to do with safe-for-space
;; clearing of a variable that is used multiple times.

(let ()
  (define weird #f)
  (set! weird 
        (lambda (get-M)
          (let* ([M  (get-M)]
                 [N1 (unsafe-fl/ M (unsafe-fllog M))])
            (get-M) ; triggers safe-for-space clearing of M
            N1)))
  
  (test 15388.0 floor (* 1000.0 (weird (lambda () 64.0)))))

(when (extflonum-available?)
  (define weird #f)
  (set! weird 
        (lambda (get-M)
          (let* ([M  (get-M)]
                 [N1 (unsafe-extfl/ M (unsafe-extfllog M))])
            (get-M) ; triggers safe-for-space clearing of M
            N1)))
  (test 15388.0t0 unsafe-extflfloor (unsafe-extfl* 1000.0t0 (weird (lambda () 64.0t0)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
