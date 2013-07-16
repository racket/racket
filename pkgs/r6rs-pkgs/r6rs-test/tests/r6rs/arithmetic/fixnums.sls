#!r6rs

(library (tests r6rs arithmetic fixnums)
  (export run-arithmetic-fixnums-tests)
  (import (rnrs)
          (tests r6rs test))

  ;; Originally from Ikarus test suite:
  (define (fx*/carry-reference fx1 fx2 fx3)
    (let* ([s (+ (* fx1 fx2) fx3)]
           [s0 (mod0 s (expt 2 (fixnum-width)))]
           [s1 (div0 s (expt 2 (fixnum-width)))])
      (values s0 s1)))
  (define (fx+/carry-reference fx1 fx2 fx3)
    (let* ([s (+ (+ fx1 fx2) fx3)]
           [s0 (mod0 s (expt 2 (fixnum-width)))]
           [s1 (div0 s (expt 2 (fixnum-width)))])
      (values s0 s1)))
  (define (fx-/carry-reference fx1 fx2 fx3)
    (let* ([s (- (- fx1 fx2) fx3)]
           [s0 (mod0 s (expt 2 (fixnum-width)))]
           [s1 (div0 s (expt 2 (fixnum-width)))])
      (values s0 s1)))

  (define (vals->list f a b c)
    (call-with-values (lambda () (f a b c)) list))

  (define-syntax carry-test
    (syntax-rules ()
      [(_ fxop/carry fxop/carry-reference fx1 fx2 fx3)
       (run-test `(fxop/carry ,fx1 ,fx2 ,fx3)
                 (vals->list fxop/carry fx1 fx2 fx3)
                 (vals->list fxop/carry-reference fx1 fx2 fx3))]))

  (define (carry-tests l)
    (for-each 
     (lambda (n)
       (for-each 
        (lambda (m)
          (for-each 
           (lambda (p)
             (carry-test fx*/carry fx*/carry-reference n m p)
             (carry-test fx+/carry fx+/carry-reference n m p)
             (carry-test fx-/carry fx-/carry-reference n m p))
           l))
        l))
     l))

  (define (run-arithmetic-fixnums-tests)

    (test (fxfirst-bit-set 0)         -1)
    (test (fxfirst-bit-set 1)         0)
    (test (fxfirst-bit-set -4)        2)

    (test (fxreverse-bit-field #b1010010 1 4)     88) ; #b1011000

    ;; ----------------------------------------

    (test (least-fixnum) (- (expt 2 (- (fixnum-width) 1))))
    (test (greatest-fixnum) (- (expt 2 (- (fixnum-width) 1)) 1))

    (test (fixnum? 1.0) #f)
    (test (fixnum? 1+1i) #f)

    (test (fixnum? 0) #t)
    (test (fixnum? 1) #t)
    (test (fixnum? -1) #t)
    (test (fixnum? (- (expt 2 23))) #t)
    (test (fixnum? (- (expt 2 23) 1)) #t)

    (test (fixnum? (least-fixnum)) #t)
    (test (fixnum? (- (least-fixnum) 1)) #f)
    (test (fixnum? (greatest-fixnum)) #t)
    (test (fixnum? (+ 1 (greatest-fixnum))) #f)

    (let ([test-ordered
           (lambda (a b c)
             (test (fx=? a a) #t)
             (test (fx=? b b) #t)
             (test (fx=? c c) #t)

             (test (fx=? a b) #f)
             (test (fx=? b a) #f)
             (test (fx=? b c) #f)
             (test (fx=? c b) #f)

             (test (fx=? a c b) #f)
             (test (fx=? a a b) #f)
             (test (fx=? a b b) #f)

             (let ([test-lt
                    (lambda (fx<? fx<=? a b c)
                      (test (fx<? a b) #t)
                      (test (fx<? b c) #t)
                      (test (fx<? a c) #t)
                      (test (fx<? a b c) #t)

                      (test (fx<? b a) #f)
                      (test (fx<? c b) #f)
                      (test (fx<? a c b) #f)
                      
                      (test (fx<=? a a) #t)
                      (test (fx<=? a b) #t)
                      (test (fx<=? a c) #t)
                      (test (fx<=? b b) #t)
                      (test (fx<=? b c) #t)
                      (test (fx<=? c c) #t)
                      (test (fx<=? a c c) #t)
                      (test (fx<=? a b c) #t)
                      (test (fx<=? b b c) #t)

                      (test (fx<=? c a) #f)
                      (test (fx<=? b a) #f)
                      (test (fx<=? a c b) #f)
                      (test (fx<=? b c a) #f))])
               (test-lt fx<? fx<=? a b c)
               (test-lt fx>? fx>=? c b a))
             
             ;; Since b is between a and c, we can add or subtract 1:
             (test (fx=? (+ b 1) (+ b 1)) #t)
             (test (fx<? b (+ b 1)) #t)
             (test (fx<=? b (+ b 1)) #t)
             (test (fx>? b (+ b 1)) #f)
             (test (fx>=? b (+ b 1)) #f)
             (test (fx=? (- b 1) (- b 1)) #t)
             (test (fx<? b (- b 1)) #f)
             (test (fx<=? b (- b 1)) #f)
             (test (fx>? b (- b 1)) #t)
             (test (fx>=? b (- b 1)) #t)
             
             ;; Check min & max while we have ordered values:
             (test (fxmin a b) a)
             (test (fxmin b c) b)
             (test (fxmin a c) a)
             (test (fxmin b a c) a)
             (test (fxmax a b) b)
             (test (fxmax b c) c)
             (test (fxmax a c) c)
             (test (fxmax b c a) c))])
      (test-ordered 1 2 3)
      (test-ordered -1 0 1)
      (test-ordered (least-fixnum) 1 (greatest-fixnum)))

    (test (fxzero? 0) #t)
    (test (fxzero? 1) #f)
    (test (fxzero? (greatest-fixnum)) #f)
    (test (fxzero? (least-fixnum)) #f)

    (test (fxpositive? 0) #f)
    (test (fxpositive? (least-fixnum)) #f)
    (test (fxpositive? (greatest-fixnum)) #t)

    (test (fxnegative? 0) #f)
    (test (fxnegative? (least-fixnum)) #t)
    (test (fxnegative? (greatest-fixnum)) #f)

    (test (fxodd? 0) #f)
    (test (fxodd? 2) #f)
    (test (fxodd? 1) #t)
    (test (fxodd? -1) #t)
    (test (fxodd? (greatest-fixnum)) #t)
    (test (fxodd? (least-fixnum)) #f)

    (test (fxeven? 0) #t)
    (test (fxeven? 2) #t)
    (test (fxeven? 1) #f)
    (test (fxeven? -1) #f)
    (test (fxeven? (greatest-fixnum)) #f)
    (test (fxeven? (least-fixnum)) #t)

    (test (fx+ 3 17) 20)
    (test (fx+ (greatest-fixnum) (least-fixnum)) -1)
    (test (fx+ 0 (greatest-fixnum)) (greatest-fixnum))
    (test (fx+ 0 (least-fixnum)) (least-fixnum))
    (test/exn (fx+ (greatest-fixnum) 1) &implementation-restriction)
    (test/exn (fx+ (least-fixnum) -1) &implementation-restriction)

    (test (fx* 3 17) 51)
    (test (fx* 1 (least-fixnum)) (least-fixnum))
    (test (fx* 1 (greatest-fixnum)) (greatest-fixnum))
    (test (fx* -1 (greatest-fixnum)) (+ (least-fixnum) 1))
    (test/exn (fx* (greatest-fixnum) 2) &implementation-restriction)
    (test/exn (fx* (least-fixnum) -1) &implementation-restriction)
    
    (test (fx- 1) -1)
    (test (fx- -1) 1)
    (test (fx- 0) 0)
    (test (fx- (greatest-fixnum)) (+ 1 (least-fixnum)))

    (test (fx- (greatest-fixnum) 1) (- (greatest-fixnum) 1))
    (test (fx- (greatest-fixnum) (greatest-fixnum)) 0)
    (test (fx- (least-fixnum) (least-fixnum)) 0)

    (test/exn (fx- (least-fixnum)) &implementation-restriction)
    (test/exn (fx- (least-fixnum) 1) &implementation-restriction)

    ;; If you put N numbers here, it runs to O(N^3) tests!
    (carry-tests (list 0 1 2 -1 -2 38734 -3843 2484598 -348732487 
                       (greatest-fixnum) (least-fixnum)))

    (test (fxdiv 123 10) 12)
    (test (fxmod 123 10) 3)
    (test (fxdiv 123 -10) -12)
    (test (fxmod 123 -10) 3)
    (test (fxdiv -123 10) -13)
    (test (fxmod -123 10) 7)
    (test (fxdiv -123 -10) 13)
    (test (fxmod -123 -10) 7)

    (test/values (fxdiv-and-mod -123 10) -13 7)

    (test (fxdiv0 123 10) 12)
    (test (fxmod0 123 10) 3)
    (test (fxdiv0 123 -10) -12)
    (test (fxmod0 123 -10) 3)
    (test (fxdiv0 -123 10) -12)
    (test (fxmod0 -123 10) -3)
    (test (fxdiv0 -123 -10) 12)
    (test (fxmod0 -123 -10) -3)

    (test/values (fxdiv0-and-mod0 -123 10) -12 -3)

    (test/exn (fxdiv 1 0) &assertion)
    (test/exn (fxmod 1 0) &assertion)
    (test/exn (fxdiv-and-mod 1 0) &assertion)
    (test/exn (fxdiv0 1 0) &assertion)
    (test/exn (fxmod0 1 0) &assertion)
    (test/exn (fxdiv0-and-mod0 1 0) &assertion)

    (test/exn (fxdiv (least-fixnum) -1) &implementation-restriction)
    (test/exn (fxdiv-and-mod (least-fixnum) -1) &implementation-restriction)
    (test/exn (fxdiv0 (least-fixnum) -1) &implementation-restriction)
    (test/exn (fxdiv0-and-mod0 (least-fixnum) -1) &implementation-restriction)

    (test (fxnot 0) -1)
    (test (fxnot -2) 1)
    (test (fxnot 1) -2)

    (test (fxand 7) 7)
    (test (fxand 7 0) 0)
    (test (fxand 7 1) 1)
    (test (fxand 7 5) 5)
    (test (fxand 7 4 5) 4)
    (test (fxand 7 5 4) 4)

    (test (fxior 7) 7)
    (test (fxior 7 0) 7)
    (test (fxior 5 4) 5)
    (test (fxior 5 3) 7)
    (test (fxior 5 3 32) 39)

    (test (fxxor 7) 7)
    (test (fxxor 7 0) 7)
    (test (fxxor 5 4) 1)
    (test (fxxor 5 3) 6)
    (test (fxxor 5 1 32) 36)

    (test (fxif 5 15 0) 5)
    (test (fxif 5 0 15) 10)
    (test (fxif 5 0 1) 0)
    (test (fxif 5 0 3) 2)
    (test (fxif 5 3 0) 1)

    (test (fxbit-count 5) 2)
    (test (fxbit-count 6) 2)
    (test (fxbit-count 7) 3)
    (test (fxbit-count -7) -3)

    (test (fxlength 1) 1)
    (test (fxlength 255) 8)
    (test (fxlength 0) 0)
    (test (fxlength -2) 1)
    (test (fxlength -255) 8)

    (test (fxfirst-bit-set 0) -1)
    (test (fxfirst-bit-set 1) 0)
    (test (fxfirst-bit-set 16) 4)
    (test (fxfirst-bit-set -2) 1)
    (test (fxfirst-bit-set (expt 2 17)) 17)

    (test (fxbit-set? 15 0) #t)
    (test (fxbit-set? 14 0) #f)
    (test (fxbit-set? 14 3) #t)
    (test (fxbit-set? 14 10) #f)
    (test (fxbit-set? -1 10) #t)

    (test (fxcopy-bit 0 0 1) 1)
    (test (fxcopy-bit 0 1 1) 2)
    (test (fxcopy-bit 0 4 1) 16)
    (test (fxcopy-bit 0 4 0) 0)
    (test (fxcopy-bit 31 4 0) 15)

    (test (fxbit-field 30 1 3) 3)
    (test (fxbit-field 30 1 4) 7)
    (test (fxbit-field 30 1 5) 15)
    (test (fxbit-field 30 1 6) 15)
    (test (fxbit-field 30 0 3) 6)

    (test (fxcopy-bit-field 0 0 3 30) 6)
    (test (fxcopy-bit-field 7 0 3 30) 6)
    (test (fxcopy-bit-field 15 0 3 30) 14)
    (test (fxcopy-bit-field 0 2 5 30) 24)
    (test (fxcopy-bit-field 1 2 5 30) 25)
    (test (fxcopy-bit-field 7 2 5 30) 27)
    (test (fxcopy-bit-field 15 2 5 30) 27)
    (test (fxcopy-bit-field 0 2 5 120) 0)
    (test (fxcopy-bit-field 1 2 5 120) 1)

    (test (fxarithmetic-shift 1 1) 2)
    (test (fxarithmetic-shift 1 -1) 0)
    (test (fxarithmetic-shift 10 2) 40)
    (test (fxarithmetic-shift 40 -2) 10)
    (test (fxarithmetic-shift -1 1) -2)
    (test (fxarithmetic-shift -1 -1) -1)
    (test (fxarithmetic-shift -10 2) -40)
    (test (fxarithmetic-shift -40 -2) -10)
    (test/exn (fxarithmetic-shift (greatest-fixnum) 1) &implementation-restriction)

    (test (fxarithmetic-shift-left 1 1) 2)
    (test (fxarithmetic-shift-right 1 1) 0)
    (test (fxarithmetic-shift-left 10 2) 40)
    (test (fxarithmetic-shift-right 40 2) 10)
    (test (fxarithmetic-shift-left -1 1) -2)
    (test (fxarithmetic-shift-right -1 1) -1)
    (test (fxarithmetic-shift-left -10 2) -40)
    (test (fxarithmetic-shift-right -40 2) -10)
    (test/exn (fxarithmetic-shift-left (greatest-fixnum) 1) &implementation-restriction)

    (test (fxrotate-bit-field 10 0 2 0) 10)
    (test (fxrotate-bit-field 10 0 2 1) 9)

    (test (fxrotate-bit-field 10 2 4 0) 10)
    (test (fxrotate-bit-field 10 2 4 1) 6)
    (test (fxrotate-bit-field 10 1 4 2) 12)
    (test (fxrotate-bit-field 10 1 4 1) 6)
    (test (fxrotate-bit-field 10 2 4 1) 6)

    ;;
    ))

