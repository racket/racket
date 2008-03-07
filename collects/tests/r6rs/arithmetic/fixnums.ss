#!r6rs

(library (tests r6rs arithmetic fixnums)
  (export run-arithmetic-fixnums-tests)
  (import (rnrs)
          (tests r6rs test))

  (define (run-arithmetic-fixnums-tests)

    (test/exn (fx- (least-fixnum)) &implementation-restriction)

    (test (fxfirst-bit-set 0)         -1)
    (test (fxfirst-bit-set 1)         0)
    (test (fxfirst-bit-set -4)        2)

    (test (fxreverse-bit-field #b1010010 1 4)     88) ; #b1011000

    ;; ----------------------------------------

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
    (test (fx* 3 17) 51)
    (test (fx* 1 (least-fixnum)) (least-fixnum))
    (test (fx* 1 (greatest-fixnum)) (greatest-fixnum))
    (test (fx* -1 (greatest-fixnum)) (+ (least-fixnum) 1))
    
    (test (fx- 1) -1)
    (test (fx- -1) 1)
    (test (fx- 0) 0)
    (test (fx- (greatest-fixnum)) (+ 1 (least-fixnum)))

    (test (fx- (greatest-fixnum) 1) (- (greatest-fixnum) 1))
    (test (fx- (greatest-fixnum) (greatest-fixnum)) 0)
    (test (fx- (least-fixnum) (least-fixnum)) 0)

    ;;
    ))

