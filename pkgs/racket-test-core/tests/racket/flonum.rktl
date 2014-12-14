(load-relative "loadtest.rktl")

(Section 'flonum)

(require racket/flonum
         "for-util.rkt")

(define (flonum-close? fl1 fl2)
  (<= (flabs (fl- fl1 fl2))
      1e-8))

;; in-flvector tests.
(let ((flv (flvector 1.0 2.0 3.0)))
  (let ((flv-seq (in-flvector flv)))
    (for ((x (in-flvector flv))
          (xseq flv-seq)
          (i (in-naturals)))
      (test (+ i 1.0) 'in-flvector-fast x)
      (test (+ i 1.0) 'in-flvector-sequence xseq))))

;; for/flvector test
(let ((flv (flvector 1.0 2.0 3.0))
      (flv1 (for/flvector ((i (in-range 3))) (+ i 1.0)))
      (flv2 (for/flvector #:length 3 ((i (in-range 3))) (+ i 1.0))))
  (test flv 'for/flvector flv1)
  (test flv 'for/flvector-fast flv2))

(test (flvector 1.0 2.0 3.0 0.0 0.0)
      'for/flvector-fill
      (for/flvector #:length 5 ([i 3]) (+ i 1.0)))
(test (flvector 1.0 2.0 3.0 -10.0 -10.0)
      'for/flvector-fill
      (for/flvector #:length 5 #:fill -10.0 ([i 3]) (+ i 1.0)))
(test (flvector 1.0 2.0 3.0 0.0 0.0)
      'for/flvector-fill
      (for/flvector #:length 5 ([i 5]) #:break (= i 3) (+ i 1.0)))
(test (flvector 1.0 2.0 3.0 4.0 0.0)
      'for/flvector-fill
      (for/flvector #:length 5 ([i 5]) #:final (= i 3) (+ i 1.0)))

;; for*/flvector test
(let ((flv (flvector 0.0 0.0 0.0 0.0 1.0 2.0 0.0 2.0 4.0))
      (flv1 (for*/flvector ((i (in-range 3)) (j (in-range 3))) (exact->inexact (* 1.0 i j))))
      (flv2 (for*/flvector #:length 9 ((i (in-range 3)) (j (in-range 3))) (exact->inexact (* 1.0 i j)))))
  (test flv 'for*/flvector flv1)
  (test flv 'for*/flvector-fast flv2))

;; Stop when a length is specified, even if the sequence continues:
(test (flvector 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0)
      'nat
      (for/flvector #:length 10 ([i (in-naturals)]) (exact->inexact i)))
(test (flvector 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0)
      'nats
      (for*/flvector #:length 10 ([i (in-naturals)] [j (in-naturals)]) (exact->inexact j)))
(test (flvector 0.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0)
      'nat+5
      (for*/flvector #:length 10 ([i (in-naturals)] [j (in-range 5)]) (exact->inexact i)))


;; Test for both length too long and length too short
(let ((v (make-flvector 3)))
  (flvector-set! v 0 0.0)
  (flvector-set! v 1 1.0)
  (let ((w (for/flvector #:length 3 ((i (in-range 2))) (exact->inexact i))))
    (test v 'for/flvector-short-iter w)))

(let ((v (make-flvector 10)))
  (for* ((i (in-range 3))
         (j (in-range 3)))
    (flvector-set! v (+ j (* i 3)) (+ 1.0 i j)))
  (let ((w (for*/flvector #:length 10 ((i (in-range 3)) (j (in-range 3))) (+ 1.0 i j))))
    (test v 'for*/flvector-short-iter w)))

(test 2 'for/flvector-long-iter
      (flvector-length (for/flvector #:length 2 ((i (in-range 10))) (exact->inexact i))))
(test 5 'for*/flvector-long-iter 
      (flvector-length (for*/flvector #:length 5 ((i (in-range 3)) (j (in-range 3))) (exact->inexact (+ i j)))))

;; Test for many body expressions
(let* ((flv (flvector 1.0 2.0 3.0))
       (flv2 (for/flvector ((i (in-range 3))) 
               (flvector-set! flv i (+ (flvector-ref flv i) 1.0))
               (flvector-ref flv i)))
       (flv3 (for/flvector #:length 3 ((i (in-range 3)))
               (flvector-set! flv i (+ (flvector-ref flv i) 1.0))
               (flvector-ref flv i))))
  (test (flvector 2.0 3.0 4.0) 'for/flvector-many-body flv2)
  (test (flvector 3.0 4.0 5.0) 'for/flvector-length-many-body flv3))

;; flvector-copy test
(let ((v (flvector 0.0 1.0 2.0 3.0)))
  (let ((vc (flvector-copy v)))
    (test (flvector-length v) 'flvector-copy (flvector-length vc))
    (for ((vx (in-flvector v))
          (vcx (in-flvector vc)))
      (test vx 'flvector-copy vcx))
    (flvector-set! vc 2 -10.0)
    (test 2.0 'flvector-copy (flvector-ref v 2))
    (test -10.0 'flvector-copy (flvector-ref vc 2))
    (test '(2.0 3.0) 'flvector-copy (for/list ([i (in-flvector (flvector-copy v 2))]) i))
    (test '(2.0) 'flvector-copy (for/list ([i (in-flvector (flvector-copy v 2 3))]) i))))

;; Check empty clauses
(let ()
  (define vector-iters 0)
  (test (flvector 3.4 0.0 0.0 0.0)
        'no-clauses
        (for/flvector #:length 4 ()
                      (set! vector-iters (+ 1 vector-iters))
                      3.4))
  (test 1 values vector-iters)
  (test (flvector 3.4 0.0 0.0 0.0)
        'no-clauses
        (for*/flvector #:length 4 ()
                       (set! vector-iters (+ 1 vector-iters))
                       3.4))
  (test 2 values vector-iters))

;; Check #:when and #:unless:
(test (flvector 0.0 1.0 2.0 1.0 2.0)
      'when-#t
      (for/flvector #:length 5
                    ([x (in-range 3)]
                     #:when #t
                     [y (in-range 3)])
         (exact->inexact (+ x y))))
(test (flvector 0.0 1.0 2.0 2.0 3.0)
      'when-...
      (for/flvector #:length 5
                    ([x (in-range 3)]
                     #:when (even? x)
                     [y (in-range 3)])
        (exact->inexact (+ x y))))
(test (flvector 0.0 1.0 2.0 1.0 2.0)
      'unless-#f
      (for/flvector #:length 5
                    ([x (in-range 3)]
                     #:unless #f
                     [y (in-range 3)])
        (exact->inexact (+ x y))))
(test (flvector 1.0 2.0 3.0 0.0 0.0)
      'unless-...
      (for/flvector #:length 5
                    ([x (in-range 3)]
                     #:unless (even? x)
                     [y (in-range 3)])
        (exact->inexact (+ x y))))


;; in-flvector tests, copied from for.rktl

(test-sequence [(1.0 2.0 3.0)] (in-flvector (flvector 1.0 2.0 3.0)))
(test-sequence [(2.0 3.0 4.0)] (in-flvector (flvector 1.0 2.0 3.0 4.0) 1))
(test-sequence [(2.0 3.0 4.0)] (in-flvector (flvector 1.0 2.0 3.0 4.0 5.0) 1 4))
(test-sequence [(2.0 4.0 6.0)] (in-flvector (flvector 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0) 1 7 2))
(test-sequence [(8.0 6.0 4.0)] (in-flvector (flvector 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0) 7 1 -2))
(test-sequence [(2.0 4.0 6.0)] (in-flvector (flvector 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0) 1 6 2))
(test-sequence [(8.0 6.0 4.0)] (in-flvector (flvector 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0) 7 2 -2))

;; flvector sequence tests

(test-sequence [(1.0 2.0 3.0)] (flvector 1.0 2.0 3.0))
(test '() 'empty-flvector-sequence (for/list ([i (flvector)]) i))

;; Check safety:
(err/rt-test (for/flvector ([i 5]) 8))
(err/rt-test (for/flvector #:length 5 ([i 5]) 8))
(err/rt-test (for/flvector #:length 5 #:fill 0.0 ([i 5]) 8))
(err/rt-test (for/flvector #:length 5 #:fill 0 ([i 5]) 8.0))
(err/rt-test (for/flvector #:length 10 #:fill 0 ([i 5]) 8.0))

;; ----------------------------------------
;; flrandom

(let ([r (make-pseudo-random-generator)]
      [seed (random 100000)])
  (define (reset)
    (parameterize ([current-pseudo-random-generator r])
      (random-seed seed)))
  (test (begin (reset) (random r))
        flrandom
        (begin (reset) r)))

(err/rt-test (flrandom 5.0))

;; ----------------------------------------
;; Check corners of `flexpt':
;;  Tests by Neil T.:

(let ()
  (define-syntax-rule (check-equal? (flexpt v1 v2) b)
    (test b flexpt v1 v2))
  
  ;; 2^53 and every larger flonum is even:
  (define +big-even.0 (expt 2.0 53))
  ;; The largest odd flonum:
  (define +max-odd.0 (- +big-even.0 1.0))

  (define -big-even.0 (- +big-even.0))
  (define -max-odd.0 (- +max-odd.0))

  (check-equal? (flexpt +0.0 +0.0) +1.0)
  (check-equal? (flexpt +0.0 +1.0) +0.0)
  (check-equal? (flexpt +0.0 +3.0) +0.0)
  (check-equal? (flexpt +0.0 +max-odd.0) +0.0)
  (check-equal? (flexpt +0.0 +0.5) +0.0)
  (check-equal? (flexpt +0.0 +1.5) +0.0)
  (check-equal? (flexpt +0.0 +2.0) +0.0)
  (check-equal? (flexpt +0.0 +2.5) +0.0)
  (check-equal? (flexpt +0.0 +big-even.0) +0.0)

  (check-equal? (flexpt -0.0 +0.0) +1.0)
  (check-equal? (flexpt -0.0 +1.0) -0.0)
  (check-equal? (flexpt -0.0 +3.0) -0.0)
  (check-equal? (flexpt -0.0 +max-odd.0) -0.0)
  (check-equal? (flexpt -0.0 +0.5) +0.0)
  (check-equal? (flexpt -0.0 +1.5) +0.0)
  (check-equal? (flexpt -0.0 +2.0) +0.0)
  (check-equal? (flexpt -0.0 +2.5) +0.0)
  (check-equal? (flexpt -0.0 +big-even.0) +0.0)

  (check-equal? (flexpt +1.0 +0.0) +1.0)
  (check-equal? (flexpt +1.0 +0.5) +1.0)
  (check-equal? (flexpt +1.0 +inf.0) +1.0)

  (check-equal? (flexpt -1.0 +0.0) +1.0)
  (check-equal? (flexpt -1.0 +0.5) +nan.0)
  (check-equal? (flexpt -1.0 +inf.0) +1.0)

  (check-equal? (flexpt +0.5 +inf.0) +0.0)
  (check-equal? (flexpt +1.5 +inf.0) +inf.0)

  (check-equal? (flexpt +inf.0 +0.0) +1.0)
  (check-equal? (flexpt +inf.0 +1.0) +inf.0)
  (check-equal? (flexpt +inf.0 +2.0) +inf.0)
  (check-equal? (flexpt +inf.0 +inf.0) +inf.0)

  (check-equal? (flexpt -inf.0 +0.0) +1.0)
  (check-equal? (flexpt -inf.0 +1.0) -inf.0)
  (check-equal? (flexpt -inf.0 +3.0) -inf.0)
  (check-equal? (flexpt -inf.0 +max-odd.0) -inf.0)
  (check-equal? (flexpt -inf.0 +0.5) +inf.0)
  (check-equal? (flexpt -inf.0 +1.5) +inf.0)
  (check-equal? (flexpt -inf.0 +2.0) +inf.0)
  (check-equal? (flexpt -inf.0 +2.5) +inf.0)
  (check-equal? (flexpt -inf.0 +big-even.0) +inf.0)
  (check-equal? (flexpt -inf.0 +inf.0) +inf.0)

  ;; Same tests as above, but with negated y
  ;; This identity should hold for these tests: (flexpt x y) = (/ 1.0 (flexpt x (- y)))

  (check-equal? (flexpt +0.0 -0.0) +1.0)
  (check-equal? (flexpt +0.0 -1.0) +inf.0)
  (check-equal? (flexpt +0.0 -3.0) +inf.0)
  (check-equal? (flexpt +0.0 -max-odd.0) +inf.0)
  (check-equal? (flexpt +0.0 -0.5) +inf.0)
  (check-equal? (flexpt +0.0 -1.5) +inf.0)
  (check-equal? (flexpt +0.0 -2.0) +inf.0)
  (check-equal? (flexpt +0.0 -2.5) +inf.0)
  (check-equal? (flexpt +0.0 -big-even.0) +inf.0)

  (check-equal? (flexpt -0.0 -0.0) +1.0)
  (check-equal? (flexpt -0.0 -1.0) -inf.0)
  (check-equal? (flexpt -0.0 -3.0) -inf.0)
  (check-equal? (flexpt -0.0 -max-odd.0) -inf.0)
  (check-equal? (flexpt -0.0 -0.5) +inf.0)
  (check-equal? (flexpt -0.0 -1.5) +inf.0)
  (check-equal? (flexpt -0.0 -2.0) +inf.0)
  (check-equal? (flexpt -0.0 -2.5) +inf.0)
  (check-equal? (flexpt -0.0 -big-even.0) +inf.0)

  (check-equal? (flexpt +1.0 -0.0) +1.0)
  (check-equal? (flexpt +1.0 -0.5) +1.0)
  (check-equal? (flexpt +1.0 -inf.0) +1.0)

  (check-equal? (flexpt -1.0 -0.0) +1.0)
  (check-equal? (flexpt -1.0 -0.5) +nan.0)
  (check-equal? (flexpt -1.0 -inf.0) +1.0)

  (check-equal? (flexpt +0.5 -inf.0) +inf.0)
  (check-equal? (flexpt +1.5 -inf.0) +0.0)

  (check-equal? (flexpt +inf.0 -0.0) +1.0)
  (check-equal? (flexpt +inf.0 -1.0) +0.0)
  (check-equal? (flexpt +inf.0 -2.0) +0.0)
  (check-equal? (flexpt +inf.0 -inf.0) +0.0)

  (check-equal? (flexpt -inf.0 -0.0) +1.0)
  (check-equal? (flexpt -inf.0 -1.0) -0.0)
  (check-equal? (flexpt -inf.0 -3.0) -0.0)
  (check-equal? (flexpt -inf.0 -max-odd.0) -0.0)
  (check-equal? (flexpt -inf.0 -0.5) +0.0)
  (check-equal? (flexpt -inf.0 -1.5) +0.0)
  (check-equal? (flexpt -inf.0 -2.0) +0.0)
  (check-equal? (flexpt -inf.0 -2.5) +0.0)
  (check-equal? (flexpt -inf.0 -big-even.0) +0.0)
  (check-equal? (flexpt -inf.0 -inf.0) +0.0)

  ;; NaN input

  (check-equal? (flexpt +nan.0 +0.0) +1.0)
  (check-equal? (flexpt +nan.0 -0.0) +1.0)
  (check-equal? (flexpt +1.0 +nan.0) +1.0)
  (check-equal? (flexpt -1.0 +nan.0) +nan.0))

;; ----------------------------------------

(report-errs)
