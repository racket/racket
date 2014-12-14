;; copypaste with small modifications from flonum.rkt
(load-relative "loadtest.rktl")

(Section 'extflonum)

(require racket/extflonum
         "for-util.rkt")

(when (extflonum-available?)
  ;; ----------------------------------------

  (define (extflonum-close? fl1 fl2)
    (extfl<= (extflabs (fl- fl1 fl2))
             (real->extfl 1e-8)))

  ;; in-extflvector tests.
  (let ((flv (extflvector 1.0t0 2.0t0 3.0t0)))
    (let ((flv-seq (in-extflvector flv)))
      (for ((x (in-extflvector flv))
            (xseq flv-seq)
            (i (in-naturals)))
        (test (->extfl (+ i 1)) 'in-extflvector-fast x)
        (test (->extfl (+ i 1)) 'in-extflvector-sequence xseq))))

  ;; for/extflvector test
  (let ((flv (extflvector 1.0t0 2.0t0 3.0t0))
        (flv1 (for/extflvector ((i (in-range 3))) (->extfl (+ i 1))))
        (flv2 (for/extflvector #:length 3 ((i (in-range 3))) (real->extfl (+ i 1.0)))))
    (test flv 'for/extflvector flv1)
    (test flv 'for/extflvector-fast flv2))

  (test (extflvector 1.0t0 2.0t0 3.0t0 0.0t0 0.0t0)
        'for/extflvector-fill
        (for/extflvector #:length 5 ([i 3]) (real->extfl (+ i 1.0))))
  (test (extflvector 1.0t0 2.0t0 3.0t0 -10.0t0 -10.0t0)
        'for/extflvector-fill
        (for/extflvector #:length 5 #:fill -10.0t0 ([i 3]) (real->extfl (+ i 1.0))))
  (test (extflvector 1.0t0 2.0t0 3.0t0 0.0t0 0.0t0)
        'for/extflvector-fill
        (for/extflvector #:length 5 ([i 5]) #:break (= i 3) (real->extfl (+ i 1.0))))
  (test (extflvector 1.0t0 2.0t0 3.0t0 4.0t0 0.0t0)
        'for/extflvector-fill
        (for/extflvector #:length 5 ([i 5]) #:final (= i 3) (real->extfl (+ i 1.0))))

  ;; for*/extflvector test
  (let ((flv (extflvector 0.0t0 0.0t0 0.0t0 0.0t0 1.0t0 2.0t0 0.0t0 2.0t0 4.0t0))
        (flv1 (for*/extflvector ((i (in-range 3)) (j (in-range 3))) (->extfl (* 1 i j))))
        (flv2 (for*/extflvector #:length 9 ((i (in-range 3)) (j (in-range 3))) (real->extfl (* 1.0 i j)))))
    (test flv 'for*/extflvector flv1)
    (test flv 'for*/extflvector-fast flv2))

  ;; Stop when a length is specified, even if the sequence continues:
  (test (extflvector 0.0t0 1.0t0 2.0t0 3.0t0 4.0t0 5.0t0 6.0t0 7.0t0 8.0t0 9.0t0)
        'nat
        (for/extflvector #:length 10 ([i (in-naturals)]) (real->extfl i)))
  (test (extflvector 0.0t0 1.0t0 2.0t0 3.0t0 4.0t0 5.0t0 6.0t0 7.0t0 8.0t0 9.0t0)
        'nats
        (for*/extflvector #:length 10 ([i (in-naturals)] [j (in-naturals)]) (real->extfl j)))
  (test (extflvector 0.0t0 0.0t0 0.0t0 0.0t0 0.0t0 1.0t0 1.0t0 1.0t0 1.0t0 1.0t0)
        'nat+5
        (for*/extflvector #:length 10 ([i (in-naturals)] [j (in-range 5)]) (real->extfl i)))

  ;; Test for both length too long and length too short
  (let ((v (make-extflvector 3)))
    (extflvector-set! v 0 0.0t0)
    (extflvector-set! v 1 1.0t0)
    (let ((w (for/extflvector #:length 3 ((i (in-range 2))) (real->extfl i))))
      (test v 'for/extflvector-short-iter w)))

  (let ((v (make-extflvector 10)))
    (for* ((i (in-range 3))
           (j (in-range 3)))
      (extflvector-set! v (+ j (* i 3)) (real->extfl (+ 1.0 i j))))
    (let ((w (for*/extflvector #:length 10 ((i (in-range 3)) (j (in-range 3))) (real->extfl (+ 1.0 i j)))))
      (test v 'for*/extflvector-short-iter w)))

  (test 2 'for/extflvector-long-iter
        (extflvector-length (for/extflvector #:length 2 ((i (in-range 10))) (real->extfl i))))
  (test 5 'for*/extflvector-long-iter 
        (extflvector-length (for*/extflvector #:length 5 ((i (in-range 3)) (j (in-range 3))) (real->extfl (+ i j)))))

  ;; Test for many body expressions
  (let* ((flv (extflvector 1.0t0 2.0t0 3.0t0))
         (flv2 (for/extflvector ((i (in-range 3))) 
                                (extflvector-set! flv i (extfl+ (extflvector-ref flv i) 1.0t0))
                                (extflvector-ref flv i)))
         (flv3 (for/extflvector #:length 3 ((i (in-range 3)))
                                (extflvector-set! flv i (extfl+ (extflvector-ref flv i) 1.0t0))
                                (extflvector-ref flv i))))
    (test (extflvector 2.0t0 3.0t0 4.0t0) 'for/extflvector-many-body flv2)
    (test (extflvector 3.0t0 4.0t0 5.0t0) 'for/extflvector-length-many-body flv3))

  ;; extflvector-copy test
  (let ((v (extflvector 0.0t0 1.0t0 2.0t0 3.0t0)))
    (let ((vc (extflvector-copy v)))
      (test (extflvector-length v) 'extflvector-copy (extflvector-length vc))
      (for ((vx (in-extflvector v))
            (vcx (in-extflvector vc)))
        (test vx 'extflvector-copy vcx))
      (extflvector-set! vc 2 -10.0t0)
      (test 2.0t0 'extflvector-copy (extflvector-ref v 2))
      (test -10.0t0 'extflvector-copy (extflvector-ref vc 2))
      (test '(2.0t0 3.0t0) 'extflvector-copy (for/list ([i (in-extflvector (extflvector-copy v 2))]) i))
      (test '(2.0t0) 'extflvector-copy (for/list ([i (in-extflvector (extflvector-copy v 2 3))]) i))))

  ;; Check empty clauses
  (let ()
    (define vector-iters 0)
    (test (extflvector 3.4t0 0.0t0 0.0t0 0.0t0)
          'no-clauses
          (for/extflvector #:length 4 ()
                           (set! vector-iters (+ 1 vector-iters))
                           3.4t0))
    (test 1 values vector-iters)
    (test (extflvector 3.4t0 0.0t0 0.0t0 0.0t0)
          'no-clauses
          (for*/extflvector #:length 4 ()
                            (set! vector-iters (+ 1 vector-iters))
                            3.4t0))
    (test 2 values vector-iters))

  ;; Check #:when and #:unless:
  (test (extflvector 0.0t0 1.0t0 2.0t0 1.0t0 2.0t0)
        'when-#t
        (for/extflvector #:length 5
                         ([x (in-range 3)]
                          #:when #t
                          [y (in-range 3)])
                         (real->extfl (+ x y))))
  (test (extflvector 0.0t0 1.0t0 2.0t0 2.0t0 3.0t0)
        'when-...
        (for/extflvector #:length 5
                         ([x (in-range 3)]
                          #:when (even? x)
                          [y (in-range 3)])
                         (real->extfl (+ x y))))
  (test (extflvector 0.0t0 1.0t0 2.0t0 1.0t0 2.0t0)
        'unless-#f
        (for/extflvector #:length 5
                         ([x (in-range 3)]
                          #:unless #f
                          [y (in-range 3)])
                         (real->extfl (+ x y))))

  (test (extflvector 1.0t0 2.0t0 3.0t0 0.0t0 0.0t0)
        'unless-...
        (for/extflvector #:length 5
                         ([x (in-range 3)]
                          #:unless (even? x)
                          [y (in-range 3)])
                         (real->extfl (+ x y))))



  ;; in-extflvector tests, copied from for.rktl

;;;;
;;;; TODO replace for/sum, because extflonum do not support + (addition) and other operations
;;;;
  (define-syntax (test-multi-sequence stx)
    (syntax-case stx ()
      [(_ [(v ...) ...] gen)
       (with-syntax ([(id ...) (generate-temporaries #'((v ...) ...))]
                     [(id2 ...) (generate-temporaries #'((v ...) ...))]
                     [((v2 ...) ...)
                      (apply map list (map syntax->list (syntax->list #'((v ...) ...))))])
         #'(begin
             (test `((v2 ...) ...) 'gen (for/list ([(id ...) gen])
                                          (list id ...)))
             (test-values `((v ...) ...) (lambda ()
                                           (for/lists (id2 ...) ([(id ...) gen])
                                                      (values id ...))))
             (test #t 'gen (for/and ([(id ...) gen])
                             (and (member (list id ...) `((v2 ...) ...)) #t)))
             (test (list (for/last ([(id ...) gen])
                                   (list id ...)))
                   'gen (for/and ([(id ...) gen])
                          (member (list id ...) `((v2 ...) ...))))
             ;; (test (for/first ([(id ...) gen])
             ;;         (list id ...))
             ;;       'gen (for/or ([(id ...) gen])
             ;;               (car (member (list id ...) `((v2 ...) ...)))))
             (void)))]))

  (define-syntax test-sequence
    (syntax-rules ()
      [(_ [seq] gen) ; we assume that seq has at least 2 elements, and all are unique
       (begin
         ;; Some tests specific to single-values:
         (test `seq 'gen (for/list ([i gen]) i))
         (test `seq 'gen (for/list ([i gen][b gen]) i))
         (test `seq 'gen (for/list ([i gen][b gen]) b))
         (test `seq 'gen (for*/list ([i gen][b '(#t)]) i))
         (test (map (lambda (x) #t) `seq) 'gen (for*/list ([i gen][b '(#t)]) b))
         (test (append `seq `seq) 'gen (for*/list ([b '(#f #t)][i gen]) i))
         (test (append `seq `seq) 'gen (for/list ([b '(#f #t)] #:when #t [i gen]) i))
         (test (append `seq `seq) 'gen (for/list ([b '(#t #t #f)] #:when b [i gen]) i))
         (test (append `seq `seq) 'gen (for/list ([b '(#f #t)] #:unless #f [i gen]) i))
         (test (append `seq `seq) 'gen (for/list ([b '(#f #f #t)] #:unless b [i gen]) i))
         (test `seq 'gen (let ([g gen]) (for/list ([i g]) i)))
         (test `seq 'gen (let ([r null])
                           (for ([i gen]) (set! r (cons i r)))
                           (reverse r)))
         (test `seq 'gen (reverse (for/fold ([a null]) ([i gen]) 
                                    (cons i a))))
         (test `seq 'gen (let-values ([(more? next) (sequence-generate gen)])
                           (let loop ()
                             (if (more?)
                                 (cons (next) (loop))
                                 null))))
         (test-values `(seq seq) (lambda ()
                                   (for/lists (r1 r2) ([id gen])
                                              (values id id))))
         (test (list (for/last ([i gen]) i)) 'gen (for/and ([i gen]) (member i `seq)))
         (test `seq 'gen (for/or ([i gen]) (member i `seq)))
         (test (for/first ([i gen]) i) 'gen (for/or ([i gen]) (and (member i `seq) i)))
         ;; (test (for/sum ([i gen]) (if (number? i) i 0)) 'gen 
         ;;       (for/fold ([n 0]) ([i gen]) (if (number? i) (+ i n) n)))
         ;; (test (for/product ([i gen]) (if (number? i) i 1)) 'gen 
         ;;       (for/fold ([n 1]) ([i gen]) (if (number? i) (* i n) n)))
         (test #t 'gen (for/and ([(i k) (in-parallel gen `seq)])
                         (equal? i k)))
         (test #f 'gen (for/and ([i gen])
                         (member i (cdr (reverse `seq)))))
         (test #f 'gen (for/or ([i gen]) (equal? i 'something-else)))
         (let ([count 0])
           (test #t 'or (for/or ([i gen]) (set! count (add1 count)) #t))
           (test 1 'count count)
           (test #f 'or (for/or ([i gen]) (set! count (add1 count)) #f))
           (test (+ 1 (length `seq)) 'count count)
           (set! count 0)
           (let ([second (for/last ([(i pos) (in-parallel gen (in-naturals))] #:when (< pos 2))
                                   (set! count (add1 count))
                                   i)])
             (test second list-ref `seq 1)
             (test 2 values count)
             (for ([i gen] #:when (equal? i second)) (set! count (add1 count)))
             (for* ([i gen] #:when (equal? i second)) (set! count (add1 count)))
             (test 4 values count)
             (for ([i (stop-before gen (lambda (x) (equal? x second)))]) (set! count (add1 count)))
             (test 5 values count)
             (let ([g (stop-before gen (lambda (x) (equal? x second)))])
               (for ([i g]) (set! count (add1 count))))
             (test 6 values count)
             (for ([i (stop-after gen (lambda (x) (equal? x second)))]) (set! count (add1 count)))
             (test 8 values count)
             (let ([g (stop-after gen (lambda (x) (equal? x second)))])
               (for ([i g]) (set! count (add1 count))))
             (test 10 values count))
           (set! count 0)
           (test #t 'and (for/and ([(e idx) (in-indexed gen)]) (set! count (add1 count)) (equal? idx (sub1 count))))
           (test #t 'and (let ([g (in-indexed gen)])
                           (set! count 0)
                           (for/and ([(e idx) g]) (set! count (add1 count)) (equal? idx (sub1 count)))))
           (void))
         ;; Run multi-value tests:
         (test-multi-sequence [seq] gen))]
      [(_ seqs gen)
       (test-multi-sequence seqs gen)]))

  (test-sequence [(1.0t0 2.0t0 3.0t0)] (in-extflvector (extflvector 1.0t0 2.0t0 3.0t0)))
  (test-sequence [(2.0t0 3.0t0 4.0t0)] (in-extflvector (extflvector 1.0t0 2.0t0 3.0t0 4.0t0) 1))
  (test-sequence [(2.0t0 3.0t0 4.0t0)] (in-extflvector (extflvector 1.0t0 2.0t0 3.0t0 4.0t0 5.0t0) 1 4))
  (test-sequence [(2.0t0 4.0t0 6.0t0)] (in-extflvector (extflvector 1.0t0 2.0t0 3.0t0 4.0t0 5.0t0 6.0t0 7.0t0 8.0t0) 1 7 2))
  (test-sequence [(8.0t0 6.0t0 4.0t0)] (in-extflvector (extflvector 1.0t0 2.0t0 3.0t0 4.0t0 5.0t0 6.0t0 7.0t0 8.0t0) 7 1 -2))
  (test-sequence [(2.0t0 4.0t0 6.0t0)] (in-extflvector (extflvector 1.0t0 2.0t0 3.0t0 4.0t0 5.0t0 6.0t0 7.0t0 8.0t0) 1 6 2))
  (test-sequence [(8.0t0 6.0t0 4.0t0)] (in-extflvector (extflvector 1.0t0 2.0t0 3.0t0 4.0t0 5.0t0 6.0t0 7.0t0 8.0t0) 7 2 -2))



  ;; ----------------------------------------
  ;; Check corners of `extflexpt':
  ;;  Tests by Neil T.:

  (let ()
    (define-syntax-rule (check-equal? (extflexpt v1 v2) b)
      (test b extflexpt v1 v2))
    
    ;; 2^53 and every larger flonum is even:
    (define +big-even.0t0 (extflexpt 2.0t0 53.0t0))
    ;; The largest odd flonum:
    (define +max-odd.0t0 (extfl- +big-even.0t0 1.0t0))

    (define -big-even.0t0 (extfl- 0.0t0 +big-even.0t0))
    (define -max-odd.0t0 (extfl- 0.0t0 +max-odd.0t0))

    (check-equal? (extflexpt +0.0t0 +0.0t0) +1.0t0)
    (check-equal? (extflexpt +0.0t0 +1.0t0) +0.0t0)
    (check-equal? (extflexpt +0.0t0 +3.0t0) +0.0t0)
    (check-equal? (extflexpt +0.0t0 +max-odd.0t0) +0.0t0)
    (check-equal? (extflexpt +0.0t0 +0.5t0) +0.0t0)
    (check-equal? (extflexpt +0.0t0 +1.5t0) +0.0t0)
    (check-equal? (extflexpt +0.0t0 +2.0t0) +0.0t0)
    (check-equal? (extflexpt +0.0t0 +2.5t0) +0.0t0)
    (check-equal? (extflexpt +0.0t0 +big-even.0t0) +0.0t0)

    (check-equal? (extflexpt -0.0t0 +0.0t0) +1.0t0)
    (check-equal? (extflexpt -0.0t0 +1.0t0) -0.0t0)
    (check-equal? (extflexpt -0.0t0 +3.0t0) -0.0t0)
    (check-equal? (extflexpt -0.0t0 +max-odd.0t0) -0.0t0)
    (check-equal? (extflexpt -0.0t0 +0.5t0) +0.0t0)
    (check-equal? (extflexpt -0.0t0 +1.5t0) +0.0t0)
    (check-equal? (extflexpt -0.0t0 +2.0t0) +0.0t0)
    (check-equal? (extflexpt -0.0t0 +2.5t0) +0.0t0)
    (check-equal? (extflexpt -0.0t0 +big-even.0t0) +0.0t0)

    (check-equal? (extflexpt +1.0t0 +0.0t0) +1.0t0)
    (check-equal? (extflexpt +1.0t0 +0.5t0) +1.0t0)
    (check-equal? (extflexpt +1.0t0 +inf.t) +1.0t0)

    (check-equal? (extflexpt -1.0t0 +0.0t0) +1.0t0)
    (check-equal? (extflexpt -1.0t0 +0.5t0) +nan.t)
    (check-equal? (extflexpt -1.0t0 +inf.t) +1.0t0)

    (check-equal? (extflexpt +0.5t0 +inf.t) +0.0t0)
    (check-equal? (extflexpt +1.5t0 +inf.t) +inf.t)

    (check-equal? (extflexpt +inf.t +0.0t0) +1.0t0)
    (check-equal? (extflexpt +inf.t +1.0t0) +inf.t)
    (check-equal? (extflexpt +inf.t +2.0t0) +inf.t)
    (check-equal? (extflexpt +inf.t +inf.t) +inf.t)

    (check-equal? (extflexpt -inf.t +0.0t0) +1.0t0)
    (check-equal? (extflexpt -inf.t +1.0t0) -inf.t)
    (check-equal? (extflexpt -inf.t +3.0t0) -inf.t)
    (check-equal? (extflexpt -inf.t +max-odd.0t0) -inf.t)
    (check-equal? (extflexpt -inf.t +0.5t0) +inf.t)
    (check-equal? (extflexpt -inf.t +1.5t0) +inf.t)
    (check-equal? (extflexpt -inf.t +2.0t0) +inf.t)
    (check-equal? (extflexpt -inf.t +2.5t0) +inf.t)
    (check-equal? (extflexpt -inf.t +big-even.0t0) +inf.t)
    (check-equal? (extflexpt -inf.t +inf.t) +inf.t)

    ;; Same tests as above, but with negated y
    ;; This identity should hold for these tests: (extflexpt x y) = (/ 1.0t0 (extflexpt x (- y)))

    (check-equal? (extflexpt +0.0t0 -0.0t0) +1.0t0)
    (check-equal? (extflexpt +0.0t0 -1.0t0) +inf.t)
    (check-equal? (extflexpt +0.0t0 -3.0t0) +inf.t)
    (check-equal? (extflexpt +0.0t0 -max-odd.0t0) +inf.t)
    (check-equal? (extflexpt +0.0t0 -0.5t0) +inf.t)
    (check-equal? (extflexpt +0.0t0 -1.5t0) +inf.t)
    (check-equal? (extflexpt +0.0t0 -2.0t0) +inf.t)
    (check-equal? (extflexpt +0.0t0 -2.5t0) +inf.t)
    (check-equal? (extflexpt +0.0t0 -big-even.0t0) +inf.t)

    (check-equal? (extflexpt -0.0t0 -0.0t0) +1.0t0)
    (check-equal? (extflexpt -0.0t0 -1.0t0) -inf.t)
    (check-equal? (extflexpt -0.0t0 -3.0t0) -inf.t)
    (check-equal? (extflexpt -0.0t0 -max-odd.0t0) -inf.t)
    (check-equal? (extflexpt -0.0t0 -0.5t0) +inf.t)
    (check-equal? (extflexpt -0.0t0 -1.5t0) +inf.t)
    (check-equal? (extflexpt -0.0t0 -2.0t0) +inf.t)
    (check-equal? (extflexpt -0.0t0 -2.5t0) +inf.t)
    (check-equal? (extflexpt -0.0t0 -big-even.0t0) +inf.t)

    (check-equal? (extflexpt +1.0t0 -0.0t0) +1.0t0)
    (check-equal? (extflexpt +1.0t0 -0.5t0) +1.0t0)
    (check-equal? (extflexpt +1.0t0 -inf.t) +1.0t0)

    (check-equal? (extflexpt -1.0t0 -0.0t0) +1.0t0)
    (check-equal? (extflexpt -1.0t0 -0.5t0) +nan.t)
    (check-equal? (extflexpt -1.0t0 -inf.t) +1.0t0)

    (check-equal? (extflexpt +0.5t0 -inf.t) +inf.t)
    (check-equal? (extflexpt +1.5t0 -inf.t) +0.0t0)

    (check-equal? (extflexpt +inf.t -0.0t0) +1.0t0)
    (check-equal? (extflexpt +inf.t -1.0t0) +0.0t0)
    (check-equal? (extflexpt +inf.t -2.0t0) +0.0t0)
    (check-equal? (extflexpt +inf.t -inf.t) +0.0t0)

    (check-equal? (extflexpt -inf.t -0.0t0) +1.0t0)
    (check-equal? (extflexpt -inf.t -1.0t0) -0.0t0)
    (check-equal? (extflexpt -inf.t -3.0t0) -0.0t0)
    (check-equal? (extflexpt -inf.t -max-odd.0t0) -0.0t0)
    (check-equal? (extflexpt -inf.t -0.5t0) +0.0t0)
    (check-equal? (extflexpt -inf.t -1.5t0) +0.0t0)
    (check-equal? (extflexpt -inf.t -2.0t0) +0.0t0)
    (check-equal? (extflexpt -inf.t -2.5t0) +0.0t0)
    (check-equal? (extflexpt -inf.t -big-even.0t0) +0.0t0)
    (check-equal? (extflexpt -inf.t -inf.t) +0.0t0)

    ;; NaN input

    (check-equal? (extflexpt +nan.t +0.0t0) +1.0t0)
    (check-equal? (extflexpt +nan.t -0.0t0) +1.0t0)
    (check-equal? (extflexpt +1.0t0 +nan.t) +1.0t0)
    (check-equal? (extflexpt -1.0t0 +nan.t) +nan.t))

  ;; ----------------------------------------

  )

(report-errs)
