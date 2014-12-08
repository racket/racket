
(load-relative "loadtest.rktl")

(Section 'for)

(require compatibility/mlist
         "for-util.rkt")

(test-sequence [(0 1 2)] 3)
(test-sequence [(0 1 2)] (in-range 3))
(test-sequence [(3 4 5)] (in-range 3 6))
(test-sequence [(7 6 5)] (in-range 7 4 -1))
(test-sequence [(3.0 4.0 5.0)] (in-range 3.0 6.0))
(test-sequence [(3.0 3.5 4.0 4.5 5.0 5.5)] (in-range 3.0 6.0 0.5))
(test-sequence [(3.0 3.1 3.2)] (in-range 3.0 3.3 0.1))

(test-sequence [(a b c)] '(a b c))
(test-sequence [(a b c)] (in-list '(a b c)))
(test-sequence [(a b c)] (mlist 'a 'b 'c))
(test-sequence [(a b c)] (in-mlist (mlist 'a 'b 'c)))
(test-sequence [(a b c)] #(a b c))
(test-sequence [(a b c)] (in-vector #(a b c)))
(test-sequence [(a b c)] (in-vector (chaperone-vector #(a b c) (lambda (vec i val) val) (lambda (vec i val) val))))
(test-sequence [(b c d)] (in-vector #(a b c d) 1))
(test-sequence [(b c d)] (in-vector #(a b c d e) 1 4))
(test-sequence [(b d f)] (in-vector #(a b c d e f g h) 1 7 2))
(test-sequence [(h f d)] (in-vector #(a b c d e f g h) 7 1 -2))
(test-sequence [(b d f)] (in-vector #(a b c d e f g h) 1 6 2))
(test-sequence [(h f d)] (in-vector #(a b c d e f g h) 7 2 -2))
(test-sequence [(c b a)] (in-vector #(a b c) 2 -1 -1))
;; Test indices out of bounds
(err/rt-test (for/list ([x (in-vector #(a b c d) 0 6 2)]) x) exn:fail:contract?)
(err/rt-test (for/list ([x (in-vector #(a b c d) 6 0 -2)]) x) exn:fail:contract?)
(test-sequence [(#\a #\b #\c)] "abc")
(test-sequence [(#\a #\u3bb #\c)] "a\u03BBc")
(test-sequence [(#\a #\b #\c)] (in-string "abc"))
(test-sequence [(#\a #\u3bb #\c)] (in-string "a\u03BBc"))
(test-sequence [(#\a #\b #\c)] (in-string "zzabc" 2))
(test-sequence [(#\a #\b #\c)] (in-string "zzabc" 2 #f))
(test-sequence [(#\a #\b #\c)] (in-string "zzabcqq" 2 5))
(test-sequence [(#\a #\b #\c)] (in-string "zzaxbyc" 2 #f 2))
(test-sequence [(#\a #\b #\c)] (in-string "zzaxbycy" 2 #f 2))
(test-sequence [(65 66 67)] #"ABC")
(test-sequence [(65 66 67)] (in-bytes #"ABC"))
(test-sequence [(65 66 67)] (in-bytes #"ZZABC" 2))
(test-sequence [(65 66 67)] (in-bytes #"ZZABC" 2 #f))
(test-sequence [(65 66 67)] (in-bytes #"ZZABCQQ" 2 5))
(test-sequence [(65 66 67)] (in-bytes #"ZZAXBYC" 2 #f 2))
(test-sequence [(65 66 67)] (in-bytes #"ZZAXBYCY" 2 #f 2))
(test-sequence [(#\a #\b #\c)] (in-input-port-chars (open-input-string "abc")))
(test-sequence [(65 66 67)] (open-input-bytes #"ABC"))
(test-sequence [(65 66 67)] (in-input-port-bytes (open-input-bytes #"ABC")))

;; Test optimized:
(test '(2) 'in-list-of-list (for/list ([v (in-list (list 1))]) (add1 v)))
(test '(0) 'in-mlist-of-mlist (for/list ([v (in-mlist (mlist 1))]) (sub1 v)))

(test-sequence [(1 2 3)] (in-port read (open-input-string "1 2 3")))
(test-sequence [((123) 4)] (in-port read (open-input-string "(123) 4")))
(test-sequence [(65 66 67)] (in-port read-byte (open-input-string "ABC")))

(test-sequence [("abc" "def")] (in-lines (open-input-string "abc\ndef")))
(test-sequence [(#"abc" #"def")] (in-bytes-lines (open-input-string "abc\ndef")))

(test-sequence [(0 1 2 3 4 5)] (in-sequences (in-range 6)))
(test-sequence [(0 1 2 3 4 5)] (in-sequences (in-range 4) '(4 5)))
(test-sequence [(0 1 2 3 4 5)] (in-sequences (in-range 6) '()))
(test-sequence [(0 1 2 3 4 5)] (in-sequences '() (in-range 4) '() '(4 5)))
(test-sequence [(0 1 2 3 4 5)] (in-sequences (in-range 0 2) (in-range 2 4) (in-range 4 6)))
(test-sequence [(0 1 2 3 4 5)] (in-sequences (in-range 0 2)
                                              (in-sequences (in-range 2 4) (in-range 4 6))))
(test-sequence [(0 1 2 3 #\a #\b #\c) (10 11 12 13 #\A #\B #\C)]
                (in-sequences (in-parallel (in-range 0 4) (in-range 10 14))
                              (in-parallel "abc" "ABC")))
;; Check empty sequences:
(test '() 'empty-seq (for/list ([v (in-sequences)]) v))
(test '() 'empty-seq (for/list ([v (in-sequences '())]) v))
(test '() 'empty-seq (for/list ([v (in-sequences '() '())]) v))

;; use in-parallel to get a finite number of items
(test-sequence [(0 1 2 3 0 1 2 3) (0 1 2 3 4 5 6 7)]
                (in-parallel (in-cycle (in-range 0 4)) (in-range 0 8)))
(test-sequence [(0 1 2 3 4 5 6 7) (0 1 2 0 1 2 0 1)]
                (in-parallel (in-range 0 8) (in-cycle (in-range 0 3))))
(test-sequence [(0 1 2 3 2 1 0 1) (0 1 2 3 4 5 6 7)]
                (in-parallel (in-cycle (in-range 0 4) (in-range 2 0 -1)) (in-range 0 8)))
;; `in-cycle' accepts 0 arguments, but it never produces a value if asked:
(test #t sequence? (in-cycle))
(test #t sequence? (in-cycle '()))

(test-sequence [(0 1 2) (a b c)] (in-parallel (in-range 3) (in-list '(a b c))))
(test-sequence [(0 1 2) (a b c)] (in-parallel (in-range 10) (in-list '(a b c))))
(test-sequence [(0 1 2) (a b c)] (in-parallel (in-range 3) (in-list '(a b c d))))
(test-sequence [(0 1 2) (a b c)] (in-parallel (in-range 3) '(a b c)))

(test-sequence [(a b c)] (stop-after (in-list '(a b c d e)) (lambda (x) (equal? x 'c))))
(test-sequence [(a b c)] (stop-before (in-list '(a b c d e)) (lambda (x) (equal? x 'd))))
(test-sequence [(3 4 5)] (stop-before (in-naturals 3) (lambda (x) (= x 6))))

(test-sequence [(a b c) (0 1 2)] (in-indexed '(a b c)))

(let ()
  (define (counter) (define n 0) (lambda ([d 1]) (set! n (+ d n)) n))
  (test-sequence [(1 2 3 4)] (for/list ([x (in-producer (counter))] [y (in-range 4)]) x))
  (test-sequence [(1 2 3 4)] (for/list ([x (in-producer (counter))] #:break (= x 5)) x))
  (test-sequence [(1 2 3 4)] (for/list ([x (in-producer (counter) 5)]) x))
  (test-sequence [(1/2 1 3/2 2 5/2 3 7/2 4 9/2)]
    (for/list ([x (in-producer (counter) 5 1/2)]) x))
  ;; test in-producer outside of for loops
  (test 6 sequence-ref (in-producer (counter)) 5))

(test-sequence [(1 2 3 4 5)]
  (parameterize ([current-input-port (open-input-string "1 2 3\n4 5")])
    (for/list ([i (in-producer read eof)]) i)))
(test-sequence [(1 2 3 4 5)]
  (for/list ([i (in-producer read eof (open-input-string "1 2 3\n4 5"))]) i))
(test-sequence [("1 2 3" "4 5")]
  (for/list ([i (in-producer read-line eof-object? (open-input-string "1 2 3\n4 5"))]) i))
(test-sequence [((1 2) (3 4) (5 ,eof))]
  (for/list ([(i j)
              (in-producer (lambda (p) (values (read p) (read p)))
                           (lambda (x y) (and (eof-object? x) (eof-object? y)))
                           (open-input-string "1 2 3\n4 5"))])
    (list i j)))

(let ([five-seq
       (lambda (pos pre post)
         (test-sequence [(1 2 3 4 5)]
                        (make-do-sequence (lambda ()
                                            (values add1
                                                    add1
                                                    0
                                                    pos 
                                                    pre 
                                                    post)))))])
  (five-seq (lambda (pos) (pos . < . 5))
            #f
            #f)
  (five-seq #f
            (lambda (val) (val . < . 6))
            #f)
  (five-seq #f
            #f
            (lambda (pos val) (val . < . 5))))

(let ([fives-seq
       (lambda (pos pre post)
         (test-sequence [(1 2 3 4 5) ("0" "1" "2" "3" "4")]
                        (make-do-sequence (lambda ()
                                            (values (lambda (n) (values (add1 n)
                                                                        (number->string n)))
                                                    add1
                                                    0
                                                    pos 
                                                    pre 
                                                    post)))))])
  (fives-seq (lambda (pos) (pos . < . 5))
             #f
             #f)
  (fives-seq #f
             (lambda (val1 val2) (val1 . < . 6))
             #f)
  (fives-seq #f
             (lambda (val1 val2) (not (string=? val2 "5")))
             #f)
  (fives-seq #f
             #f
             (lambda (pos val1 val2) (val1 . < . 5)))
  (fives-seq #f
             #f
             (lambda (pos val1 val2) (not (string=? val2 "4")))))


(test '(1 2 3)
      'three
      (for/list ([i 10])
        #:break (= i 3)
        (add1 i)))
(test '(1 2 3 4)
      'three
      (for/list ([i 10])
        #:final (= i 3)
        (add1 i)))

;; Make sure that breaking a sequence stops before consuming another element:
(test '(("1" "2" "3" "4" "5" "6" "7" "8" "9") . 10)
      'producer
      (let ([c 0])
        (cons
         (for/list ([i (in-producer (lambda () (set! c (add1 c)) c))])
           #:break (= i 10)
           (number->string i))
         c)))
(test '(("1" "2" "3" "4" "5" "6" "7" "8" "9") . 10)
      'producer
      (let ([c 0])
        (cons
         (for*/list ([j '(0)]
                     [i (in-producer (lambda () (set! c (add1 c)) c))])
           #:break (= i 10)
           (number->string i))
         c)))

;; Basic sanity checks.
(test '#(1 2 3 4) 'for/vector (for/vector ((i (in-range 4))) (+ i 1)))
(test '#(1 2 3 4) 'for/vector-fast (for/vector #:length 4 ((i (in-range 4))) (+ i 1)))
(test '#(1 2 3 4 0 0) 'for/vector-fast (for/vector #:length 6 ((i (in-range 4))) (+ i 1)))
(test '#(1 2 3 4 #f #f) 'for/vector-fast (for/vector #:length 6 #:fill #f ((i (in-range 4))) (+ i 1)))

(test '#(0 0 0 0 1 2 0 2 4) 'for*/vector (for*/vector ((i (in-range 3))
                                                       (j (in-range 3)))
                                           (+ i j)
                                           (* i j)))
(test '#(0 0 0 0 1 2 0 2 4) 'for*/vector-fast (for*/vector #:length 9 ((i (in-range 3))
                                                                       (j (in-range 3)))
                                                (+ i j)
                                                (* i j)))

;; Test for both length too long and length too short
(let ((v (make-vector 3)))
  (vector-set! v 0 0)
  (vector-set! v 1 1)
  (let ((w (for/vector #:length 3 ((i (in-range 2))) i)))
    (test v 'for/vector-short-iter w)))

(let ((v (make-vector 10)))
  (for* ((i (in-range 3))
         (j (in-range 3)))
    (vector-set! v (+ j (* i 3)) (+ i j)))
  (let ((w (for*/vector #:length 10 ((i (in-range 3)) (j (in-range 3))) (+ i j))))
    (test v 'for*/vector-short-iter w)))

(test 2 'for/vector-long-iter
      (vector-length (for/vector #:length 2 ((i (in-range 10))) i)))
(test 5 'for*/vector-long-iter 
      (vector-length (for*/vector #:length 5 ((i (in-range 3)) (j (in-range 3))) (+ i j))))

;; Test for many body expressions
(let* ((v (vector 1.0 2.0 3.0))
       (v2 (for/vector ((i (in-range 3))) 
             (vector-set! v i (+ (vector-ref v i) 1.0))
             (vector-ref v i)))
       (v3 (for/vector #:length 3 ((i (in-range 3)))
             (vector-set! v i (+ (vector-ref v i) 1.0))
             (vector-ref v i))))
  (test (vector 2.0 3.0 4.0) 'for/vector-many-body v2)
  (test (vector 3.0 4.0 5.0) 'for/vector-length-many-body v3))

;; Stop when a length is specified, even if the sequence continues:
(test '#(0 1 2 3 4 5 6 7 8 9)
      'nat
      (for/vector #:length 10 ([i (in-naturals)]) i))
(test '#((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0) (7 . 0) (8 . 0) (9 . 0))
      'nats
      (for*/vector #:length 10 ([i (in-naturals)] [j (in-naturals)]) (cons j i)))
(test '#((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (0 . 1) (1 . 1) (2 . 1) (3 . 1) (4 . 1))
      'nat+5
      (for*/vector #:length 10 ([i (in-naturals)] [j (in-range 5)]) (cons j i)))
(test '#(1 3 5 7 9 11 13 15 17 19)
      'parallel
      (for*/vector #:length 10 ([(i j) (in-parallel (in-naturals)
                                                    (in-naturals 1))])
                   (+ i j)))

;; Make sure the sequence stops at the length before consuming another element:
(test '(#("1" "2" "3" "4" "5" "6" "7" "8" "9" "10") . 10)
      'producer
      (let ([c 0])
        (cons
         (for/vector #:length 10 ([i (in-producer (lambda () (set! c (add1 c)) c))]) 
                     (number->string i))
         c)))
(test '(#("1" "2" "3" "4" "5" "6" "7" "8" "9" "10") . 10)
      'producer
      (let ([c 0])
        (cons
         (for*/vector #:length 10 ([j '(0)]
                                   [i (in-producer (lambda () (set! c (add1 c)) c))])
                      (number->string i))
         c)))

;; Check empty clauses
(let ()
  (define vector-iters 0)
  (test (vector 3.4 0 0 0)
        'no-clauses
        (for/vector #:length 4 ()
                    (set! vector-iters (+ 1 vector-iters))
                    3.4))
  (test 1 values vector-iters)
  (test (vector 3.4 0 0 0)
        'no-clauses
        (for*/vector #:length 4 ()
                     (set! vector-iters (+ 1 vector-iters))
                     3.4))
  (test 2 values vector-iters))

;; Check #:when and #:unless:
(test (vector 0 1 2 1 2)
      'when-#t
      (for/vector #:length 5
                  ([x (in-range 3)]
                   #:when #t
                   [y (in-range 3)])
        (+ x y)))
(test (vector 0 1 2 2 3)
      'when-...
      (for/vector #:length 5
                  ([x (in-range 3)]
                   #:when (even? x)
                   [y (in-range 3)])
        (+ x y)))
(test (vector 0 1 2 1 2)
      'unless-#f
      (for/vector #:length 5
                  ([x (in-range 3)]
                   #:unless #f
                   [y (in-range 3)])
        (+ x y)))
(test (vector 1 2 3 -1 -1)
      'unless-...
      (for/vector #:length 5
                  #:fill -1
                  ([x (in-range 3)]
                   #:unless (even? x)
                   [y (in-range 3)])
        (+ x y)))

(test #hash((a . 1) (b . 2) (c . 3)) 'mk-hash
      (for/hash ([v (in-naturals)]
                 [k '(a b c)])
                (values k (add1 v))))
(test #hasheq((a . 1) (b . 2) (c . 3)) 'mk-hasheq
      (for/hasheq ([v (in-naturals)]
                   [k '(a b c)])
                  (values k (add1 v))))
(test #hash((a . 1) (b . 2) (c . 3)) 'cp-hash
      (for/hash ([(k v) #hash((a . 1) (b . 2) (c . 3))])
                (values k v)))
(test #hash((a . 1) (b . 2) (c . 3)) 'cp-hash
      (for/hash ([(k v) (in-hash #hash((a . 1) (b . 2) (c . 3)))])
                (values k v)))
(test #hash((a . a) (b . b) (c . c)) 'cp-hash
      (for/hash ([k (in-hash-keys #hash((a . 1) (b . 2) (c . 3)))])
                (values k k)))
(test #hash((1 . 1) (2 . 2) (3 . 3)) 'cp-hash
      (for/hash ([v (in-hash-values #hash((a . 1) (b . 2) (c . 3)))])
                (values v v)))

(test 1 'parallel-or-first
      (for/or (((a b) (in-parallel '(1 #f) '(#t #f)))) 
              a))
(test 1 'parallel-or-last
      (for/or (((a b) (in-parallel '(#f 1) '(#t #f)))) 
              a))
(test #f 'parallel-and-first
      (for/and (((a b) (in-parallel '(1 #f) '(#t #f)))) 
              a))
(test #f 'parallel-and-last
      (for/and (((a b) (in-parallel '(#f 1) '(#t #f)))) 
              a))

(test '(11) 'in-value (for/list ([i (in-value 11)]) i))
(let-values ([(more? next) (sequence-generate (in-value 13))])
  (test #t more?)
  (test 13 next)
  (test #f more?))

;; check ranges on `in-vector', especially as a value
(test '() 'in-empty-vector (let ([v (in-vector '#())]) (for/list ([e v]) e)))
(test '() 'in-empty-vector (let ([v (in-vector '#() 0)]) (for/list ([e v]) e)))
(test '() 'in-empty-vector (let ([v (in-vector '#() 0 0)]) (for/list ([e v]) e)))
(test '() 'in-empty-vector (let ([v (in-vector '#(1) 1 1)]) (for/list ([e v]) e)))
(test '() 'in-empty-vector (let ([v (in-vector '#(1) 0 0)]) (for/list ([e v]) e)))
(test '(1) 'in-empty-vector (let ([v (in-vector '#(1) 0 1)]) (for/list ([e v]) e)))

(test '(1 2 3)
      'sequence-syntax-with-keywords
      (let ()
        (define (in-X #:x seq) seq)
        (for/list ([x (in-X #:x '(1 2 3))]) x)
        ;; => '(1 2 3)
        (define-sequence-syntax in-X* (lambda () #'in-X) (lambda (stx) #f))
        (for/list ([x (in-X* #:x '(1 2 3))]) x)))


;; extra tests for #:break and #:final
(test '((0 0) (0 1) (1 0) (1 1)) 'multi-level-break
      (for*/list ([i 4] [j 2] #:break (= i 2)) (list i j)))
(test '((1 0 0) (1 0 1) (1 1 0) (1 1 1)) 'multi-level-break
      (for/list ([i 5] #:when (odd? i) [j 2] #:when #t [k 2] #:break (= i 3))
        (list i j k)))
(test '((0 0) (0 1) (1 0) (1 1) (2 0)) 'outer-loop-final
      (for*/list ([i 4][j 2] #:final (= i 2)) (list i j)))
(test '((0 0) (0 1) (1 0) (1 1) (2 0)) 'outer-loop-final
      (for/list ([i 4]  #:final (= i 2) [j 2]) (list i j)))

(test '((0 0) (0 1) (1 0) (1 1)) 'break-and-final
 (for*/list ([i 4][j 2] #:final (= i 2) #:break (= i 2)) (list i j)))

(test '((0 0) (0 1) (1 0) (1 1)) 'break-and-final
 (for*/list ([i 4][j 2] #:break (= i 2) #:final (= i 2)) (list i j)))

(test '((0 1) (1 1)) 'skipped-final
      (for*/list ([i 4][j 2] #:final (= i 2) #:unless (= j 0)) (list i j)))

;; check #:break and #:final in body with exprs in between
(test (list 0 1) 'nested-body-break
      (for/list ([i 4]) (define j (add1 i)) #:break (= j 3) i))
(test (list 0 1 2) 'nested-body-final
      (for/list ([i 4]) (define j (add1 i)) #:final (= j 3) i))
(test '((0 0) (0 1) (1 0) (1 1)) 'nested-body-break-and-final
      (for*/list ([i 4][j 2]) 
        (define k i) #:final (= k 2)
        (define m i) #:break (= m 2) 
        (list i j)))
(test '((0 0) (0 1) (1 0) (1 1)) 'nested-body-break-and-final
      (for*/list ([i 4][j 2]) 
        (define m i) #:break (= m 2) 
        (define k i) #:final (= k 2)
        (list i j)))

;; for should discard any results and return void
(test (void) 'for-0-values (for ([x '(1 2 3)] [y '(a b c)]) (values)))
(test (void) 'for*-0-values (for* ([x '(1 2 3)] [y '(a b c)]) (values)))
(test (void) 'for-1-value (for ([x '(1 2 3)] [y '(a b c)]) (values x)))
(test (void) 'for*-1-value (for* ([x '(1 2 3)] [y '(a b c)]) (values x)))
(test (void) 'for-2-values (for ([x '(1 2 3)] [y '(a b c)]) (values x y)))
(test (void) 'for*-2-values (for* ([x '(1 2 3)] [y '(a b c)]) (values x y)))

;; for/fold with no accums
(test '() 'for/fold-no-accum 
      (call-with-values (λ () (for/fold () ([x '(1 2)]) (values))) (λ x x)))
(test '() 'for*/fold-no-accum 
      (call-with-values (λ () (for*/fold () ([x '(1 2)]) (values))) (λ x x)))
(err/rt-test (for/fold () ([x '(1 2)]) x) exn:fail:contract:arity?)
(err/rt-test (for*/fold () ([x '(1 2)]) x) exn:fail:contract:arity?)

(report-errs)
