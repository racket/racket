
(load-relative "loadtest.rktl")

(Section 'for)

(require "for-util.rkt")

;; These are copied from
;; https://github.com/racket/r6rs/blob/master/r6rs-lib/rnrs/arithmetic/fixnums-6.rkt
(define CS? (eq? 'chez-scheme (system-type 'vm)))
(define 64-bit? (fixnum? (expt 2 33)))
(define (least-fixnum) (if CS?
                           (if 64-bit? (- (expt 2 60)) -536870912)
                           (if 64-bit? (- (expt 2 62)) -1073741824)))
(define (greatest-fixnum) (if CS?
                              (if 64-bit? (- (expt 2 60) 1) +536870911)
                              (if 64-bit? (- (expt 2 62) 1) +1073741823)))

(define (five) 5)

(test-sequence [(0 1 2)] 3)
(test-sequence [(0 1 2)] (in-range 3))
(test-sequence [(3 4 5)] (in-range 3 6))
(test-sequence [(7 6 5)] (in-range 7 4 -1))
(test-sequence [(5 6)] (in-range (five) 7))
(test-sequence [(3 4)] (in-range 3 (five)))
(test-sequence [(0 5)] (in-range 0 10 (five)))
(test-sequence [(3.0 4.0 5.0)] (in-range 3.0 6.0))
(test-sequence [(3.0 3.5 4.0 4.5 5.0 5.5)] (in-range 3.0 6.0 0.5))
(test-sequence [(3.0 3.1 3.2)] (in-range 3.0 3.3 0.1))
(test-sequence [(6 7)] (in-inclusive-range 6 7))
(test-sequence [(3 4 5 6)] (in-inclusive-range 3 6))
(test-sequence [(7 6 5 4)] (in-inclusive-range 7 4 -1))
(test-sequence [(5 6 7)] (in-inclusive-range (five) 7))
(test-sequence [(3 4 5)] (in-inclusive-range 3 (five)))
(test-sequence [(0 5 10)] (in-inclusive-range 0 10 (five)))
(test-sequence [(3.0 4.0 5.0 6.0)] (in-inclusive-range 3.0 6.0))
(test-sequence [(3.0 3.5 4.0 4.5 5.0 5.5 6.0)] (in-inclusive-range 3.0 6.0 0.5))
(test-sequence [(#e3.0 #e3.1 #e3.2 #e3.3)] (in-inclusive-range #e3.0 #e3.3 #e0.1))
(test-sequence [(,(least-fixnum)
                 ,(+ (least-fixnum) 1))]
               (in-inclusive-range (least-fixnum)
                                   (+ (least-fixnum) 1)))
(test-sequence [(,(- (greatest-fixnum) 1)
                 ,(greatest-fixnum))]
               (in-inclusive-range (- (greatest-fixnum) 1)
                                   (greatest-fixnum)))
(err/rt-test (for/list ([x (in-range)]) x))
(err/rt-test (in-range))
(err/rt-test (for/list ([x (in-inclusive-range 1)]) x))
(err/rt-test (for/list ([x (in-naturals 0 1)]) x))
(err/rt-test (in-naturals 0 1))

(test-sequence [(a b c)] '(a b c))
(test-sequence [(a b c)] (in-list '(a b c)))
(err/rt-test (for/list ([x (in-list '(a b c) '?)]) x))
(err/rt-test (in-list '(a b c) '?))
(test-sequence [(a b c)] (mcons 'a (mcons 'b (mcons 'c empty))))
(test-sequence [(a b c)] (in-mlist (mcons 'a (mcons 'b (mcons 'c empty)))))
(err/rt-test (for/list ([x (in-mlist (mcons 'a (mcons 'b (mcons 'c empty))) '?)]) x))
(err/rt-test (in-mlist (mcons 'a (mcons 'b (mcons 'c empty))) '?))
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
(err/rt-test (for/list ([x (in-vector)]) x))
(err/rt-test (in-vector))
(test-sequence [(#\a #\b #\c)] "abc")
(test-sequence [(#\a #\u3bb #\c)] "a\u03BBc")
(test-sequence [(#\a #\b #\c)] (in-string "abc"))
(test-sequence [(#\a #\u3bb #\c)] (in-string "a\u03BBc"))
(test-sequence [(#\a #\b #\c)] (in-string "zzabc" 2))
(test-sequence [(#\a #\b #\c)] (in-string "zzabc" 2 #f))
(test-sequence [(#\a #\b #\c)] (in-string "zzabcqq" 2 5))
(test-sequence [(#\a #\b #\c)] (in-string "zzaxbyc" 2 #f 2))
(test-sequence [(#\a #\b #\c)] (in-string "zzaxbycy" 2 #f 2))
(err/rt-test (for/list ([x (in-string)]) x))
(err/rt-test (in-string))
(test-sequence [(65 66 67)] #"ABC")
(test-sequence [(65 66 67)] (in-bytes #"ABC"))
(test-sequence [(65 66 67)] (in-bytes #"ZZABC" 2))
(test-sequence [(65 66 67)] (in-bytes #"ZZABC" 2 #f))
(test-sequence [(65 66 67)] (in-bytes #"ZZABCQQ" 2 5))
(test-sequence [(65 66 67)] (in-bytes #"ZZAXBYC" 2 #f 2))
(test-sequence [(65 66 67)] (in-bytes #"ZZAXBYCY" 2 #f 2))
(err/rt-test (for/list ([x (in-bytes)]) x))
(err/rt-test (in-bytes))
(test-sequence [(#\a #\b #\c)] (in-input-port-chars (open-input-string "abc")))
(err/rt-test (for/list ([c (in-input-port-chars (open-input-string "abc") '?)]) c))
(err/rt-test (in-input-port-chars (open-input-string "abc") '?))
(test-sequence [(65 66 67)] (open-input-bytes #"ABC"))
(test-sequence [(65 66 67)] (in-input-port-bytes (open-input-bytes #"ABC")))
(err/rt-test (for/list ([b (in-input-port-bytes (open-input-bytes #"ABC") '?)]) b))
(err/rt-test (in-input-port-bytes (open-input-bytes #"ABC") '?))

;; Test optimized:
(test '(2) 'in-list-of-list (for/list ([v (in-list (list 1))]) (add1 v)))
(test '(0) 'in-mlist-of-mlist (for/list ([v (in-mlist (mlist 1))]) (sub1 v)))
(test '() 'in-mlist-of-mlist (for/list ([v (in-mlist null)]) v))

;; `in-mlist` only checks listness as much as explored
(test 1 'in-mlist-of-mlist (for/first ([v (in-mlist (mcons 1 2))]) v))

(test-sequence [(1 2 3)] (in-port read (open-input-string "1 2 3")))
(test-sequence [((123) 4)] (in-port read (open-input-string "(123) 4")))
(test-sequence [(65 66 67)] (in-port read-byte (open-input-string "ABC")))

(test-sequence [("abc" "def")] (in-lines (open-input-string "abc\ndef")))
(test-sequence [("abc" "def")] (in-lines (open-input-string "abc\ndef") 'any))
(err/rt-test (for/list ([l (in-lines (open-input-string "abc\ndef") 'any '?)]) l))
(err/rt-test (in-lines (open-input-string "abc\ndef") 'any '?))
(test-sequence [(#"abc" #"def")] (in-bytes-lines (open-input-string "abc\ndef")))
(test-sequence [(#"abc" #"def")] (in-bytes-lines (open-input-string "abc\ndef") 'any))
(err/rt-test (for/list ([l (in-bytes-lines (open-input-string "abc\ndef") 'any '?)]) l))
(err/rt-test (in-bytes-lines (open-input-string "abc\ndef") 'any '?))

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
(err/rt-test (for/list ([(x y) (in-indexed '(a b c) '?)]) x))
(err/rt-test (for/list ([(x y) (in-indexed '(a b c) '?)]) x))
(err/rt-test (for/list ([x (in-indexed '(a b c))]) x))
(err/rt-test (in-indexed '(a b c) '?))

;; Make sure `in-indexed` doesn't provide a bad position to the underlying
;; sequence
(let ()
  (define pre-poss '())
  (define post-poss '())
  (define naturals-sequence
    (make-do-sequence (thunk (values identity
                                     add1
                                     1
                                     (lambda (pos) (set! pre-poss (cons pos pre-poss)) #t)
                                     #f
                                     (lambda (pos val) (set! post-poss (cons pos post-poss)) #t)))))
  (let-values (((next? next) (sequence-generate (in-indexed naturals-sequence))))
    (for ((n 3)) (next)))
  (test '(3 2 1) values pre-poss)
  (test '(2 1) values post-poss))

(let ()
  (define (counter) (define n 0) (lambda ([d 1]) (set! n (+ d n)) n))
  (test-sequence [(1 2 3 4)] (for/list ([x (in-producer (counter))] [y (in-range 4)]) x))
  (test-sequence [(1 2 3 4)] (for/list ([x (in-producer (counter))] #:break (= x 5)) x))
  (test-sequence [(1 2 3 4)] (for/list ([x (in-producer (counter) 5)]) x))
  (test-sequence [(1/2 1 3/2 2 5/2 3 7/2 4 9/2)]
    (for/list ([x (in-producer (counter) 5 1/2)]) x))
  ;; test in-producer outside of for loops
  (test 6 sequence-ref (in-producer (counter)) 5)
  (err/rt-test (for/list ([x (in-producer)]) x))
  (err/rt-test (in-producer)))

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

(let ([five-odd-seq
       (lambda (pos pre post)
         (test-sequence [(1 3 5)]
                        (make-do-sequence (lambda ()
                                            (values add1
                                                    add1 ; "pre" next
                                                    add1
                                                    0
                                                    pos 
                                                    pre 
                                                    post)))))])
  (five-odd-seq (lambda (pos) (pos . < . 5))
                #f
                #f)
  (five-odd-seq #f
                (lambda (val) (val . < . 6))
                #f)
  (five-odd-seq #f
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

;; Check #:do:
(test (vector 0 0 1 0 1 2)
      'do
      (for/vector #:length 6
                  ([x (in-range 3)]
                   #:do [(define z (+ x 1))]
                   [y (in-range z)])
                  y))
(test (vector 0 0 1 0 1 2)
      'do
      (for/vector #:length 6
                  ([x (in-range 3)]
                   #:do [(define-syntax-rule (z) (+ x 1))]
                   [x (in-list '(oops))]
                   #:when #t
                   [y (in-range (z))])
                  y))
(test (vector 0 0 0 1 1 1)
      'do
      (for/vector #:length 6
                  ([x (in-range 3)]
                   #:do [(struct pt (x y))
                         (define (mk x y) (pt x y))]
                   #:when #t
                   [y (in-range 3)])
        (pt-x (mk x y))))

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
(err/rt-test (for/list ([i (in-value 1 2)]) i))
(err/rt-test (in-value 1 2))
(let-values ([(more? next) (sequence-generate (in-value 13))])
  (test #t more?)
  (test 13 next)
  (test #f more?))

(test 1 'in-value-bind-correctly (for/fold ([x #f])
                                           ([x (in-value 1)])
                                   x))
(test 2 'in-value-bind-correctly (for/fold ([x #f])
                                           ([x (values (in-value 2))])
                                   x))

(let ([x 'out]
      [prints '()])
  (for/fold ([x (begin
                  (set! prints (cons (list 'top x) prints))
                  'top)])
            ([x (in-list (begin
                           (set! prints (cons (list 'rhs x) prints))
                           (list 1 2 3)))])
    (set! prints (cons x prints))
    x)
  (test '(3 2 1 (top out) (rhs out)) values prints))

;; check ranges on `in-vector', especially as a value
(test '() 'in-empty-vector (let ([v (in-vector '#())]) (for/list ([e v]) e)))
(test '() 'in-empty-vector (let ([v (in-vector '#() 0)]) (for/list ([e v]) e)))
(test '() 'in-empty-vector (let ([v (in-vector '#() 0 0)]) (for/list ([e v]) e)))
(test '() 'in-empty-vector (let ([v (in-vector '#(1) 1)]) (for/list ([e v]) e)))
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

;; extra tests for #:result
(test '(0 1 2 3 4) 'for/fold-result-clause1
      (for/fold ([acc '()]
                 [seen (hash)]
                 #:result (reverse acc))
                ([x (in-list '(0 1 1 2 3 4 4 4))])
        (cond
          [(hash-ref seen x #f)
           (values acc seen)]
          [else (values (cons x acc)
                        (hash-set seen x #t))])))
(test '((4 3 2 1 0) (0 1 2 3 4) ())
      'for/fold-result-clause2
      (let-values ([(backwards forwards other)
                    (for/fold ([acc '()]
                               [seen (hash)]
                               #:result (values acc (reverse acc) '()))
                              ([x (in-list '(0 1 1 2 3 4 4 4))])
                      (cond
                        [(hash-ref seen x #f)
                         (values acc seen)]
                        [else (values (cons x acc)
                                      (hash-set seen x #t))]))])
        (list backwards forwards other)))
(test '(2 4 6) 'for/fold-result-clause3
      (for/fold ([acc '()]
                 [seen (hash)]
                 #:result (reverse acc))
                ([x (in-list '(0 0 1 1 2 2))]
                 [y (in-list '(2 2 3 3 4 4))])
        (define val (+ x y))
        (cond
          [(hash-ref seen val #f)
           (values acc seen)]
          [else (values (cons val acc)
                        (hash-set seen val #t))])))
(test '((6 4 2) (2 4 6) ())
      'for/fold-result-clause4
      (let-values ([(backwards forwards other)
                    (for/fold ([acc '()]
                               [seen (hash)]
                               #:result (values acc (reverse acc) '()))
                              ([x (in-list '(0 0 1 1 2 2))]
                               [y (in-list '(2 2 3 3 4 4))])
                      (define val (+ x y))
                      (cond
                        [(hash-ref seen val #f)
                         (values acc seen)]
                        [else (values (cons val acc)
                                      (hash-set seen val #t))]))])
        (list backwards forwards other)))
(test '(0 1 2 3 4) 'for*/fold-result-clause1
      (for*/fold ([acc '()]
                  [seen (hash)]
                  #:result (reverse acc))
                 ([x (in-list '(0 1 1 2 3 4 4 4))])
        (cond
          [(hash-ref seen x #f)
           (values acc seen)]
          [else (values (cons x acc)
                        (hash-set seen x #t))])))
(test '((4 3 2 1 0) (0 1 2 3 4) ())
      'for*/fold-result-clause2
      (let-values ([(backwards forwards other)
                    (for*/fold ([acc '()]
                                [seen (hash)]
                                #:result (values acc (reverse acc) '()))
                               ([x (in-list '(0 1 1 2 3 4 4 4))])
                      (cond
                        [(hash-ref seen x #f)
                         (values acc seen)]
                        [else (values (cons x acc)
                                      (hash-set seen x #t))]))])
        (list backwards forwards other)))
(test '(0 1 3 2 4 5) 'for*/fold-result-clause3
      (for*/fold ([acc '()]
                  [seen (hash)]
                  #:result (reverse acc))
                 ([xs (in-list '((0 1) (1 0) (3 2) (2 3) (4 5) (5 4)))]
                  [x (in-list xs)])
        (cond
          [(hash-ref seen x #f)
           (values acc seen)]
          [else (values (cons x acc)
                        (hash-set seen x #t))])))
(test '((5 4 2 3 1 0) (0 1 3 2 4 5) ())
      'for*/fold-result-clause4
      (let-values ([(backwards forwards other)
                    (for*/fold ([acc '()]
                                [seen (hash)]
                                #:result (values acc (reverse acc) '()))
                               ([xs (in-list '((0 1) (1 0) (3 2) (2 3) (4 5) (5 4)))]
                                [x (in-list xs)])
                      (cond
                        [(hash-ref seen x #f)
                         (values acc seen)]
                        [else (values (cons x acc)
                                      (hash-set seen x #t))]))])
        (list backwards forwards other)))
(test '((1 3 5) (2 4 6))
      'for/lists-result-clause1
      (for/lists (firsts seconds #:result (list firsts seconds))
                 ([pr '((1 . 2) (3 . 4) (5 . 6))])
        (values (car pr) (cdr pr))))
(test '((1 3 5) (2 4 6) ())
      'for/lists-result-clause2
      (let-values ([(firsts seconds other)
                    (for/lists (firsts
                                seconds
                                #:result (values firsts seconds '()))
                               ([pr '((1 . 2) (3 . 4) (5 . 6))])
                      (values (car pr) (cdr pr)))])
        (list firsts seconds other)))
(test '((1 3 5 7 9) (2 4 6 8 10))
      'for*/lists-result-clause1
      (for*/lists (firsts seconds #:result (list firsts seconds))
                  ([lst '(((1 . 2) (3 . 4) (5 . 6))
                          ((7 . 8) (9 . 10)))]
                   [pr (in-list lst)])
        (values (car pr) (cdr pr))))
(test '((1 3 5 7 9) (2 4 6 8 10) ())
      'for*/lists-result-clause2
      (let-values ([(firsts seconds other)
                    (for*/lists (firsts
                                 seconds
                                 #:result (values firsts seconds '()))
                                ([lst '(((1 . 2) (3 . 4) (5 . 6))
                                        ((7 . 8) (9 . 10)))]
                                 [pr (in-list lst)])
                      (values (car pr) (cdr pr)))])
        (list firsts seconds other)))

(test '()
      'for/lists-split-body
      (for/lists (lst) ([x '()])
        #:break #f
        x))
(test '()
      'for/lists-split-body
      (for/lists (lst) ([x '()])
        (define-syntax (m stx) #'0)
        m))
(test '()
      'for*/lists-split-body
      (for*/lists (lst) ([x '()])
        #:break #f
        x))
(test '()
      'for*/lists-split-body
      (for*/lists (lst) ([x '()])
        (define-syntax (m stx) #'0)
        m))

(test '(bad 1)
      'for/lists-weird-set!
      (for/lists (acc)
          ([v (in-range 2)])
        (unless (zero? v)
          (set! acc '(bad)))
        v))

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

;; for/fold result-arity checking:
(err/rt-test (begin (for/fold () ([i (in-range 10)]) 1) 1)
             exn:fail:contract:arity?
             #rx".*expected number of values not received.*")
(err/rt-test (begin (for/fold () () 1) 1)
             exn:fail:contract:arity?
             #rx".*expected number of values not received.*")
(err/rt-test (begin (for/fold ([x 1]) () (values 1 2)) 1)
             exn:fail:contract:arity?
             #rx"expected number of values not received")
(err/rt-test (begin (for/fold ([x 1] [y 2]) ([i (in-range 10)]) 1) 1)
             exn:fail:contract:arity?
             #rx".*expected number of values not received.*")
(test 1 'one (begin (for/fold () () (values)) 1))

;; iterator contract tests
(err/rt-test (for ([x (in-range (sqrt -1))]) x)
             exn:fail:contract?
             #rx"expected\\: real\\?")
(err/rt-test (for ([x (in-range 1 (sqrt -1))]) x)
             exn:fail:contract?
             #rx"expected\\: real\\?")
(err/rt-test (for ([x (in-range 1 2 (sqrt -1))]) x)
             exn:fail:contract?
             #rx"expected\\: real\\?")
(test (* 10 pi) 'in-range-with-reals
      (for/sum ([x (in-range 0 (+ (* 4 pi) .1) pi)]) x))
(err/rt-test (for ([x (in-naturals 1.1)]) x)
             exn:fail:contract?
             #rx"expected\\: exact-nonnegative-integer\\?")
(err/rt-test (for ([x (in-naturals -1)]) x)
             exn:fail:contract?
             #rx"expected\\: exact-nonnegative-integer\\?")
(err/rt-test (for ([x (in-list 1)]) x)
             exn:fail:contract?
             #rx"expected\\: list\\?")
(err/rt-test (for ([x (in-list (vector 1 2 3))]) x)
             exn:fail:contract?
             #rx"expected\\: list\\?")
(err/rt-test (for ([x (in-list (mcons 1 '()))]) x)
             exn:fail:contract?
             #rx"expected\\: list\\?")
(err/rt-test (for ([x (in-mlist (list 1 2 3))]) x)
             exn:fail:contract?
             #rx"expected\\: mpair\\?")
(err/rt-test (for ([x (in-vector '(1 2))]) x)
             exn:fail:contract?
             #rx"expected\\: vector")
(err/rt-test (for ([x (in-vector (vector 1 2) -1)]) x)
             exn:fail:contract?
             #rx"expected\\: exact-nonnegative-integer\\?")
(err/rt-test (for ([x (in-vector (vector 1 2) 10)]) x)
             exn:fail:contract?
             #rx"starting index is out of range")
(err/rt-test (for ([x (in-vector (vector 1 2) 1.1)]) x)
             exn:fail:contract?
             #rx"expected\\: exact-nonnegative-integer\\?")
(err/rt-test (for ([x (in-vector (vector 1 2) 0 1.1)]) x)
             exn:fail:contract?
             #rx"expected\\: exact-integer\\?")
(err/rt-test (for ([x (in-vector (vector 1 2) 0 2 1.1)]) x)
             exn:fail:contract?
             #rx"expected:.*exact-integer\\?")
(err/rt-test (for ([x (in-vector (vector 1 2) 0 2 0)]) x)
             exn:fail:contract?
             #rx"expected:.*not/c zero\\?")
(err/rt-test (for ([x (in-port (vector 1 2))]) x)
             exn:fail:contract?
             #rx"expected:.*procedure-arity-includes/c 1")
(err/rt-test (for ([x (in-input-port-bytes (vector 1 2))]) x)
             exn:fail:contract?
             #rx"expected: input-port\\?")
(err/rt-test (for ([x (in-hash (vector 1 2))]) x)
             exn:fail:contract?
             #rx"expected: hash\\?")
(err/rt-test (for ([x (in-hash-pairs (vector 1 2))]) x)
             exn:fail:contract?
             #rx"expected: hash\\?")
(err/rt-test (for ([x (in-hash-keys (vector 1 2))]) x)
             exn:fail:contract?
             #rx"expected: hash\\?")
(err/rt-test (for ([x (in-hash-values (vector 1 2))]) x)
             exn:fail:contract?
             #rx"expected: hash\\?")
(err/rt-test (for ([x (in-hash (hash 1 2))]) x)
             exn:fail:contract:arity?
             #rx"expected number of values not received")

(err/rt-test (for ([x (in-hash 1 2 3)]) x)
             exn:fail:contract:arity?)
(err/rt-test (for ([x (in-hash-keys 1 2 3)]) x)
             exn:fail:contract:arity?)
(err/rt-test (for ([x (in-hash-values 1 2 3)]) x)
             exn:fail:contract:arity?)
(err/rt-test (for ([x (in-hash-pairs 1 2 3)]) x)
             exn:fail:contract:arity?)

(err/rt-test (for/sum ([x (in-vector (vector 1 2) 2 -1 -1)]) x) ; pr 15227
             exn:fail:contract?
             #rx"starting index is out of range")
(err/rt-test (for/sum ([x (in-vector (vector) -1 -1 -1)]) x)
             exn:fail:contract?
             #rx"expected: exact-nonnegative-integer?")
(err/rt-test (for/sum ([x (in-vector (vector) 1 1 1)]) x)
             exn:fail:contract?
             #rx"starting index is out of range")
(err/rt-test (for/sum ([x (in-vector (vector 1) 1 2)]) x)
             exn:fail:contract?
             #rx"starting index is out of range")
(err/rt-test (for/sum ([x (in-vector (vector 1) 0 2)]) x)
             exn:fail:contract?
             #rx"stopping index is out of range")
(err/rt-test (for/sum ([x (in-vector (vector 1) 0 -1)]) x)
             exn:fail:contract?
             #rx"starting index more than stopping index, but given a positive step")
(err/rt-test (for/sum ([x (in-vector (vector 1) 0 1 -1)]) x)
             exn:fail:contract?
             #rx"starting index less than stopping index, but given a negative step")

;; for/fold & for*/fold syntax checking
(syntax-test #'(for/fold () bad 1)
             #rx".*for/fold:.*bad sequence binding clauses.*")
(syntax-test #'(for/fold () ([42 '()]) 1)
             #rx".*for/fold:.*bad sequence binding clause.*")
(syntax-test #'(for/fold ([0 42] [x 42]) ([z '()]) 1)
             #rx".*for/fold:.*expected an identifier to bind.*")
(syntax-test #'(for/fold ([x 42] [x 42]) ([z '()]) 1)
             #rx".*for/fold:.*duplicate identifier as accumulator binding.*")
(syntax-test #'(for/fold (#:result 42) bad 1)
             #rx".*for/fold:.*bad sequence binding clauses.*")
(syntax-test #'(for/fold (#:result 42) ([42 '()]) 1)
             #rx".*for/fold:.*bad sequence binding clause.*")
(syntax-test #'(for/fold ([0 42] [x 42] #:result 42) ([z '()]) 1)
             #rx".*for/fold:.*expected an identifier to bind.*")
(syntax-test #'(for/fold ([x 42] [x 42] #:result 42) ([z '()]) 1)
             #rx".*for/fold:.*duplicate identifier as accumulator binding.*")
(syntax-test #'(for/fold ([x 42] [x 42] #:wrong-keyword 42) ([z '()]) 1)
             #rx".*for/fold:.*invalid accumulator binding clause.*")

(syntax-test #'(for*/fold () bad 1)
             #rx".*for\\*/fold:.*bad sequence binding clauses.*")
(syntax-test #'(for*/fold () ([42 '()]) 1)
             #rx".*for\\*/fold:.*bad sequence binding clause.*")
(syntax-test #'(for*/fold ([0 42] [x 42]) ([z '()]) 1)
             #rx".*for\\*/fold:.*expected an identifier to bind.*")
(syntax-test #'(for*/fold ([x 42] [x 42]) ([z '()]) 1)
             #rx".*for\\*/fold:.*duplicate identifier as accumulator binding.*")
(syntax-test #'(for*/fold (#:result 42) bad 1)
             #rx".*for\\*/fold:.*bad sequence binding clauses.*")
(syntax-test #'(for*/fold (#:result 42) ([42 '()]) 1)
             #rx".*for\\*/fold:.*bad sequence binding clause.*")
(syntax-test #'(for*/fold ([0 42] [x 42] #:result 42) ([z '()]) 1)
             #rx".*for\\*/fold:.*expected an identifier to bind.*")
(syntax-test #'(for*/fold ([x 42] [x 42] #:result 42) ([z '()]) 1)
             #rx".*for\\*/fold:.*duplicate identifier as accumulator binding.*")
(syntax-test #'(for*/fold ([x 42] [x 42] #:wrong-keyword 42) ([z '()]) 1)
             #rx".*for\\*/fold:.*invalid accumulator binding clause.*")

(syntax-test #'(for ()) #rx".*missing body.*")
(syntax-test #'(for/vector ()) #rx".*missing body.*")

;; specific hash set iterators
(err/rt-test (for/sum ([x (in-immutable-set '(1 2))]) x)
             exn:fail:contract?
             #rx"not a hash set")
(err/rt-test (for/sum ([x (in-mutable-set '(1 2))]) x)
             exn:fail:contract?
             #rx"not a hash set")
(err/rt-test (for/sum ([x (in-weak-set '(1 2))]) x)
             exn:fail:contract?
             #rx"not a hash set")
(err/rt-test (for/sum ([x (in-weak-set (set 1 2))]) x)
             exn:fail:contract?
             #rx"wrong kind of hash set")
(err/rt-test (for/sum ([x (in-mutable-set (set 1 2))]) x)
             exn:fail:contract?
             #rx"wrong kind of hash set")
(err/rt-test (for/sum ([x (in-immutable-set (mutable-set 1 2))]) x)
             exn:fail:contract?
             #rx"wrong kind of hash set")
(err/rt-test (for/sum ([x (in-immutable-set (weak-set 1 2))]) x)
             exn:fail:contract?
             #rx"wrong kind of hash set")
(test 10 'in-hash-set (for/sum ([x (in-immutable-set (set 1 2 3 4))]) x))
(test 10 'in-hash-set (for/sum ([x (in-mutable-set (mutable-set 1 2 3 4))]) x))
(test 10 'in-hash-set (for/sum ([x (in-weak-set (weak-set 1 2 3 4))]) x))
(test 10 'in-hash-set (for/sum ([x (in-immutable-set (seteqv 1 2 3 4))]) x))
(test 10 'in-hash-set (for/sum ([x (in-mutable-set (mutable-seteqv 1 2 3 4))]) x))
(test 10 'in-hash-set (for/sum ([x (in-weak-set (weak-seteqv 1 2 3 4))]) x))
(test 10 'in-hash-set (for/sum ([x (in-immutable-set (seteq 1 2 3 4))]) x))
(test 10 'in-hash-set (for/sum ([x (in-mutable-set (mutable-seteq 1 2 3 4))]) x))
(test 10 'in-hash-set (for/sum ([x (in-weak-set (weak-seteq 1 2 3 4))]) x))
(test 10 'in-hash-set (for/sum ([x (in-immutable-set (list->set '(1 2 3 4)))]) x))
(test 10 'in-hash-set (for/sum ([x (in-mutable-set (list->mutable-set '(1 2 3 4)))]) x))
(test 10 'in-hash-set (for/sum ([x (in-weak-set (list->weak-set '(1 2 3 4)))]) x))
(test 30 'custom-in-hash-set
      (let ()
        (define-custom-set-types pos-set
          #:elem? positive?
          (λ (x y recur) (+ x y))
          (λ (x recur) x))
        (define imm
          (make-immutable-pos-set '(1 2 3 4)))
        (define m
          (make-mutable-pos-set '(1 2 3 4)))
        (define w
          (make-weak-pos-set '(1 2 3 4)))
        (+ (for/sum ([x (in-immutable-set imm)]) x)
           (for/sum ([x (in-mutable-set m)]) x)
           (for/sum ([x (in-weak-set w)]) x))))

(err/rt-test 
    (for ([(k v) (in-immutable-hash (make-hash '((1 . 2))))]) (+ k v))
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? immutable\\?")
(err/rt-test 
    (for ([(k v) (in-immutable-hash (make-weak-hash '((1 . 2))))]) (+ k v))
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? immutable\\?")
(err/rt-test 
    (for ([(k v) (in-mutable-hash (make-immutable-hash '((1 . 2))))]) (+ k v))
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? mutable\\?")
(err/rt-test 
    (for ([(k v) (in-mutable-hash (make-weak-hash '((1 . 2))))]) (+ k v))
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? mutable\\?")
(err/rt-test 
    (for ([(k v) (in-weak-hash (make-immutable-hash '((1 . 2))))]) (+ k v))
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? hash-weak\\?")
(err/rt-test 
    (for ([(k v) (in-weak-hash (make-hash '((1 . 2))))]) (+ k v))
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? hash-weak\\?")
;; keys
(err/rt-test 
    (for ([k (in-immutable-hash-keys (make-hash '((1 . 2))))]) k)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? immutable\\?")
(err/rt-test 
    (for ([k (in-immutable-hash-keys (make-weak-hash '((1 . 2))))]) k)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? immutable\\?")
(err/rt-test 
    (for ([k (in-mutable-hash-keys (make-immutable-hash '((1 . 2))))]) k) 
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? mutable\\?")
(err/rt-test 
    (for ([k (in-mutable-hash-keys (make-weak-hash '((1 . 2))))]) k)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? mutable\\?")
(err/rt-test 
    (for ([k (in-weak-hash-keys (make-immutable-hash '((1 . 2))))]) k)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? hash-weak\\?")
(err/rt-test 
    (for ([k (in-weak-hash-keys (make-hash '((1 . 2))))]) k)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? hash-weak\\?")
;; values
(err/rt-test 
    (for ([v (in-immutable-hash-values (make-hash '((1 . 2))))]) v)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? immutable\\?")
(err/rt-test 
    (for ([v (in-immutable-hash-values (make-weak-hash '((1 . 2))))]) v)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? immutable\\?")
(err/rt-test 
    (for ([v (in-mutable-hash-values (make-immutable-hash '((1 . 2))))]) v) 
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? mutable\\?")
(err/rt-test 
    (for ([v (in-mutable-hash-values (make-weak-hash '((1 . 2))))]) v)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? mutable\\?")
(err/rt-test 
    (for ([v (in-weak-hash-values (make-immutable-hash '((1 . 2))))]) v)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? hash-weak\\?")
(err/rt-test 
    (for ([v (in-weak-hash-values (make-hash '((1 . 2))))]) v)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? hash-weak\\?")
;; pairs
(err/rt-test 
    (for ([p (in-immutable-hash-pairs (make-hash '((1 . 2))))]) p)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? immutable\\?")
(err/rt-test 
    (for ([p (in-immutable-hash-pairs (make-weak-hash '((1 . 2))))]) p)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? immutable\\?")
(err/rt-test 
    (for ([p (in-mutable-hash-pairs (make-immutable-hash '((1 . 2))))]) p) 
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? mutable\\?")
(err/rt-test 
    (for ([p (in-mutable-hash-pairs (make-weak-hash '((1 . 2))))]) p)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? mutable\\?")
(err/rt-test 
    (for ([p (in-weak-hash-pairs (make-immutable-hash '((1 . 2))))]) p)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? hash-weak\\?")
(err/rt-test 
    (for ([p (in-weak-hash-pairs (make-hash '((1 . 2))))]) p)
  exn:fail:contract?
  #rx"expected:.*and/c hash\\? hash-weak\\?")

;; ----------------------------------------
;; Check that iteration over a list or stream doesn't implicitly
;; retain the head while the body is running

(when (custodian-memory-accounting-available?)
  (define-syntax-rule (check for/... in-... proc extra ...)
    (let* ([N 10]
           [vals (for/... ([i N]) (gensym))]
           [bs (for/list ([val vals]) (make-weak-box val))]
           [retained 0])
      (if proc
          (proc (lambda (i b extra ...)
                  (collect-garbage)
                  (when (weak-box-value b)
                    (set! retained (add1 retained))))
                vals bs (for/list ([val vals]) 'extra) ...)
          (for ([i (in-... vals)]
                [b (in-list bs)])
            (collect-garbage)
            (when (weak-box-value b)
              (set! retained (add1 retained)))))
      (test #t `(for/... in-... proc extra ... ,N ,retained) (< retained (/ N 2)))))
  (check for/list in-list #f)
  (check for/list values #f)
  (check for/stream in-stream #f)
  (check for/stream values #f)
  (define-syntax-rule (stop-before-in-list e)
    (values (stop-before (in-list e) (lambda (v) #f))))
  (check for/list stop-before-in-list #f)
  (define-syntax-rule (values-stop-before e)
    (values (values (stop-before e (lambda (v) #f)))))
  (check for/list values-stop-before #f)
  
  ;; Check `map`, etc., too
  (check for/list values map)
  (check for/list values map extra) ; 1 and 2 arguments are special-cased
  (check for/list values for-each)
  (check for/list values ormap)
  (check for/list values andmap)

  ;; similar check for `sequence-generate`
  (let ()
    (define l (cons (box 1) (cons (box 2) null)))
    (define wb (make-weak-box (car l)))

    (define-values (more? val) (sequence-generate l))
    (set! l #f)

    (let loop ()
      (cond
        [(more?)
         (define u (unbox (val)))
         (collect-garbage)
         (cons (cons u (weak-box-value wb)) (loop))]
        [else null])))

  ;; similar check for `sequence-generate*`
  (let ()
    (define l (cons (box 1) (cons (box 2) null)))
    (define wb (make-weak-box (car l)))

    (define-values (vals next) (sequence-generate* l))
    (set! l #f)

    (let loop ([vals vals] [next next])
      (cond
        [vals
         (define u (unbox (car vals)))
         (collect-garbage)
         (cons (cons u (weak-box-value wb))
               (let ()
                 (define-values (new-vals new-next) (next))
                 (loop new-vals new-next)))]
        [else null]))))

;; ----------------------------------------
;; `for/foldr`

(test '(0 1 2 3 4)
      'for/foldr-one-seq
      (for/foldr ([lst '()])
                 ([x (in-range 5)])
        (cons x lst)))
(test '((0 5) (1 6) (2 7) (3 8) (4 9))
      'for/foldr-two-seqs
      (for/foldr ([lst '()])
                 ([x (in-range 5)]
                  [y (in-range 5 10)])
        (cons (list x y) lst)))
(test '((0 5 10) (1 6 11) (2 7 12) (3 8 13) (4 9 14))
      'for/foldr-three-seqs
      (for/foldr ([lst '()])
                 ([x (in-range 5)]
                  [y (in-range 5 10)]
                  [z (in-range 10 15)])
        (cons (list x y z) lst)))

(test '(0 1 2)
      'for*/foldr-one-seq
      (for*/foldr ([lst '()])
                  ([x (in-range 3)])
        (cons x lst)))
(test '((0 0) (0 1) (0 2) (1 1) (1 2) (1 3) (2 2) (2 3) (2 4))
      'for*/foldr-two-seqs
      (for*/foldr ([lst '()])
                  ([x (in-range 3)]
                   [y (in-range x (+ x 3))])
        (cons (list x y) lst)))

(test '((0 0) (0 1) (0 2) (2 2) (2 3) (2 4))
      'for/foldr-guard
      (for/foldr ([lst '()])
                 ([x (in-range 3)]
                  #:unless (= x 1)
                  [y (in-range x (+ x 3))])
        (cons (list x y) lst)))
(test '((0 0) (0 1) (0 2) (1 1) (1 2) (1 3) (2 2))
      'for*/foldr-break
      (for*/foldr ([lst '()])
                  ([x (in-range 3)]
                   [y (in-range x (+ x 3))]
                   #:break (and (= x 2) (= y 3)))
        (cons (list x y) lst)))
(test '((0 0) (0 1) (0 2) (1 1) (1 2) (1 3) (2 2) (2 3))
      'for*/foldr-final
      (for*/foldr ([lst '()])
                  ([x (in-range 3)]
                   [y (in-range x (+ x 3))]
                   #:final (and (= x 2) (= y 3)))
        (cons (list x y) lst)))

(test (list 0 0 1 0 1 2)
      'do
      (for/foldr ([acc null])
                 ([x (in-range 3)]
                  #:do [(define-syntax-rule (z) (+ x 1))]
                  [y (in-range (z))])
         (cons y acc)))

(test '(408 . 20400)
      'for/foldr-two-accs
      (for/foldr ([a 1] [b 1] #:result (cons a b))
                 ([n (in-range 5)])
        (values b (* a (+ b n)))))

(test #t 'for/foldr-delay-init
      (for/foldr ([acc (error "never gets here")] #:delay)
                 ([v (in-value #t)])
        v))

(test '(0 1 4 9 16)
      'for/foldr-stream-finite
      (stream->list
       (for/foldr ([s empty-stream] #:delay)
                  ([v (in-range 5)])
         (stream-cons (sqr v) (force s)))))
(test '(0 1 4 9 16)
      'for/foldr-stream-infinite
      (stream->list
       (stream-take
        (for/foldr ([s (error "never gets here")] #:delay)
                   ([v (in-naturals)])
          (stream-cons (sqr v) (force s)))
        5)))

(test '(0 1 4 9 16)
      'for/foldr-stream-finite/thunk
      (stream->list
       (for/foldr ([s empty-stream] #:delay-with thunk)
                  ([v (in-range 5)])
         (stream-cons (sqr v) (s)))))
(test '(0 1 4 9 16)
      'for/foldr-stream-infinite/thunk
      (stream->list
       (stream-take
        (for/foldr ([s (error "never gets here")] #:delay-with thunk)
                   ([v (in-naturals)])
          (stream-cons (sqr v) (s)))
        5)))

(test '(4 9 16 4 9 16 4 9 16 4)
      'for/foldr-stream-circular
      (letrec ([s (for/foldr ([s s] #:delay)
                             ([v (in-range 2 5)])
                    (stream-cons (sqr v) (force s)))])
        (stream->list (stream-take s 10))))
(test '(0 1 1 2 3 5 8 13 21 34)
      'for/foldr-stream-self-iter
      (letrec ([fibs (stream* 0 1 (for/foldr ([more (error "never gets here")] #:delay)
                                             ([a (in-stream fibs)]
                                              [b (in-stream (stream-rest fibs))])
                                    (stream-cons (+ a b) (force more))))])
        (stream->list (stream-take fibs 10))))

(test '(0 -1 2 -3 0 1 -2 3 0 -1)
      'for/foldr-stream-twist
      (letrec-values ([(s1 s2) (for/foldr ([s1 s2] [s2 s1] #:delay)
                                          ([n (in-range 4)])
                                 (values (stream-cons n (force s2))
                                         (stream-cons (- n) (force s1))))])
        (stream->list (stream-take s1 10))))
(test '(0 -1 2 -3 0 1 -2 3 0 -1)
      'for/foldr-stream-twist/thunk
      (letrec-values ([(s1 s2)
                       (for/foldr ([s1 s2] [s2 s1] #:delay-with thunk #:delay-as get-next)
                                  ([n (in-range 4)])
                         (define next (delay (get-next)))
                         (values (stream-cons n (let-values ([(s1 s2) (force next)]) s2))
                                 (stream-cons (- n) (let-values ([(s1 s2) (force next)]) s1))))])
        (stream->list (stream-take s1 10))))

;; `#:delay` applies inside `#:result`
(let ()
  (define evaluated? #f)
  (define result (for/foldr ([acc (set! evaluated? #t)] #:result acc #:delay) ()
                   (force acc)))
  (test #f 'for/foldr-result-delay-1 evaluated?)
  (test (void) 'for/foldr-result-delay-2 (force result))
  (test #t 'for/foldr-result-delay-3 evaluated?))

;; same expansion (from @soegaard)
(let ()
  (test #t 'same-expansion-for-integer-clause
        (equal? (syntax->datum (expand #'(for ([j 100]) j)))
                (syntax->datum (expand #'(for ([j (in-range '100)]) j)))))

  (test #t 'same-expansion-for-list-clause
        (equal? (syntax->datum (expand #'(for ([j '(1 2 3)]) j)))
                (syntax->datum (expand #'(for ([j (in-list '(1 2 3))]) j)))))

  (test #t 'same-expansion-for-vector-clause
        (equal? (syntax->datum (expand #'(for ([j #(1 2 3)]) j)))
                (syntax->datum (expand #'(for ([j (in-vector #(1 2 3))]) j)))))

  (test #t 'same-expansion-for-hash-clause
        (equal? (syntax->datum (expand #'(for ([(i j) #hash((1 . 2) (3 . 4))]) j)))
                (syntax->datum (expand #'(for ([(i j) (in-immutable-hash #hash((1 . 2) (3 . 4)))]) j)))))

  (test #t 'same-expansion-for-string-clause
        (equal? (syntax->datum (expand #'(for ([j "abc"]) j)))
                (syntax->datum (expand #'(for ([j (in-string "abc")]) j)))))

  (test #t 'same-expansion-for-bytes-clause
        (equal? (syntax->datum (expand #'(for ([j #"abc"]) j)))
                (syntax->datum (expand #'(for ([j (in-bytes #"abc")]) j))))))

;; #%datum is picked up (from @gus-massa)
(let ()
  (local-require (only-in racket (#%datum #%old-datum)))
  (define-syntax-rule (#%datum . x) (#%old-datum . 3))
  (test-sequence [(0 1 2)] 5))

;; for expanded in expression context
(module test-for-expansion racket
  (provide foo%)
  (define foo%
    (class object%
      (super-new)
      (define/public (bar) 1)
      (for ([x (bar)]) #t))))

(let ()
  (local-require 'test-for-expansion)
  (test #t object? (new foo%)))

(err/rt-test (for/list ([x -1]) x))
(err/rt-test (for/list ([x 1.5]) x))

;; ----------------------------------------
;; splicing clauses

(define-splicing-for-clause-syntax parallel3
  (lambda (stx)
    (syntax-case stx ()
      [(_ n m) #'([n (in-range 3)]
                  [m (in-range 3)])])))

(define-splicing-for-clause-syntax cross3
  (lambda (stx)
    (syntax-case stx ()
      [(_ n m) #'([n (in-range 3)]
                  #:when #t
                  [m (in-range 3)])])))

(test '((0 0) (1 1) (2 2))
      'parallel3
      (for/list (#:splice (parallel3 n m))
        (list n m)))
(test '((0 0) (1 1) (2 2))
      'parallel3
      (for*/list (#:splice (parallel3 n m))
        (list n m)))

(test '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))
      'cross3
      (for/list (#:splice (cross3 n m))
        (list n m)))
(test '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2))
      'cross3
      (for*/list (#:splice (cross3 n m))
        (list n m)))

(define-splicing-for-clause-syntax final-if-7
  (lambda (stx)
    (syntax-case stx ()
      [(_ i) #'(#:final (= i 7))])))
(test '(0 1 2 3 4 5 6 7)
      'final-if-7
      (for/list ([i (in-range 10)] #:splice (final-if-7 i)) i))

;; ----------------------------------------
;; Make sure explicitly quoted datum doesn't need to have a `#%datum` binding

(test '(0)
      eval-syntax #`(for/list ([i (quote #,(datum->syntax #f 1))]) i))
(test '(1)
      eval-syntax #`(for/list ([i (quote #,(datum->syntax #f '(1)))]) i))
(test '(1)
      eval-syntax #`(for/list ([i (quote #,(datum->syntax #f '#(1)))]) i))
(test '(#\1)
      eval-syntax #`(for/list ([i (quote #,(datum->syntax #f "1"))]) i))
(test '(49)
      eval-syntax #`(for/list ([i (quote #,(datum->syntax #f #"1"))]) i))
(test '(1)
      eval-syntax #`(for/list ([(k v) (quote #,(datum->syntax #f #hash((1 . 0))))]) k))

;; ----------------------------------------

(report-errs)
