
(load-relative "loadtest.ss")

(Section 'for)

(require scheme/generator)

(define-syntax (test-multi-generator stx)
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
           (test (for/first ([(id ...) gen])
                   (list id ...))
                 'gen (for/or ([(id ...) gen])
                         (car (member (list id ...) `((v2 ...) ...)))))
           (void)))]))

(define-syntax test-generator
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
       (test-multi-generator [seq] gen))]
    [(_ seqs gen)
     (test-multi-generator seqs gen)]))

(test-generator [(0 1 2)] (in-range 3))
(test-generator [(3 4 5)] (in-range 3 6))
(test-generator [(7 6 5)] (in-range 7 4 -1))

(test-generator [(a b c)] '(a b c))
(test-generator [(a b c)] (in-list '(a b c)))
(test-generator [(a b c)] #(a b c))
(test-generator [(a b c)] (in-vector #(a b c)))
(test-generator [(b c d)] (in-vector #(a b c d) 1))
(test-generator [(b c d)] (in-vector #(a b c d e) 1 4))
(test-generator [(b d f)] (in-vector #(a b c d e f g h) 1 7 2))
(test-generator [(h f d)] (in-vector #(a b c d e f g h) 7 1 -2))
(test-generator [(b d f)] (in-vector #(a b c d e f g h) 1 6 2))
(test-generator [(h f d)] (in-vector #(a b c d e f g h) 7 2 -2))
(test-generator [(#\a #\b #\c)] "abc")
(test-generator [(#\a #\b #\c)] (in-string "abc"))
(test-generator [(#\a #\b #\c)] (in-string "zzabc" 2))
(test-generator [(#\a #\b #\c)] (in-string "zzabc" 2 #f))
(test-generator [(#\a #\b #\c)] (in-string "zzabcqq" 2 5))
(test-generator [(#\a #\b #\c)] (in-string "zzaxbyc" 2 #f 2))
(test-generator [(#\a #\b #\c)] (in-string "zzaxbycy" 2 #f 2))
(test-generator [(65 66 67)] #"ABC")
(test-generator [(65 66 67)] (in-bytes #"ABC"))
(test-generator [(65 66 67)] (in-bytes #"ZZABC" 2))
(test-generator [(65 66 67)] (in-bytes #"ZZABC" 2 #f))
(test-generator [(65 66 67)] (in-bytes #"ZZABCQQ" 2 5))
(test-generator [(65 66 67)] (in-bytes #"ZZAXBYC" 2 #f 2))
(test-generator [(65 66 67)] (in-bytes #"ZZAXBYCY" 2 #f 2))
(test-generator [(#\a #\b #\c)] (in-input-port-chars (open-input-string "abc")))
(test-generator [(65 66 67)] (open-input-bytes #"ABC"))
(test-generator [(65 66 67)] (in-input-port-bytes (open-input-bytes #"ABC")))

(test-generator [(1 2 3)] (in-port read (open-input-string "1 2 3")))
(test-generator [((123) 4)] (in-port read (open-input-string "(123) 4")))
(test-generator [(65 66 67)] (in-port read-byte (open-input-string "ABC")))

(test-generator [("abc" "def")] (in-lines (open-input-string "abc\ndef")))
(test-generator [(#"abc" #"def")] (in-bytes-lines (open-input-string "abc\ndef")))

(test-generator [(0 1 2 3 4 5)] (in-sequences (in-range 6)))
(test-generator [(0 1 2 3 4 5)] (in-sequences (in-range 4) '(4 5)))
(test-generator [(0 1 2 3 4 5)] (in-sequences (in-range 6) '()))
(test-generator [(0 1 2 3 4 5)] (in-sequences '() (in-range 4) '() '(4 5)))
(test-generator [(0 1 2 3 4 5)] (in-sequences (in-range 0 2) (in-range 2 4) (in-range 4 6)))
(test-generator [(0 1 2 3 4 5)] (in-sequences (in-range 0 2)
                                              (in-sequences (in-range 2 4) (in-range 4 6))))
(test-generator [(0 1 2 3 #\a #\b #\c) (10 11 12 13 #\A #\B #\C)]
                (in-sequences (in-parallel (in-range 0 4) (in-range 10 14))
                              (in-parallel "abc" "ABC")))

;; use in-parallel to get a finite number of items
(test-generator [(0 1 2 3 0 1 2 3) (0 1 2 3 4 5 6 7)]
                (in-parallel (in-cycle (in-range 0 4)) (in-range 0 8)))
(test-generator [(0 1 2 3 4 5 6 7) (0 1 2 0 1 2 0 1)]
                (in-parallel (in-range 0 8) (in-cycle (in-range 0 3))))
(test-generator [(0 1 2 3 2 1 0 1) (0 1 2 3 4 5 6 7)]
                (in-parallel (in-cycle (in-range 0 4) (in-range 2 0 -1)) (in-range 0 8)))

(test-generator [(0 1 2) (a b c)] (in-parallel (in-range 3) (in-list '(a b c))))
(test-generator [(0 1 2) (a b c)] (in-parallel (in-range 10) (in-list '(a b c))))
(test-generator [(0 1 2) (a b c)] (in-parallel (in-range 3) (in-list '(a b c d))))
(test-generator [(0 1 2) (a b c)] (in-parallel (in-range 3) '(a b c)))

(test-generator [(a b c)] (stop-after (in-list '(a b c d e)) (lambda (x) (equal? x 'c))))
(test-generator [(a b c)] (stop-before (in-list '(a b c d e)) (lambda (x) (equal? x 'd))))
(test-generator [(3 4 5)] (stop-before (in-naturals 3) (lambda (x) (= x 6))))

(test-generator [(a b c) (0 1 2)] (in-indexed '(a b c)))

(test-generator [(1 2 3 4 5)]
  (parameterize ([current-input-port (open-input-string "1 2 3\n4 5")])
    (for/list ([i (in-producer read eof)]) i)))
(test-generator [(1 2 3 4 5)]
  (for/list ([i (in-producer read eof (open-input-string "1 2 3\n4 5"))]) i))
(test-generator [("1 2 3" "4 5")]
  (for/list ([i (in-producer read-line eof-object? (open-input-string "1 2 3\n4 5"))]) i))
(test-generator [((1 2) (3 4) (5 ,eof))]
  (for/list ([(i j)
              (in-producer (lambda (p) (values (read p) (read p)))
                           (lambda (x y) (and (eof-object? x) (eof-object? y)))
                           (open-input-string "1 2 3\n4 5"))])
    (list i j)))

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

(test-generator [(0 1 2)] (in-generator (yield 0) (yield 1) (yield 2)))
(let ([g (lambda () (in-generator (yield 0) (yield 1) (yield 2)))])
  (test-generator [(0 1 2)] (g)))
(test '((1 0) (2 1) (3 2)) 'indexed-generator
      (for/list ([(x i) (in-indexed (in-generator (yield 1) (yield 2) (yield 3)))])
        (list x i)))

(let ([helper (lambda (i)
                (yield (add1 i)))])
  (test '(1 2 3) 'parameterized-yield
        (for/list ([x (in-generator (helper 0) (helper 1) (helper 2))])
                  x)))

(let ([g (lambda () (generator () (yield 1) (yield 2) (yield 3)))])
  (let ([g (g)]) (test '(1 2 3) list (g) (g) (g)))
  (let ([g (g)]) (test '(1 2 3 10 10) list (g) (g) (g) (g 10) (g)))
  (let ([g (generator () (yield (yield (yield 1))))])
    (test '(1 2 3 4 4 4) list (g) (g 2) (g 3) (g 4) (g) (g)))
  (let ([g (g)])
    (test '(fresh 1 suspended 2 suspended 3 suspended last done)
          list (generator-state g) (g)
               (generator-state g) (g)
               (generator-state g) (g)
               (generator-state g) (g 'last)
               (generator-state g)))
  (letrec ([g (generator () (yield (generator-state g))
                            (yield (generator-state g)))])
    (test '(fresh running suspended running suspended last done)
          list (generator-state g) (g)
               (generator-state g) (g)
               (generator-state g) (g 'last)
               (generator-state g))))

(let* ([helper (lambda (pred num)
                 (for ([i (in-range 0 3)]) (yield (pred (+ i num)))))]
       [g1 (generator () (helper odd? 1) (yield 'odd))]
       [g2 (generator () (helper even? 1) (yield 'even))])
  (test '(#t #f #f #t #t #f odd even) 'yield-helper
        (list (g1) (g2) (g1) (g2) (g1) (g2) (g1) (g2))))

(test '(1 2 3)
      'sequence->generator-1
      (let ([maker (sequence->generator '(1 2 3))])
        (list (maker) (maker) (maker))))

(test '(1 2 3)
      'sequence->generator-2
      (let ([maker (sequence->generator (in-list '(1 2 3)))])
        (list (maker) (maker) (maker))))

(test '(0 1 2 3 4)
      'sequence->generator-3
      (let ([maker (sequence->generator (in-range 0 5))])
        (list (maker) (maker) (maker) (maker) (maker))))

(test '(0 1 2 3 4)
      'sequence->generator-4
      (let ([maker (sequence->generator (in-naturals))])
        (list (maker) (maker) (maker) (maker) (maker))))

(test '(1 2 3 1 2 3)
      'sequence->repeated-generator
      (let ([maker (sequence->repeated-generator '(1 2 3))])
        (list (maker) (maker) (maker)
              (maker) (maker) (maker))))

(report-errs)
