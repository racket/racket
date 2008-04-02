
(load-relative "loadtest.ss")

(Section 'list)

(require scheme/list)

(test (list 1 2 3 4) foldl cons '() (list 4 3 2 1))
(test (list 1 2 3 4) foldr cons '() (list 1 2 3 4))
(test (list (list 5 6) (list 3 4) (list 1 2))
      foldl (lambda (x y sofar) (cons (list x y) sofar))
      '()
      (list 1 3 5)
      (list 2 4 6))
(test (list (list 1 2) (list 3 4) (list 5 6))
      foldr (lambda (x y sofar) (cons (list x y) sofar))
      '()
      (list 1 3 5)
      (list 2 4 6))

(arity-test foldl 3 -1)
(arity-test foldr 3 -1)

(test '(1 2 3) filter number? '(1 a 2 b 3 c d))
(test '() filter string? '(1 a 2 b 3 c d))
(err/rt-test (filter string? '(1 2 3 . 4)) exn:application:mismatch?)
(err/rt-test (filter 2 '(1 2 3)))
(err/rt-test (filter cons '(1 2 3)))
(arity-test filter 2 2)

(test '(0 1 2) memf add1 '(0 1 2))
(test '(2 (c 17)) memf number? '((a 1) (0 x) (1 w) 2 (c 17)))
(test '("ok" (2 .7) c) memf string? '((a 0) (0 a) (1 w) "ok" (2 .7) c))
(err/rt-test (memf cons '((1) (2) (3))))
(err/rt-test (memf string? '((1) (2) (3) . 4)) exn:application:mismatch?)

(err/rt-test (assf add1 '(0 1 2)) exn:application:mismatch?)
(test '(0 x) assf number? '((a 1) (0 x) (1 w) (2 r) (c 17)))
(test '("ok" . 10) assf string? '((a 0) (0 a) (1 w) ("ok" . 10) (2 .7) c))
(err/rt-test (assf cons '((1) (2) (3))))
(err/rt-test (assf string? '((1) (2) (3) . 4)) exn:application:mismatch?)

;; ---------- sort ----------
(test '("a" "b" "c" "c" "d" "e" "f")
      sort
      '("d" "f" "e" "c" "a" "c" "b")
      string<?)
(let ()
  (define (car< x y) (< (car x) (car y)))
  (define (random-list n range)
    (let loop ([n n] [r '()])
      (if (zero? n) r (loop (sub1 n) (cons (list (random range)) r)))))
  (define (sort* lst)
    (let ([s1 (sort lst car<)]
          [s2 (sort lst < #:key car)]
          [s3 (sort lst < #:key car #:cache-keys? #t)])
      (test #t andmap eq? s1 s2)
      (test #t andmap eq? s1 s3)
      s1))
  (define (test-sort len times)
    (or (zero? times)
        (and (let* ([rand (random-list len (if (even? times) 1000000 10))]
                    [orig< (lambda (x y) (memq y (cdr (memq x rand))))]
                    [sorted (sort* rand)]
                    [l1 (reverse (cdr (reverse sorted)))]
                    [l2 (cdr sorted)])
               (and (= (length sorted) (length rand))
                    (andmap (lambda (x1 x2)
                              (and (not (car< x2 x1)) ; sorted?
                                   (or (car< x1 x2) (orig< x1 x2)))) ; stable?
                            l1 l2)))
             (test-sort len (sub1 times)))))
  (test #t test-sort    1  10)
  (test #t test-sort    2  20)
  (test #t test-sort    3  60)
  (test #t test-sort    4 100)
  (test #t test-sort    5 100)
  (test #t test-sort   10 100)
  (test #t test-sort  100 100)
  (test #t test-sort 1000 100)
  ;; test stability
  (test '((1) (2) (3 a) (3 b) (3 c)) sort* '((3 a) (1) (3 b) (2) (3 c)))
  ;; test short lists (+ stable)
  (test '() sort* '())
  (test '((1 1)) sort* '((1 1)))
  (test '((1 2) (1 1)) sort* '((1 2) (1 1)))
  (test '((1) (2)) sort* '((2) (1)))
  (for-each (lambda (l) (test '((0 3) (1 1) (1 2)) sort* l))
            '(((1 1) (1 2) (0 3))
              ((1 1) (0 3) (1 2))
              ((0 3) (1 1) (1 2))))
  (for-each (lambda (l) (test '((0 2) (0 3) (1 1)) sort* l))
            '(((1 1) (0 2) (0 3))
              ((0 2) (1 1) (0 3))
              ((0 2) (0 3) (1 1)))))
;; test #:key and #:cache-keys?
(let ()
  (define l '((0) (9) (1) (8) (2) (7) (3) (6) (4) (5)))
  (define sorted '((0) (1) (2) (3) (4) (5) (6) (7) (8) (9)))
  ;; can't use keyword args, so use values and the sort call
  (test sorted values (sort l < #:key car))
  (let ([c1 0] [c2 0] [touched '()])
    (test sorted values
          (sort l (lambda (x y) (set! c1 (add1 c1)) (< x y))
                #:key (lambda (x)
                        (set! c2 (add1 c2))
                        (set! touched (cons x touched))
                        (car x))))
    ;; test that the number of key uses is half the number of comparisons
    (test #t = (* 2 c1) c2)
    ;; and that this is larger than the number of items in the list
    (test #t < (length l) c2)
    ;; and that every item was touched
    (test null remove* touched l))
  (let ([c 0] [touched '()])
    ;; now cache the keys
    (test sorted values
          (sort l <
                #:key (lambda (x)
                        (set! c (add1 c))
                        (set! touched (cons x touched))
                        (car x))
                #:cache-keys? #t))
    ;; test that the number of key uses is the same as the list length
    (test #t = c (length l))
    ;; and that every item was touched
    (test null remove* touched l))
  (let* ([c 0] [getkey (lambda (x) (set! c (add1 c)) x)])
    ;; either way, we never use the key proc on no arguments
    (test '() values (sort '() < #:key getkey #:cache-keys? #f))
    (test '() values (sort '() < #:key getkey #:cache-keys? #t))
    (test #t = c 0)
    ;; we also don't use it for 1-arg lists
    (test '(1) values (sort '(1) < #:key getkey #:cache-keys? #f))
    (test #t = c 0)
    ;; but we do use it once if caching happens (it's a consistent interface)
    (test '(1) values (sort '(1) < #:key getkey #:cache-keys? #t))
    (test #t = c 1)
    ;; check a few other short lists
    (test '(1 2) values (sort '(2 1) < #:key getkey #:cache-keys? #t))
    (test '(1 2 3) values (sort '(2 3 1) < #:key getkey #:cache-keys? #t))
    (test '(1 2 3 4) values (sort '(4 2 3 1) < #:key getkey #:cache-keys? #t))
    (test #t = c 10)))

;; ---------- take/drop ----------
(let ()
  (define tests
    ;; ------call------- --take--- --drop---
    '([(? (a b c d) 2)   (a b)     (c d)    ]
      [(? (a b c d) 0)   ()        (a b c d)]
      [(? (a b c d) 4)   (a b c d) ()       ]
      [(? (a b c . d) 1) (a)       (b c . d)]
      [(? (a b c . d) 3) (a b c)   d        ]
      [(? 99 0)          ()        99       ]))
  (for ([t tests])
    (apply test (cadr t)  take (cdar t))
    (apply test (caddr t) drop (cdar t)))
  (arity-test take 2 2)
  (arity-test drop 2 2)
  (err/rt-test (drop 1 1) exn:application:mismatch?)
  (err/rt-test (take 1 1) exn:application:mismatch?)
  (err/rt-test (drop '(1 2 3) 2.0))
  (err/rt-test (take '(1 2 3) 2.0))
  (err/rt-test (drop '(1) '(1)))
  (err/rt-test (take '(1) '(1)))
  (err/rt-test (drop '(1) -1))
  (err/rt-test (take '(1) -1))
  (err/rt-test (drop '(1) 2) exn:application:mismatch?)
  (err/rt-test (take '(1) 2) exn:application:mismatch?)
  (err/rt-test (drop '(1 2 . 3) 3) exn:application:mismatch?)
  (err/rt-test (take '(1 2 . 3) 3) exn:application:mismatch?))

;; ---------- append* ----------
(let ()
  (test '(0 1 0 2 0 3) append* (map (lambda (x) (list 0 x)) '(1 2 3)))
  (test '(1 2 3 4 5 6 7 8 9) append* '(1 2 3) '(4 5) '((6 7 8) (9))))

;; ---------- flatten ----------
(let ()
  (define (all-sexps n)
    (if (zero? n)
      '(x ())
      (let ([r (all-sexps (sub1 n))])
        (append r (for*/list ([x r] [y r]) (cons x y))))))
  (define sexps (all-sexps 3)) ; can use 4 on fast machines
  (define (flat? x) (and (list? x) (andmap (lambda (x) (eq? 'x x)) x)))
  (for ([x sexps]) (test #t flat? (flatten x))))

(report-errs)
