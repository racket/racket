
(load-relative "loadtest.rkt")

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

(err/rt-test (foldl 'list 0 10))
(err/rt-test (foldl list 0 10))
(err/rt-test (foldl add1 0 '()))
(err/rt-test (foldl cons 0 '() '()))
(err/rt-test (foldl list 0 '() 10))
(err/rt-test (foldl list 0 '() '() 10))
(err/rt-test (let/ec k (foldl k 0 '(1 2) '(1 2 3))))
(err/rt-test (let/ec k (foldl k 0 '(1 2) '(1 2) '(1 2 3))))
(err/rt-test (foldr 'list 0 10))
(err/rt-test (foldr list 0 10))
(err/rt-test (foldr add1 0 '()))
(err/rt-test (foldr cons 0 '() '()))
(err/rt-test (foldr list 0 '() 10))
(err/rt-test (foldr list 0 '() '() 10))
(err/rt-test (let/ec k (foldr k 0 '(1 2) '(1 2 3))))
(err/rt-test (let/ec k (foldr k 0 '(1 2) '(1 2) '(1 2 3))))

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

;; ---------- last, last-pair ----------
(let ()
  (test 3        last '(1 2 3))
  (test '(3)     last-pair '(1 2 3))
  (err/rt-test  (last '(1 2 3 . 4)))
  (test '(3 . 4) last-pair '(1 2 3 . 4))
  (err/rt-test  (last '()))
  (err/rt-test  (last 1))
  (err/rt-test  (last-pair '()))
  (err/rt-test  (last-pair 1)))

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
              ((0 2) (0 3) (1 1))))
  ;; exhaustive tests for 2 and 3 item lists
  (for-each (lambda (l) (test '((1 x) (2 y)) sort* l))
            '(((1 x) (2 y))
              ((2 y) (1 x))))
  (for-each (lambda (l) (test '((1 x) (2 y) (3 z)) sort* l))
            '(((1 x) (2 y) (3 z))
              ((2 y) (1 x) (3 z))
              ((2 y) (3 z) (1 x))
              ((3 z) (2 y) (1 x))
              ((3 z) (1 x) (2 y))
              ((1 x) (3 z) (2 y)))))
;; test #:key and #:cache-keys?
(let ()
  (define l '((0) (9) (1) (8) (2) (7) (3) (6) (4) (5)))
  (define sorted '((0) (1) (2) (3) (4) (5) (6) (7) (8) (9)))
  (test sorted sort l < #:key car)
  (let ([c1 0] [c2 0] [touched '()])
    (test sorted
          sort l (lambda (x y) (set! c1 (add1 c1)) (< x y))
                 #:key (lambda (x)
                         (set! c2 (add1 c2))
                         (set! touched (cons x touched))
                         (car x)))
    ;; test that the number of key uses is half the number of comparisons
    (test #t = (* 2 c1) c2)
    ;; and that this is larger than the number of items in the list
    (test #t < (length l) c2)
    ;; and that every item was touched
    (test null remove* touched l))
  (let ([c 0] [touched '()])
    ;; now cache the keys
    (test sorted
          sort l <
               #:key (lambda (x)
                       (set! c (add1 c))
                       (set! touched (cons x touched))
                       (car x))
               #:cache-keys? #t)
    ;; test that the number of key uses is the same as the list length
    (test #t = c (length l))
    ;; and that every item was touched
    (test null remove* touched l))
  (let* ([c 0] [getkey (lambda (x) (set! c (add1 c)) x)])
    ;; either way, we never use the key proc on no arguments
    (test '() sort '() < #:key getkey #:cache-keys? #f)
    (test '() sort '() < #:key getkey #:cache-keys? #t)
    (test #t = c 0)
    ;; we also don't use it for 1-arg lists
    (test '(1) sort '(1) < #:key getkey #:cache-keys? #f)
    (test #t = c 0)
    ;; but we do use it once if caching happens (it's a consistent interface)
    (test '(1) sort '(1) < #:key getkey #:cache-keys? #t)
    (test #t = c 1)
    ;; check a few other short lists
    (test '(1 2) sort '(2 1) < #:key getkey #:cache-keys? #t)
    (test '(1 2 3) sort '(2 3 1) < #:key getkey #:cache-keys? #t)
    (test '(1 2 3 4) sort '(4 2 3 1) < #:key getkey #:cache-keys? #t)
    (test #t = c 10)))

;; ---------- make-list ----------
(let ()
  (test '()    make-list 0 'x)
  (test '(x)   make-list 1 'x)
  (test '(x x) make-list 2 'x)
  (err/rt-test (make-list -3 'x)))

;; ---------- take/drop[-right] ----------
(let ()
  (define-syntax-rule (vals-list expr)
    (call-with-values (lambda () expr) list))
  (define (split-at*       l n) (vals-list (split-at       l n)))
  (define (split-at-right* l n) (vals-list (split-at-right l n)))
  (define funs (list take drop take-right drop-right
                     split-at* split-at-right*))
  (define tests
    ;; -----args------ --take--- --drop--- --take-r--- --drop-r-
    '([((a b c d) 2)   (a b)     (c d)     (c d)       (a b)    ]
      [((a b c d) 0)   ()        (a b c d) ()          (a b c d)]
      [((a b c d) 4)   (a b c d) ()        (a b c d)   ()       ]
      [((a b c . d) 1) (a)       (b c . d) (c . d)     (a b)    ]
      [((a b c . d) 3) (a b c)   d         (a b c . d) ()       ]
      [(99 0)          ()        99        99          ()       ]))
  (for ([t tests]
        #:when #t
        [expect `(,@(cdr t)
                  ,(list (list-ref t 1) (list-ref t 2))
                  ,(list (list-ref t 4) (list-ref t 3)))]
        [fun funs])
    (apply test expect fun (car t)))
  (for ([fun funs])
    (arity-test fun 2 2)
    (err/rt-test (fun 1 1) exn:application:mismatch?)
    (err/rt-test (fun '(1 2 3) 2.0))
    (err/rt-test (fun '(1) '(1)))
    (err/rt-test (fun '(1) -1))
    (err/rt-test (fun '(1) 2) exn:application:mismatch?)
    (err/rt-test (fun '(1 2 . 3) 3) exn:application:mismatch?)))

;; ---------- append* ----------
(let ()
  (test '()        append* '())
  (test '()        append* '(()))
  (test '()        append* '(() ()))
  (test '(0 1 2 3) append* '((0 1 2 3)))
  (test '(0 1 2 3) append* '(0 1 2 3) '())
  (test '(0 1 2 3) append* '(0 1 2 3) '(()))
  (test '(0 1 2 3) append* '(0 1 2 3) '(() ()))
  (test '(0 1 2 3) append* '(0 1) '((2) (3)))
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

;; ---------- add-between ----------
(let ()
  (test '()          add-between '() 1)
  (test '(9)         add-between '(9) 1)
  (test '(9 1 8 1 7) add-between '(9 8 7) 1)
  (test '(9 (1) 8)   add-between '(9 8) '(1)))

;; ---------- remove-duplicates ----------
(let ()
  (define rd remove-duplicates)
  ;; basic 'naive tests
  (test '() rd '())
  (test '(a) rd '(a a a a))
  (test '(a b) rd '(a b))
  (test '(a b) rd '(a b a b a b))
  (test '(a b) rd '(a a a b b b))
  (test '(a b) rd '(a b b a)) ; keeps first occurrences
  (test '("a" "b") rd '("a" "A" "b" "B" "a") #:key string-downcase)
  (let ([long (for/list ([i (in-range 300)]) i)])
    (test long rd long)
    (test long rd (append long long))
    (test long rd (append long (reverse long))) ; keeps first
    (test long rd (append* (map (lambda (x) (list x x)) long)))
    (test long rd (append long (map (lambda (x) (- x)) long)) #:key abs)
    (test long rd (append long (map (lambda (x) (- x)) long)) = #:key abs)))

;; ---------- filter and filter-not ----------
(let ()
  (define f filter)
  (define fn filter-not)

  (test '()              f  number? '())
  (test '()              fn number? '())
  (test '(1 2 3)         f  number? '(1 a 2 b 3 c d))
  (test '(a b c d)       fn number? '(1 a 2 b 3 c d))
  (test '()              f  string? '(1 a 2 b 3 c d))
  (test '(1 a 2 b 3 c d) fn string? '(1 a 2 b 3 c d))
  (err/rt-test (f string? '(1 2 3 . 4)) exn:application:mismatch?)
  (err/rt-test (fn string? '(1 2 3 . 4)) exn:application:mismatch?)
  (err/rt-test (f  2 '(1 2 3)))
  (err/rt-test (fn 2 '(1 2 3)))
  (err/rt-test (f cons '(1 2 3)))
  (err/rt-test (fn cons '(1 2 3)))
  (arity-test f  2 2)
  (arity-test fn 2 2))

;; ---------- partition ----------
(let ()
  (define (p pred l) (call-with-values (lambda () (partition pred l)) list))
  (test '(() ()) p (lambda (_) #t) '())
  (test '(() ()) p (lambda (_) #f) '())
  (test '((1 2 3 4) ()) p (lambda (_) #t) '(1 2 3 4))
  (test '(() (1 2 3 4)) p (lambda (_) #f) '(1 2 3 4))
  (test '((2 4) (1 3)) p even? '(1 2 3 4))
  (test '((1 3) (2 4)) p odd? '(1 2 3 4)))

;; ---------- filter-map ----------
(let ()
  (define fm filter-map)
  (test '() fm values '())
  (test '(1 2 3) fm values '(1 2 3))
  (test '() fm values '(#f #f #f))
  (test '(1 2 3) fm values '(#f 1 #f 2 #f 3 #f))
  (test '(4 8 12) fm (lambda (x) (and (even? x) (* x 2))) '(1 2 3 4 5 6)))

;; ---------- count ----------

(let ()
  (test 0 count even? '())
  (test 4 count even? '(0 2 4 6))
  (test 0 count even? '(1 3 5 7))
  (test 2 count even? '(1 2 3 4))
  (test 2 count < '(1 2 3 4) '(4 3 2 1)))

;; ---------- append-map ----------
(let ()
  (define am append-map)
  (test '() am list '())
  (test '(1 2 3) am list '(1 2 3))
  (test '(1 1 2 2 3 3) am (lambda (x) (list x x)) '(1 2 3)))

;; ---------- argmin & argmax ----------

(let ()

  (define ((check-regs . regexps) exn)
    (and (exn:fail? exn)
         (andmap (λ (reg) (regexp-match reg (exn-message exn)))
                 regexps)))

  (test 'argmin object-name argmin)
  (test 1 argmin (lambda (x) 0) (list 1))
  (test 1 argmin (lambda (x) x) (list 1 2 3))
  (test 1 argmin (lambda (x) 1) (list 1 2 3))

  (test 3
        'argmin-makes-right-number-of-calls
        (let ([c 0])
          (argmin (lambda (x) (set! c (+ c 1)) 0)
                  (list 1 2 3))
          c))

  (test '(1 banana) argmin car '((3 pears) (1 banana) (2 apples)))

  (err/rt-test (argmin 1 (list 1)) (check-regs #rx"argmin" #rx"procedure"))
  (err/rt-test (argmin (lambda (x) x) 3) (check-regs #rx"argmin" #rx"list"))
  (err/rt-test (argmin (lambda (x) x) (list 1 #f)) (check-regs #rx"argmin" #rx"procedure that returns real numbers"))
  (err/rt-test (argmin (lambda (x) x) (list #f)) (check-regs #rx"argmin" #rx"procedure that returns real numbers"))

  (err/rt-test (argmin (lambda (x) x) (list +i)) (check-regs #rx"argmin" #rx"procedure that returns real numbers"))
  (err/rt-test (argmin (lambda (x) x) (list)) (check-regs #rx"argmin" #rx"non-empty list"))

  (test 'argmax object-name argmax)
  (test 1 argmax (lambda (x) 0) (list 1))
  (test 3 argmax (lambda (x) x) (list 1 2 3))
  (test 1 argmax (lambda (x) 1) (list 1 2 3))

  (test 3
        'argmax-makes-right-number-of-calls
        (let ([c 0])
          (argmax (lambda (x) (set! c (+ c 1)) 0)
                  (list 1 2 3))
          c))

  (test '(3 pears) argmax car '((3 pears) (1 banana) (2 apples)))

  (err/rt-test (argmax 1 (list 1)) (check-regs #rx"argmax" #rx"procedure"))
  (err/rt-test (argmax (lambda (x) x) 3) (check-regs #rx"argmax" #rx"list"))
  (err/rt-test (argmax (lambda (x) x) (list 1 #f)) (check-regs #rx"argmax" #rx"procedure that returns real numbers"))
  (err/rt-test (argmax (lambda (x) x) (list #f)) (check-regs #rx"argmax" #rx"procedure that returns real numbers"))

  (err/rt-test (argmax (lambda (x) x) (list +i)) (check-regs #rx"argmax" #rx"procedure that returns real numbers"))
  (err/rt-test (argmax (lambda (x) x) (list)) (check-regs #rx"argmax" #rx"non-empty list")))



;; ---------- check no collisions with srfi/1 ----------
(test (void)
      eval '(module foo scheme/base (require scheme/base srfi/1/list))
           (make-base-namespace))

(report-errs)
