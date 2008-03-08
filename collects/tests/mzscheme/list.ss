
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
  (define (test-sort len times)
    (or (zero? times)
        (and (let* ([rand (random-list len (if (even? times) 1000000 10))]
                    [orig< (lambda (x y) (memq y (cdr (memq x rand))))]
                    [sorted (sort rand car<)]
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
  (test #t test-sort    4 200)
  (test #t test-sort    5 200)
  (test #t test-sort   10 200)
  (test #t test-sort  100 200)
  (test #t test-sort 1000 200)
  ;; test stability
  (test '((1) (2) (3 a) (3 b) (3 c)) sort '((3 a) (1) (3 b) (2) (3 c)) car<)
  ;; test short lists (+ stable)
  (test '() sort '() car<)
  (test '((1 1)) sort '((1 1)) car<)
  (test '((1 2) (1 1)) sort '((1 2) (1 1)) car<)
  (test '((1) (2)) sort '((2) (1)) car<)
  (for-each (lambda (l) (test '((0 3) (1 1) (1 2)) sort l car<))
            '(((1 1) (1 2) (0 3))
              ((1 1) (0 3) (1 2))
              ((0 3) (1 1) (1 2))))
  (for-each (lambda (l) (test '((0 2) (0 3) (1 1)) sort l car<))
            '(((1 1) (0 2) (0 3))
              ((0 2) (1 1) (0 3))
              ((0 2) (0 3) (1 1)))))

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
