
(load-relative "loadtest.ss")

(Section 'function)

(require (lib "list.ss"))
(require (lib "etc.ss"))

(test (list 1 2 3 4) foldl cons '() (list 4 3 2 1))
(test (list 1 2 3 4) foldr cons '() (list 1 2 3 4))
(test 
 (list (list 5 6) (list 3 4) (list 1 2))
 foldl (lambda (x y sofar) (cons (list x y) sofar))
 '()
 (list 1 3 5)
 (list 2 4 6))
(test 
 (list (list 1 2) (list 3 4) (list 5 6))
 foldr (lambda (x y sofar) (cons (list x y) sofar))
 '()
 (list 1 3 5)
 (list 2 4 6))

(arity-test foldl 3 -1)
(arity-test foldr 3 -1)

(test 0 (compose add1 sub1) 0)
(test 2 (compose add1 (lambda () 1)))
(test 5 (compose (lambda (a b) a) (lambda (x) (values (add1 x) x))) 4)
(test -1 (compose (lambda (a b) (+ a b)) (lambda (x y) (values (- y) x))) 2 3)
(test 'hi (compose (case-lambda [(x) 'bye][(y z) 'hi]) (lambda () (values 1 2))))
(test 'ok (compose (lambda () 'ok) (lambda () (values))))
(test 'ok (compose (lambda () 'ok) (lambda (w) (values))) 5)
(test-values '(1 2 3) (lambda () ((compose (lambda (x) (values x (add1 x) (+ x 2))) (lambda (y) y)) 1)))

(err/rt-test (compose 5))
(err/rt-test (compose add1 sub1 5))
(err/rt-test (compose add1 5 sub1))
(err/rt-test (compose 5 add1 sub1))
(err/rt-test ((compose add1 (lambda () (values 1 2)))) exn:application:arity?)
(err/rt-test ((compose add1 sub1)) exn:application:arity?)
(err/rt-test ((compose (lambda () 1) add1) 8) exn:application:arity?)

(arity-test compose 1 -1)

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

(test '("a" "b" "c" "c" "d" "e" "f")
      sort
      '("d" "f" "e" "c" "a" "c" "b")
      string<?)
(let ()
  (define (random-list n)
    (let loop ([n n] [r '()])
      (if (zero? n) r (loop (sub1 n) (cons (random 1000000) r)))))
  (define (test-sort sort len times)
    (or (zero? times)
        (and (let* ([rand (random-list len)]
                    [sorted (sort rand <)]
                    [same   (sort rand (lambda (x y) #f))])
               (and (= (length sorted) (length rand))
                    ;; sorted?
                    (andmap <=
                            (reverse! (cdr (reverse sorted)))
                            (cdr sorted))
                    ;; stable?
                    (equal? rand same)))
             (test-sort sort len (sub1 times)))))
  (test #t test-sort sort 1 10)
  (test #t test-sort sort 2 10)
  (test #t test-sort sort 10 100)
  (test #t test-sort sort 100 100)
  (test #t test-sort sort 1000 100))

(let ([s (let loop ([n 1000])
	   (if (zero? n)
	       '()
	       (cons (random 1000) (loop (sub1 n)))))])
  (test (quicksort s <) mergesort s <))

(report-errs)
