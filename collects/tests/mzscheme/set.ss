(load-relative "loadtest.ss")

(Section 'sets)
(require scheme/set)

;; ----------------------------------------

(test #t set? (make-set))
(test #t set-empty? (make-set))
(test #t set? (make-set 1 2 3))
(test #f set-empty? (make-set 1 2 3))
(test #t set? (make-seteq))
(test #t set-empty? (make-seteq))
(test #t set? (make-seteq 1 2 3))
(test #f set-empty? (make-seteq 1 2 3))
(test #t set? (make-seteqv))
(test #t set-empty? (make-seteqv))
(test #t set? (make-seteqv 1 2 3))
(test #f set-empty? (make-seteqv 1 2 3))

(test #f set-eq? (make-set 1 2 3))
(test #f set-eqv? (make-set 1 2 3))
(test #t set-eq? (make-seteq 1 2 3))
(test #f set-eqv? (make-seteq 1 2 3))
(test #f set-eq? (make-seteqv 1 2 3))
(test #t set-eqv? (make-seteqv 1 2 3))

(test 3 set-count (make-set (string #\a) "b" "c" (string #\a)))
(test 4 set-count (make-seteqv (string #\a) "b" "c" (string #\a)))
(test 4 set-count (make-seteq (string #\a) "b" "c" (string #\a)))

(test #t set-member? (make-set 1 2 3) 1)
(test #t set-member? (make-set 1 2 3) 2)
(test #t set-member? (make-set 1 2 3) 3)
(test #f set-member? (make-set 1 2 3) 4)

(let ([s (make-set 1 2 3)])
  (test #t equal? s (set-add (set-add (set-add (make-set) 1) 2) 3))
  (test #t equal? (make-seteq 1 2 3) (make-seteq 1 2 3))
  (test #t equal? (make-seteq 1 2 3) (make-seteq 3 2 1))
  (test #t equal? (make-seteqv 1 2 3) (make-seteqv 1 2 3))
  (test #f equal? s (make-seteq 1 2 3))
  (test #f equal? s (make-seteqv 1 2 3))
  (test #f equal? (make-seteq 1 2 3) (make-seteqv 1 2 3))

  (test #t set-member? (set-add s 5) 3)
  (test #t set-member? (set-add s 5) 5)
  (test #f set-member? (set-add s 5) 4)

  (test #t set-member? (set-remove s 5) 3)
  (test #f set-member? (set-remove s 3) 3)

  (test 3 set-count (set-union s))
  (test 6 set-count (set-union s (make-set 3 4 5 6)))
  (test 6 set-count (set-union (make-set 3 4 5 6) s))
  (test 8 set-count (set-union (make-set 3 4 5 6) s (make-set 1 10 100)))

  (test (make-seteq 1 2 3) set-union (make-seteq 1 2) (make-seteq 3))
  (test (make-seteqv 1 2 3) set-union (make-seteqv 1 2) (make-seteqv 3))

  (test s set-intersect s)
  (test (make-set 3) set-intersect s (make-set 5 4 3 6))
  (test (make-set 3) set-intersect (make-set 5 4 3 6) s)
  (test (make-seteq 3) set-intersect (make-seteq 5 4 3 6) (make-seteq 1 2 3))
  (test (make-seteqv 3) set-intersect (make-seteqv 5 4 3 6) (make-seteqv 1 2 3))
  (test (make-set 3 2) set-intersect s (make-set 5 2 3))
  (test (make-seteq 3 2) set-intersect (make-seteq 1 2 3) (make-seteq 5 2 3))
  (test (make-set 2) set-intersect s (make-set 5 2 3) (make-set 2 20 200))
  (test (make-seteq 2) set-intersect (make-seteq 1 2 3) (make-seteq 5 2 3) (make-seteq 2 20 200))

  (test s set-subtract s)
  (test (make-set) set-subtract s s)
  (test s set-subtract s (make-set 100))
  (test (make-set 1 3) set-subtract s (make-set 2 100))
  (test (make-seteq 100) set-subtract (make-seteq 2 100) (make-seteq 1 2 3))
  (test (make-seteq 9 100) set-subtract (make-seteq 2 100 1000 9) (make-seteq 1 2 3) (make-seteq 1000 5))

  (let ([try-mismatch (lambda (set-op)
                        (err/rt-test (set-op (make-seteqv 1 2) (make-set 3)))
                        (err/rt-test (set-op (make-seteqv 1 2) (make-seteq 3)))
                        (err/rt-test (set-op (make-set 1 2) (make-seteq 3)))
                        (err/rt-test (set-op (make-set 1 2) (make-set 4) (make-seteq 3)))
                        (err/rt-test (set-op (make-set 1 2) (make-seteq 3) (make-set 4)))
                        (err/rt-test (set-op (make-seteq 3) (make-set 1 2) (make-set 4))))])
    (try-mismatch set-union)
    (try-mismatch set-intersect)
    (try-mismatch set-subtract))

  (test #t andmap negative? (set-map s -))
  (test 3 length (set-map s +))

  (let ([v 0])
    (set-for-each s (lambda (n) (set! v (+ v n))))
    (test 6 values v))

  (test '(1 2 3) sort (for/list ([v s]) v) <)
  (test '(1 2 3) sort (for/list ([v (in-set s)]) v) <)
  (test '(1 2 3) sort (let ([seq (in-set s)]) (for/list ([v seq]) v)) <)
        

  (void))

;; ----------------------------------------

(report-errs)
