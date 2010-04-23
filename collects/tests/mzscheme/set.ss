(load-relative "loadtest.ss")

(Section 'sets)
(require scheme/set)

;; ----------------------------------------

(test #t set? (set))
(test #t set-empty? (set))
(test #t set? (set 1 2 3))
(test #f set-empty? (set 1 2 3))
(test #t set? (seteq))
(test #t set-empty? (seteq))
(test #t set? (seteq 1 2 3))
(test #f set-empty? (seteq 1 2 3))
(test #t set? (seteqv))
(test #t set-empty? (seteqv))
(test #t set? (seteqv 1 2 3))
(test #f set-empty? (seteqv 1 2 3))

(test #f set-eq? (set 1 2 3))
(test #f set-eqv? (set 1 2 3))
(test #t set-eq? (seteq 1 2 3))
(test #f set-eqv? (seteq 1 2 3))
(test #f set-eq? (seteqv 1 2 3))
(test #t set-eqv? (seteqv 1 2 3))

(test 3 set-count (set (string #\a) "b" "c" (string #\a)))
(test 4 set-count (seteqv (string #\a) "b" "c" (string #\a)))
(test 4 set-count (seteq (string #\a) "b" "c" (string #\a)))

(test #t set-member? (set 1 2 3) 1)
(test #t set-member? (set 1 2 3) 2)
(test #t set-member? (set 1 2 3) 3)
(test #f set-member? (set 1 2 3) 4)

(let ([s (set 1 2 3)])
  (test #t equal? s (set-add (set-add (set-add (set) 1) 2) 3))
  (test #t equal? (seteq 1 2 3) (seteq 1 2 3))
  (test #t equal? (seteq 1 2 3) (seteq 3 2 1))
  (test #t equal? (seteqv 1 2 3) (seteqv 1 2 3))
  (test #f equal? s (seteq 1 2 3))
  (test #f equal? s (seteqv 1 2 3))
  (test #f equal? (seteq 1 2 3) (seteqv 1 2 3))

  (test #t set-member? (set-add s 5) 3)
  (test #t set-member? (set-add s 5) 5)
  (test #f set-member? (set-add s 5) 4)

  (test #t set-member? (set-remove s 5) 3)
  (test #f set-member? (set-remove s 3) 3)

  (test #t set-subset? s (set 1 3))
  (test #t set-subset? s (set 1 2 3))
  (test #f set-subset? s (set 1 4))
  (test #t set-subset? s (set))

  (test 3 set-count (set-union s))
  (test 6 set-count (set-union s (set 3 4 5 6)))
  (test 6 set-count (set-union (set 3 4 5 6) s))
  (test 8 set-count (set-union (set 3 4 5 6) s (set 1 10 100)))

  (test (seteq 1 2 3) set-union (seteq 1 2) (seteq 3))
  (test (seteqv 1 2 3) set-union (seteqv 1 2) (seteqv 3))

  (test s set-intersect s)
  (test (set 3) set-intersect s (set 5 4 3 6))
  (test (set 3) set-intersect (set 5 4 3 6) s)
  (test (seteq 3) set-intersect (seteq 5 4 3 6) (seteq 1 2 3))
  (test (seteqv 3) set-intersect (seteqv 5 4 3 6) (seteqv 1 2 3))
  (test (set 3 2) set-intersect s (set 5 2 3))
  (test (seteq 3 2) set-intersect (seteq 1 2 3) (seteq 5 2 3))
  (test (set 2) set-intersect s (set 5 2 3) (set 2 20 200))
  (test (seteq 2) set-intersect (seteq 1 2 3) (seteq 5 2 3) (seteq 2 20 200))

  (test s set-subtract s)
  (test (set) set-subtract s s)
  (test s set-subtract s (set 100))
  (test (set 1 3) set-subtract s (set 2 100))
  (test (seteq 100) set-subtract (seteq 2 100) (seteq 1 2 3))
  (test (seteq 9 100) set-subtract (seteq 2 100 1000 9) (seteq 1 2 3) (seteq 1000 5))

  (let ([try-mismatch (lambda (set-op)
                        (err/rt-test (set-op (seteqv 1 2) (set 3)))
                        (err/rt-test (set-op (seteqv 1 2) (seteq 3)))
                        (err/rt-test (set-op (set 1 2) (seteq 3)))
                        (err/rt-test (set-op (set 1 2) (set 4) (seteq 3)))
                        (err/rt-test (set-op (set 1 2) (seteq 3) (set 4)))
                        (err/rt-test (set-op (seteq 3) (set 1 2) (set 4))))])
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

(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2)]) (add1 i)))

;; ----------------------------------------

(report-errs)
