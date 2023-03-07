#lang racket/base

(require racket/fixnum
         racket/hash-code
         racket/set
         rackunit)

(test-case "equal-hash-code reshuffle"
  ;; [A, B] != [B, A]
  (check-not-equal? (equal-hash-code (hash 0 1))
                    (equal-hash-code (hash 1 0)))
  (check-not-equal? (equal-hash-code (make-hash '((0 . 1))))
                    (equal-hash-code (make-hash '((1 . 0)))))
  ;; {[A, B], C} != {[A, C], B}
  ;; {{A: B}, {C: D}} != {{A: D}, {C: B}}
  (check-not-equal? (equal-hash-code (hash (hash 0 2) #true (hash 1 3) #true))
                    (equal-hash-code (hash (hash 0 3) #true (hash 1 2) #true)))
  (check-not-equal? (equal-hash-code (set (hash 0 2) (hash 1 3)))
                    (equal-hash-code (set (hash 0 3) (hash 1 2))))
  (check-not-equal? (equal-hash-code (make-hash
                                      (list (cons (make-hash '((0 . 2))) #true)
                                            (cons (make-hash '((1 . 3))) #true))))
                    (equal-hash-code (make-hash
                                      (list (cons (make-hash '((0 . 3))) #true)
                                            (cons (make-hash '((1 . 2))) #true)))))
  (check-not-equal? (equal-hash-code (mutable-set (make-hash '((0 . 2)))
                                                  (make-hash '((1 . 3)))))
                    (equal-hash-code (mutable-set (make-hash '((0 . 3)))
                                                  (make-hash '((1 . 2))))))
  ;; [A, [B]] != [B, [A]]
  (check-not-equal? (equal-hash-code (list 1 (list 2)))
                    (equal-hash-code (list 2 (list 1)))))

(test-case "hash-hash-code"
  ;; {k1: v1, k2: A, k3: B} != {k1: v1, k2: C, k3: D}
  (check-not-equal? (equal-hash-code (hash 1 4 2 0 3 0))
                    (equal-hash-code (hash 1 4 2 5 3 6)))
  (check-not-equal? (equal-hash-code (hash 1 4 2 5 3 6))
                    (equal-hash-code (hash 1 4 2 6 3 5)))

  (check-not-equal? (equal-hash-code (hash 1 4 2 5 3 6))
                    (equal-hash-code (hashalw 1 4 2 5 3 6)))
  (check-not-equal? (equal-hash-code (hashalw 1 4 2 5 3 6))
                    (equal-hash-code (hasheqv 1 4 2 5 3 6)))
  (check-not-equal? (equal-hash-code (hasheqv 1 4 2 5 3 6))
                    (equal-hash-code (hasheq 1 4 2 5 3 6)))
  (check-not-equal? (equal-hash-code (hash 1 4 2 5 3 6))
                    (equal-hash-code (make-hash '((1 . 4) (2 . 5)))))
  (check-not-equal? (equal-hash-code (make-hash '((1 . 4) (2 . 5))))
                    (equal-hash-code (make-weak-hash '((1 . 4) (2 . 5)))))
  (check-not-equal? (equal-hash-code (make-weak-hash '((1 . 4) (2 . 5))))
                    (equal-hash-code (make-ephemeron-hash '((1 . 4) (2 . 5))))))

(test-case "unary hash-code-combine, aka 'mix-hash-code'"
  (check-equal? (hash-code-combine (equal-hash-code 'A))
                (hash-code-combine (equal-hash-code 'A)))
  (check-not-equal? (hash-code-combine (equal-hash-code 'A))
                    (equal-hash-code 'A))
  (check-not-equal? (+ (hash-code-combine (equal-hash-code 'A))
                       (equal-hash-code 'B))
                    (+ (hash-code-combine (equal-hash-code 'B))
                       (equal-hash-code 'A)))

  (check-equal? (hash-code-combine 0) 1040)
  (check-equal? (hash-code-combine 1) 2081)
  (check-equal? (hash-code-combine 2) 3122)
  (check-equal? (hash-code-combine 3) 4163)
  (check-equal? (hash-code-combine 4) 5204)
  (check-equal? (hash-code-combine 5) 6245)

  (check-equal? (hash-code-combine 10) 11450)
  (check-equal? (hash-code-combine 100) 102965)
  (check-equal? (hash-code-combine 1000) 1022327)
  (check-equal? (hash-code-combine 10000) 10361532)

  (check-equal? (hash-code-combine 100000) 100956810)
  (check-equal? (hash-code-combine 200000) 201912581)
  (check-equal? (hash-code-combine 300000) 303908799)
  (check-equal? (hash-code-combine 400000) 403824378)
  (check-equal? (hash-code-combine 500000) 519443892)
  (check-equal? (hash-code-combine 520000) 532406413)
  (check-equal? (hash-code-combine 523000) 529273219)
  (check-equal? (hash-code-combine 523700) 528558834)
  (check-equal? (hash-code-combine 523770) 528488029)
  (check-equal? (hash-code-combine 523775) 528482824)

  (when (= (most-positive-fixnum) 1152921504606846975)
    (check-equal? (hash-code-combine 562949953421312)
                  586039697604609040)
    (check-equal? (hash-code-combine 1152921504606846975)
                  1134907106097364992)
    (check-equal? (hash-code-combine -1) -36028797018963968)
    (check-equal? (hash-code-combine -2) -36028797018962927)
    (check-equal? (hash-code-combine -3) -36028797018961886)
    (check-equal? (hash-code-combine -536870911) -36028238144731103)))

(test-case "avoid 10runs- mix collisions"
  (define 10runs
    (hash-code-combine-unordered
     #b111111111100000000001111111111000000000011111111101111111111))
  (define (10runs- n) (fx-/wraparound 10runs n))
  (check-not-equal? (hash-code-combine (10runs- 0)) (hash-code-combine 0))
  (for ([a (in-list (list 1 2 6 9 105 150 27030 38505))])
    (define b (- a))
    (check-not-equal? (hash-code-combine (10runs- a)) (hash-code-combine a))
    (check-not-equal? (hash-code-combine (10runs- b)) (hash-code-combine b))))

(test-case "one-mix-combine"
  ;; equivalent to the old `hash-code-combine` function from
  ;; racket/src/cs/rumble/hash-code.ss
  ;; before it was fixed to avoid a reshuffle-collision
  (define (one-mix-combine hc v)
    (define (mix-hash-code hc) (hash-code-combine hc))
    (hash-code-combine-unordered (mix-hash-code hc) v))
  (check-equal? (one-mix-combine 0 0) 1040)
  (check-equal? (one-mix-combine 0 -1) 1039)
  (check-equal? (one-mix-combine 0 1) 1041)
  (check-equal? (one-mix-combine 1 0) 2081)
  (check-equal? (one-mix-combine 1 -1) 2080)
  (check-equal? (one-mix-combine 1 1) 2082)
  (check-equal? (one-mix-combine 0 -2) 1038)
  (check-equal? (one-mix-combine 1 -2) 2079)
  (check-equal? (one-mix-combine 0 2) 1042)
  (check-equal? (one-mix-combine 1 2) 2083)
  (check-equal? (one-mix-combine 2 0) 3122)
  (check-equal? (one-mix-combine 2 -1) 3121)
  (check-equal? (one-mix-combine 2 1) 3123)
  (check-equal? (one-mix-combine 2 -2) 3120)
  (check-equal? (one-mix-combine 2 2) 3124)
  (check-equal? (one-mix-combine 0 -3) 1037)
  (check-equal? (one-mix-combine 1 -3) 2078)
  (check-equal? (one-mix-combine 2 -3) 3119)
  (check-equal? (one-mix-combine 0 3) 1043)
  (check-equal? (one-mix-combine 1 3) 2084)
  (check-equal? (one-mix-combine 2 3) 3125)
  (check-equal? (one-mix-combine 3 0) 4163)
  (check-equal? (one-mix-combine 3 -1) 4162)
  (check-equal? (one-mix-combine 3 1) 4164)
  (check-equal? (one-mix-combine 3 -2) 4161)
  (check-equal? (one-mix-combine 3 2) 4165)
  (check-equal? (one-mix-combine 3 -3) 4160)
  (check-equal? (one-mix-combine 3 3) 4166)

  (when (= (most-positive-fixnum) 1152921504606846975)
    (check-equal? (one-mix-combine -1 0) -36028797018963968)
    (check-equal? (one-mix-combine -1 -1) -36028797018963969)
    (check-equal? (one-mix-combine -1 1) -36028797018963967)
    (check-equal? (one-mix-combine -1 -2) -36028797018963970)
    (check-equal? (one-mix-combine -2 0) -36028797018962927)
    (check-equal? (one-mix-combine -2 -1) -36028797018962928)
    (check-equal? (one-mix-combine -2 1) -36028797018962926)
    (check-equal? (one-mix-combine -2 -2) -36028797018962929)
    (check-equal? (one-mix-combine -1 2) -36028797018963966)
    (check-equal? (one-mix-combine -2 2) -36028797018962925)
    (check-equal? (one-mix-combine -1 -3) -36028797018963971)
    (check-equal? (one-mix-combine -2 -3) -36028797018962930)
    (check-equal? (one-mix-combine -3 0) -36028797018961886)
    (check-equal? (one-mix-combine -3 -1) -36028797018961887)
    (check-equal? (one-mix-combine -3 1) -36028797018961885)
    (check-equal? (one-mix-combine -3 -2) -36028797018961888)
    (check-equal? (one-mix-combine -3 2) -36028797018961884)
    (check-equal? (one-mix-combine -3 -3) -36028797018961889)
    (check-equal? (one-mix-combine -1 3) -36028797018963965)
    (check-equal? (one-mix-combine -2 3) -36028797018962924)
    (check-equal? (one-mix-combine -3 3) -36028797018961883)))

(test-case "hash-code-combine"
  (check-not-equal? (hash-code-combine 1) 1)
  (check-not-equal? (hash-code-combine 1 2) (hash-code-combine 2 1))
  ;; test that `hash-code-combine` doesn't have the "one-mix-combine" problem
  (check-not-equal? (hash-code-combine-unordered (hash-code-combine 1 2) 3)
                    (hash-code-combine-unordered (hash-code-combine 1 3) 2))
  (check-not-equal? (hash-code-combine-unordered 4 (hash-code-combine 5 6))
                    (hash-code-combine-unordered 6 (hash-code-combine 5 4)))
  (check-not-equal? (hash-code-combine-unordered 7 (hash-code-combine 8 9))
                    (hash-code-combine-unordered 8 (hash-code-combine 7 9)))
  (check-not-equal? (hash-code-combine-unordered (hash-code-combine 11 12) 10)
                    (hash-code-combine-unordered (hash-code-combine 10 12) 11)))

(test-case "hash-code-combine-unordered"
  (check-equal? (hash-code-combine-unordered 1 2) 3)
  (check-equal? (hash-code-combine-unordered 2 1) 3)
  (check-equal? (hash-code-combine-unordered 100 -16) 84)
  (check-equal? (hash-code-combine-unordered -16 100) 84)
  (check-equal? (hash-code-combine-unordered -1000 -24) -1024)
  (check-equal? (hash-code-combine-unordered -24 -1000) -1024)
  (check-equal? (hash-code-combine-unordered 503070 20705) 523775)
  (check-equal? (hash-code-combine-unordered 20705 503070) 523775))

(test-case "ordered-triple"
  (struct ordered-triple (fst snd thd)
    #:methods gen:equal+hash
    [(define (equal-proc self other rec)
       (and (rec (ordered-triple-fst self) (ordered-triple-fst other))
            (rec (ordered-triple-snd self) (ordered-triple-snd other))
            (rec (ordered-triple-thd self) (ordered-triple-thd other))))
     (define (hash-proc self rec)
       (hash-code-combine (rec (ordered-triple-fst self))
                          (rec (ordered-triple-snd self))
                          (rec (ordered-triple-thd self))))
     (define (hash2-proc self rec)
       (hash-code-combine (rec (ordered-triple-fst self))
                          (rec (ordered-triple-snd self))
                          (rec (ordered-triple-thd self))))])
  (check-equal? (ordered-triple 'A 'B 'C) (ordered-triple 'A 'B 'C))
  (check-equal? (equal-hash-code (ordered-triple 'A 'B 'C))
                (equal-hash-code (ordered-triple 'A 'B 'C)))
  (check-not-equal? (ordered-triple 'A 'B 'C) (ordered-triple 'C 'B 'A))
  (check-not-equal? (equal-hash-code (ordered-triple 'A 'B 'C))
                    (equal-hash-code (ordered-triple 'C 'B 'A)))
  (check-not-equal? (ordered-triple 'A 'B 'C) (ordered-triple 'C 'A 'B))
  (check-not-equal? (equal-hash-code (ordered-triple 'A 'B 'C))
                    (equal-hash-code (ordered-triple 'C 'A 'B))))

(test-case "wrap"
  (struct wrap (value)
    #:methods gen:equal+hash
    [(define (equal-proc self other rec)
       (rec (wrap-value self) (wrap-value other)))
     (define (hash-proc self rec)
       (hash-code-combine (rec (wrap-value self))))
     (define (hash2-proc self rec)
       (hash-code-combine (rec (wrap-value self))))])
  (check-equal? (wrap 'A) (wrap 'A))
  (check-equal? (equal-hash-code (wrap 'A))
                (equal-hash-code (wrap 'A)))
  (check-not-equal? (wrap 'A) 'A)
  (check-not-equal? (equal-hash-code (wrap 'A))
                    (equal-hash-code 'A)))

(test-case "flip-triple"
  (struct flip-triple (left mid right)
    #:methods gen:equal+hash
    [(define (equal-proc self other rec)
       (and (rec (flip-triple-mid self) (flip-triple-mid other))
            (or
             (and (rec (flip-triple-left self) (flip-triple-left other))
                  (rec (flip-triple-right self) (flip-triple-right other)))
             (and (rec (flip-triple-left self) (flip-triple-right other))
                  (rec (flip-triple-right self) (flip-triple-left other))))))
     (define (hash-proc self rec)
       (hash-code-combine (rec (flip-triple-mid self))
                          (hash-code-combine-unordered
                           (rec (flip-triple-left self))
                           (rec (flip-triple-right self)))))
     (define (hash2-proc self rec)
       (hash-code-combine (rec (flip-triple-mid self))
                          (hash-code-combine-unordered
                           (rec (flip-triple-left self))
                           (rec (flip-triple-right self)))))])
  (check-equal? (flip-triple 'A 'B 'C) (flip-triple 'A 'B 'C))
  (check-equal? (equal-hash-code (flip-triple 'A 'B 'C))
                (equal-hash-code (flip-triple 'A 'B 'C)))
  (check-equal? (flip-triple 'A 'B 'C) (flip-triple 'C 'B 'A))
  (check-equal? (equal-hash-code (flip-triple 'A 'B 'C))
                (equal-hash-code (flip-triple 'C 'B 'A)))
  (check-not-equal? (flip-triple 'A 'B 'C) (flip-triple 'C 'A 'B))
  (check-not-equal? (equal-hash-code (flip-triple 'A 'B 'C))
                    (equal-hash-code (flip-triple 'C 'A 'B))))

(test-case "rotate-triple"
  (struct rotate-triple (rock paper scissors)
    #:methods gen:equal+hash
    [(define (equal-proc self other rec)
       (or
        (and (rec (rotate-triple-rock self) (rotate-triple-rock other))
             (rec (rotate-triple-paper self) (rotate-triple-paper other))
             (rec (rotate-triple-scissors self) (rotate-triple-scissors other)))
        (and (rec (rotate-triple-rock self) (rotate-triple-paper other))
             (rec (rotate-triple-paper self) (rotate-triple-scissors other))
             (rec (rotate-triple-scissors self) (rotate-triple-rock other)))
        (and (rec (rotate-triple-rock self) (rotate-triple-scissors other))
             (rec (rotate-triple-paper self) (rotate-triple-rock other))
             (rec (rotate-triple-scissors self) (rotate-triple-paper other)))))
     (define (hash-proc self rec)
       (define r (rec (rotate-triple-rock self)))
       (define p (rec (rotate-triple-paper self)))
       (define s (rec (rotate-triple-scissors self)))
       (hash-code-combine
        (eq-hash-code struct:rotate-triple)
        (hash-code-combine-unordered
         (hash-code-combine r p)
         (hash-code-combine p s)
         (hash-code-combine s r))))
     (define (hash2-proc self rec)
       (define r (rec (rotate-triple-rock self)))
       (define p (rec (rotate-triple-paper self)))
       (define s (rec (rotate-triple-scissors self)))
       (hash-code-combine
        (eq-hash-code struct:rotate-triple)
        (hash-code-combine-unordered
         (hash-code-combine r p)
         (hash-code-combine p s)
         (hash-code-combine s r))))])
  (check-equal? (rotate-triple 'A 'B 'C) (rotate-triple 'A 'B 'C))
  (check-equal? (equal-hash-code (rotate-triple 'A 'B 'C))
                (equal-hash-code (rotate-triple 'A 'B 'C)))
  (check-not-equal? (rotate-triple 'A 'B 'C) (rotate-triple 'C 'B 'A))
  (check-not-equal? (equal-hash-code (rotate-triple 'A 'B 'C))
                    (equal-hash-code (rotate-triple 'C 'B 'A)))
  (check-equal? (rotate-triple 'A 'B 'C) (rotate-triple 'C 'A 'B))
  (check-equal? (equal-hash-code (rotate-triple 'A 'B 'C))
                (equal-hash-code (rotate-triple 'C 'A 'B)))

  (struct rotate-triple2 (rock paper scissors)
    #:methods gen:equal+hash
    [(define (equal-proc self other rec)
       (or
        (and (rec (rotate-triple2-rock self) (rotate-triple2-rock other))
             (rec (rotate-triple2-paper self) (rotate-triple2-paper other))
             (rec (rotate-triple2-scissors self) (rotate-triple2-scissors other)))
        (and (rec (rotate-triple2-rock self) (rotate-triple2-paper other))
             (rec (rotate-triple2-paper self) (rotate-triple2-scissors other))
             (rec (rotate-triple2-scissors self) (rotate-triple2-rock other)))
        (and (rec (rotate-triple2-rock self) (rotate-triple2-scissors other))
             (rec (rotate-triple2-paper self) (rotate-triple2-rock other))
             (rec (rotate-triple2-scissors self) (rotate-triple2-paper other)))))
     (define (hash-proc self rec)
       (define r (rec (rotate-triple2-rock self)))
       (define p (rec (rotate-triple2-paper self)))
       (define s (rec (rotate-triple2-scissors self)))
       (hash-code-combine
        (eq-hash-code struct:rotate-triple2)
        (hash-code-combine-unordered
         (hash-code-combine r p)
         (hash-code-combine p s)
         (hash-code-combine s r))))
     (define (hash2-proc self rec)
       (define r (rec (rotate-triple2-rock self)))
       (define p (rec (rotate-triple2-paper self)))
       (define s (rec (rotate-triple2-scissors self)))
       (hash-code-combine
        (eq-hash-code struct:rotate-triple2)
        (hash-code-combine-unordered
         (hash-code-combine r p)
         (hash-code-combine p s)
         (hash-code-combine s r))))])

  (check-equal? (set (rotate-triple 'A 'B 'C) (rotate-triple2 'D 'E 'F))
                (set (rotate-triple 'A 'B 'C) (rotate-triple2 'D 'E 'F)))
  (check-equal? (equal-hash-code
                 (set (rotate-triple 'A 'B 'C) (rotate-triple2 'D 'E 'F)))
                (equal-hash-code
                 (set (rotate-triple 'A 'B 'C) (rotate-triple2 'D 'E 'F))))
  (check-not-equal? (set (rotate-triple 'A 'B 'C) (rotate-triple2 'D 'E 'F))
                    (set (rotate-triple2 'A 'B 'C) (rotate-triple 'D 'E 'F)))
  (check-not-equal? (equal-hash-code
                     (set (rotate-triple 'A 'B 'C) (rotate-triple2 'D 'E 'F)))
                    (equal-hash-code
                     (set (rotate-triple2 'A 'B 'C) (rotate-triple 'D 'E 'F)))))

(test-case "wrap-list reshuffle"
  (struct wrap-list (list)
    #:methods gen:equal+hash
    [(define (equal-proc self other rec)
       (rec (wrap-list-list self) (wrap-list-list other)))
     (define (hash-proc self rec)
       (apply hash-code-combine (map rec (wrap-list-list self))))
     (define (hash2-proc self rec)
       (apply hash-code-combine (map rec (wrap-list-list self))))])

  (check-not-equal? (equal-hash-code (wrap-list (list 1 (wrap-list (list 2)))))
                    (equal-hash-code (wrap-list (list 2 (wrap-list (list 1)))))))

(test-case "hash-code-combine arity 0-2 vs list"
  (define l (list 0 -1 1 -2 2 -3 3 -4 4 -5 5
                  10 100 1000 10000
                  100000 200000 300000 400000
                  500000 520000 523000 523700 523770 523775
                  562949953421312 1152921504606846975 -536870911))

  (check-equal? (hash-code-combine) (hash-code-combine* '()))
  (check-equal? (hash-code-combine-unordered) (hash-code-combine-unordered* '()))

  (for ([a (in-list l)])
    (with-check-info (['a a])
      (check-equal? (hash-code-combine a) (hash-code-combine* (list a)))
      (check-equal? (hash-code-combine* (list a)) (hash-code-combine* a '()))
      (check-not-equal? (hash-code-combine a) a)

      (check-equal? (hash-code-combine-unordered a)
                    (hash-code-combine-unordered* (list a)))
      (check-equal? (hash-code-combine-unordered* (list a))
                    (hash-code-combine-unordered* a '()))
      (when (fixnum? a)
        (check-equal? (hash-code-combine-unordered a) a))))

  (for* ([a (in-list l)]
         [b (in-list l)])
    (with-check-info (['a a]
                      ['b b])
      (check-equal? (hash-code-combine a b) (hash-code-combine* (list a b)))
      (check-equal? (hash-code-combine* (list a b)) (hash-code-combine* a b '()))
      (check-equal? (hash-code-combine* a (list b)) (hash-code-combine* a b '()))
      (unless (= a b)
        (check-not-equal? (hash-code-combine a b) (hash-code-combine b a)))

      (check-equal? (hash-code-combine-unordered a b)
                    (hash-code-combine-unordered* (list a b)))
      (check-equal? (hash-code-combine-unordered* (list a b))
                    (hash-code-combine-unordered* a b '()))
      (check-equal? (hash-code-combine-unordered* a (list b))
                    (hash-code-combine-unordered* a b '()))
      (check-equal? (hash-code-combine-unordered a b)
                    (hash-code-combine-unordered b a)))))

(test-case "error messages"
  (check-exn #rx"hash-code-combine:.*expected: exact-integer[?].*given: 0.0"
             (λ () (hash-code-combine 0.0)))
  (check-exn #rx"hash-code-combine:.*expected: exact-integer[?].*given: [+]nan.0"
             (λ () (hash-code-combine +nan.0 1)))
  (check-exn #rx"hash-code-combine:.*expected: exact-integer[?].*given: [+]inf.0"
             (λ () (hash-code-combine 2 +inf.0)))
  (check-exn #rx"hash-code-combine:.*expected: exact-integer[?].*given: [+]inf.0"
             (λ () (hash-code-combine 1 2 3 4 +inf.0 6)))
  (check-exn
   #rx"hash-code-combine-unordered:.*expected: exact-integer[?].*given: #f"
   (λ () (hash-code-combine-unordered #f)))
  (check-exn
   #rx"hash-code-combine-unordered:.*expected: exact-integer[?].*given: '#s[(]E[)]"
   (λ () (hash-code-combine-unordered '#s(E) 4)))
  (check-exn
   #rx"hash-code-combine-unordered:.*expected: exact-integer[?].*given: \"F\""
   (λ () (hash-code-combine-unordered 5 "F")))
  (check-exn
   #rx"hash-code-combine-unordered:.*expected: exact-integer[?].*given: \"F\""
   (λ () (hash-code-combine-unordered 1 2 3 4 "F" 5)))

  (check-exn #rx"hash-code-combine[*]:.*expected: .listof exact-integer[?].*given: '.#&6."
             (λ () (hash-code-combine* '(#&6))))
  (check-exn #rx"hash-code-combine[*]:.*expected: exact-integer[?].*given: '#()"
             (λ () (hash-code-combine* '#() 7 '())))
  (check-exn #rx"hash-code-combine[*]:.*expected: exact-integer[?].*given: '()"
             (λ () (hash-code-combine* '() '(I))))
  (check-exn #rx"hash-code-combine[*]:.*expected: .listof exact-integer[?]..*given: 'I"
             (λ () (hash-code-combine* 1 2 3 'I)))
  (check-exn
   #rx"hash-code-combine-unordered[*]:.*expected: .listof exact-integer[?].*given: '.-7 #[\\]J."
   (λ () (hash-code-combine-unordered* '(-7 #\J))))
  (check-exn
   #rx"hash-code-combine-unordered[*]:.*expected: exact-integer[?].*given: #t"
   (λ () (hash-code-combine-unordered* #t 10 '())))
  (check-exn
   #rx"hash-code-combine-unordered[*]:.*expected: exact-integer[?].*given: #t"
   (λ () (hash-code-combine-unordered* 10 #t '())))
  (check-exn
   #rx"hash-code-combine-unordered[*]:.*expected: .listof exact-integer[?].*given: '.3 L."
   (λ () (hash-code-combine-unordered* 11 '(3 L))))
  (check-exn
   #rx"hash-code-combine-unordered[*]:.*expected: .listof exact-integer[?].*given: 'L"
   (λ () (hash-code-combine-unordered* 1 2 3 'L))))

(test-case "recur"
  (define (rational-hash x)
    (cond
      [(rational? x) (equal-hash-code (inexact->exact x))]
      [else (equal-hash-code/recur x rational-hash)]))

  (check-equal? (rational-hash 0.0) (rational-hash -0.0))
  (check-not-equal? (rational-hash 1.0) (rational-hash -1.0))
  (check-equal? (rational-hash (list (list (list 4.0 0.0) 9.0) 6.0))
                (rational-hash (list (list (list 4 0) 9) 6)))

  (define (rationalw-hash x)
    (cond
      [(rational? x) (equal-always-hash-code (inexact->exact x))]
      [else (equal-always-hash-code/recur x rationalw-hash)]))

  (check-equal? (rationalw-hash 0.0) (rationalw-hash -0.0))
  (check-not-equal? (rationalw-hash 1.0) (rationalw-hash -1.0))
  (check-equal? (rationalw-hash (list (list (list 4.0 0.0) 9.0) 6.0))
                (rationalw-hash (list (list (list 4 0) 9) 6)))

  (define (shallow-insides/hash hash/recur v)
    (define is (mutable-seteq))
    (hash/recur v (λ (i) (set-add! is i) 0))
    is)

  (define (shallow-insides/equal equal/recur v1 v2)
    (define i1s (mutable-seteq))
    (define i2s (mutable-seteq))
    (equal/recur v1 v2 (λ (i1 i2) (set-add! i1s i1) (set-add! i2s i2) #true))
    (values i1s i2s))

  (define (deep-insides/hash hash/recur v)
    (define is (mutable-seteq))
    (define (rec i) (set-add! is i) (hash/recur i rec))
    (hash/recur v rec)
    is)

  (define (deep-insides/equal equal/recur v1 v2)
    (define i1s (mutable-seteq))
    (define i2s (mutable-seteq))
    (define (rec i1 i2) (set-add! i1s i1) (set-add! i2s i2) (equal/recur i1 i2 rec))
    (equal/recur v1 v2 rec)
    (values i1s i2s))

  (define (test-shallow-insides f a1s a2s)
    (define b1 (apply f a1s))
    (define b2 (apply f a2s))
    (for ([equal/recur
           (in-list (list equal?/recur equal-always?/recur))]
          [hash/recur
           (in-list (list equal-hash-code/recur equal-always-hash-code/recur))])
      (with-check-info (['name 'test-shallow-insides] ['b1 b1] ['b2 b2])
        (define hi1s (shallow-insides/hash hash/recur b1))
        (define hi2s (shallow-insides/hash hash/recur b2))
        (define-values [ei1s ei2s] (shallow-insides/equal equal/recur b1 b2))
        (check-equal? hi1s ei1s)
        (check-equal? hi2s ei2s))))

  (define (test-deep-insides f a1s)
    (define b1 (apply f a1s))
    (define b2 (apply f a1s))
    (for ([equal/recur
           (in-list (list equal?/recur equal-always?/recur))]
          [hash/recur
           (in-list (list equal-hash-code/recur equal-always-hash-code/recur))])
      (with-check-info (['name 'test-deep-insides] ['f f] ['a1s a1s] ['b1 b1])
        (define hi1s (deep-insides/hash hash/recur b1))
        (define hi2s (deep-insides/hash hash/recur b2))
        (define-values [ei1s ei2s] (deep-insides/equal equal/recur b1 b2))
        (define a1set (apply seteq a1s))
        (check-equal? (set-intersect a1set hi1s) (set-intersect a1set ei1s))
        (check-equal? (set-intersect a1set hi2s) (set-intersect a1set ei2s)))))

  (define (test-insides f a1s a2s)
    (test-shallow-insides f a1s a2s)
    (test-deep-insides f a1s)
    (test-deep-insides f a2s))

  (struct ifoo (a b c) #:transparent)
  (struct mfoo (a b c) #:transparent #:mutable)

  (test-insides box '(1) '(2))
  (test-insides box-immutable '(1) '(2))
  (test-insides cons '(1 2) '(3 4))
  (test-insides mcons '(1 2) '(3 4))
  (test-insides list* '(1 2 3) '(4 5 6))
  (test-insides list '(1 2 3) '(4 5 6))
  (test-insides vector '(1 2 3) '(4 5 6))
  (test-insides vector-immutable '(1 2 3) '(4 5 6))
  (test-deep-insides ifoo '(1 2 3))
  (test-deep-insides mfoo '(1 2 3))
  (test-deep-insides (λ (a b c) (hash 1 a 2 b 3 c)) '(4 5 6))
  (test-deep-insides (λ (a b c)
                       (make-hash (list (cons 1 a) (cons 2 b) (cons 3 c))))
                     '(4 5 6)))
