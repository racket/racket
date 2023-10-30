#lang racket/base

(provide hash-table-tests)

(require racket/match
         rackunit)

(define hash-table-tests
  (test-suite "Test for hash tables"
    (test-case "non hash"
      (check-equal? (match 1
                      [(hash* [3 x]) x]
                      [_ 'failed])
                    'failed))

    (test-case "missing key"
      (check-equal? (match (hash 1 2 5 4)
                      [(hash* [3 x]) x]
                      [_ 'failed])
                    'failed))

    (test-case "value pattern matching"
      (check-equal? (match (hash 1 2 3 4)
                      [(hash* [1 (? odd? x)]) x]
                      [_ 'failed])
                    'failed)

      (check-equal? (match (hash 1 2 3 4)
                      [(hash* [1 (? even? x)]) x]
                      [_ 'failed])
                    2))

    (test-case "key expression"
      (check-equal? (match (hash 1 2 3 4)
                      [(hash* [(+ 1 2) x]) x]
                      [_ 'failed])
                    4))

    (test-case "duplicate"
      (check-equal? (match (hash 1 2 3 4)
                      [(hash* [1 x] [1 y]) (list x y)]
                      [_ 'failed])
                    (list 2 2))

      (check-equal? (match (hash 1 2 3 4)
                      [(hash* [1 x] [1 y] #:closed) (list x y)]
                      [_ 'failed])
                    'failed)

      (check-equal? (match (hash 1 2 3 4)
                      [(hash* [1 x] [1 y] [3 z] #:closed) (list x y z)]
                      [_ 'failed])
                    (list 2 2 4)))

    (test-case "partial matching"
      (check-equal? (match (hash 1 2 3 4)
                      [(hash* [1 x]) x]
                      [_ 'failed])
                    2))

    (test-case "partial matching (multiple)"
      (check-equal? (match (hash 1 2 3 4 5 6)
                      [(hash* [1 x] [5 z]) (list x z)]
                      [_ 'failed])
                    (list 2 6)))

    (test-case "full matching"
      (check-equal? (match (hash 1 2 3 4 5 6)
                      [(hash* [1 x] [3 y] [5 z] #:closed) (list x y z)]
                      [_ 'failed])
                    (list 2 4 6)))

    (test-case "full matching failure"
      ;; extra keys
      (check-equal? (match (hash 1 2 3 4 5 6)
                      [(hash* [1 x] [5 z] #:closed) (list x z)]
                      [_ 'failed])
                    'failed)

      ;; missing keys
      (check-equal? (match (hash 1 2 3 4 5 6)
                      [(hash* [1 x] [4 y] [5 z] #:closed) (list x y z)]
                      [_ 'failed])
                    'failed))

    (test-case "rest matching"
      ;; single
      (check-equal? (match (hash 1 2 3 4 5 6)
                      [(hash* [1 x] [5 z] #:rest (? hash? h)) (list x z h)]
                      [_ 'failed])
                    (list 2 6 (hash 3 4)))

      ;; multiple
      (check-equal? (match (hash 1 2 3 4 5 6)
                      [(hash* [5 z] #:rest (? hash? h)) (list z h)]
                      [_ 'failed])
                    (list 6 (hash 1 2 3 4)))

      ;; nested
      (check-equal? (match (hash 1 2 3 4 5 6)
                      [(hash* [5 z] #:rest (hash* [1 x])) (list x z)]
                      [_ 'failed])
                    (list 2 6)))

    (test-case "rest matching failure"
      ;; rest-pat
      (check-equal? (match (hash 1 2 3 4 5 6)
                      [(hash* [1 x] [5 z] #:rest (? number? h)) (list x z h)]
                      [_ 'failed])
                    'failed)

      ;; extra keys
      (check-equal? (match (hash 1 2 3 4 5 6)
                      [(hash* [4 z] #:rest (? hash? h)) (list z h)]
                      [_ 'failed])
                    'failed))

    (test-case "evaluate only once (at least when hash* is the top-level pattern)"
      ;; partial mode
      (let ([var 0])
        (check-equal? (match (hash -5 -6 1 2 3 4)
                        [(hash* [(begin (set! var (+ var 1))
                                        var)
                                 x]
                                [(begin (set! var (+ var 2))
                                        var)
                                 y])
                         (list x y)]
                        [_ 'failed])
                      (list 2 4))
        (check-equal? var 3))

      ;; full mode
      (let ([var 0])
        (check-equal? (match (hash 1 2 3 4)
                        [(hash* [(begin (set! var (+ var 1))
                                        var)
                                 x]
                                [(begin (set! var (+ var 2))
                                        var)
                                 y]
                                #:closed)
                         (list x y)]
                        [_ 'failed])
                      (list 2 4))
        (check-equal? var 3))

      ;; rest mode
      (let ([var 0])
        (check-equal? (match (hash -5 -6 1 2 3 4)
                        [(hash* [(begin (set! var (+ var 1))
                                        var)
                                 x]
                                [(begin (set! var (+ var 2))
                                        var)
                                 y]
                                #:rest (? (λ (h) (= 1 (hash-count h))) h))
                         (list x y h)]
                        [_ 'failed])
                      (list 2 4 (hash -5 -6)))
        (check-equal? var 3)))

    (test-case "default value"
      ;; mismatch
      (check-equal? (match (hash 1 2
                                 3 4
                                 5 6)
                      [(hash* [1 (list x) #:default (list 42)]) (list x)]
                      [_ 'failed])
                    'failed)

      ;; partial
      (check-equal? (match (hash 1 (list 2)
                                 3 (list 4)
                                 5 (list 6))
                      [(hash* [2 (list x) #:default (list 42)]
                              [5 (list z) #:default (list -42)])
                       (list x z)]
                      [_ 'failed])
                    (list 42 6))

      (check-equal? (match (hash 0 2)
                      [(hash* [1 x #:default 3] [1 y #:default 4])
                       (list x y)]
                      [_ 'failed])
                    (list 3 4))

      ;; full
      (check-equal? (match (hash 1 (list 2)
                                 3 (list 4)
                                 5 (list 6))
                      [(hash* [2 w #:default (list 42)]
                              [1 x]
                              [3 y]
                              [5 z #:default (list -42)]
                              #:closed)
                       (list w x y z)]
                      [_ 'failed])
                    (list (list 42) (list 2) (list 4) (list 6)))

      (check-equal? (match (hash 1 2)
                      [(hash* [1 x #:default 3] [1 y #:default 4] #:closed)
                       (list x y)]
                      [_ 'failed])
                    (list 2 2))

      ;; full failure
      (check-equal? (match (hash 1 (list 2)
                                 3 (list 4)
                                 5 (list 6))
                      ;; 1 is not matched here
                      [(hash* [2 w #:default (list 42)]
                              [3 y]
                              [5 z #:default (list -42)]
                              #:closed)
                       (list w y z)]
                      [_ 'failed])
                    'failed)

      ;; rest
      (check-equal? (match (hash 1 (list 2)
                                 3 (list 4)
                                 5 (list 6))
                      [(hash* [2 w #:default (list 42)]
                              [3 y]
                              [5 z #:default (list -42)]
                              #:rest h)
                       (list w y z h)]
                      [_ 'failed])
                    (list (list 42) (list 4) (list 6) (hash 1 (list 2)))))

    (test-case "key comparator"
      (let ([b-1 (box 1)]
            [b-2 (box 3)])
        (check-equal? (match (hasheq b-1 2 b-2 4)
                        [(hash* [b-1 x] [b-2 y]) (list x y)]
                        [_ 'failed])
                      (list 2 4))

        (check-equal? (match (hasheq b-1 2 b-2 4)
                        [(hash* [(box 1) x] [(box 3) y]) (list x y)]
                        [_ 'failed])
                      'failed)))

    (test-case "mutability/weakness"
      (check-equal? (match (make-immutable-hash (list (cons 1 2) (cons 3 4)))
                      [(hash* [1 x] #:rest h) (list x h (immutable? h) (hash-strong? h))]
                      [_ 'failed])
                    (list 2 (make-immutable-hash (list (cons 3 4))) #t #t))

      (check-equal? (match (make-hash (list (cons 1 2) (cons 3 4)))
                      [(hash* [1 x] #:rest h) (list x h (immutable? h) (hash-strong? h))]
                      [_ 'failed])
                    (list 2 (make-hash (list (cons 3 4))) #f #t))

      (check-equal? (match (make-weak-hash (list (cons 1 2) (cons 3 4)))
                      [(hash* [1 x] #:rest h) (list x h (immutable? h) (hash-strong? h))]
                      [_ 'failed])
                    (list 2 (make-weak-hash (list (cons 3 4))) #f #f)))

    (test-case "hash"
      (check-equal? (match (hash 1 2 3 4)
                      [(hash 1 x 3 y) (list x y)])
                    (list 2 4)))))
