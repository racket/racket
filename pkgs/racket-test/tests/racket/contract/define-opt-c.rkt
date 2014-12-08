#lang racket/base
(require "test-util.rkt")
(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])

  (contract-eval '(define-contract-struct node (val obj rank left right) (make-inspector)))
  (contract-eval '(define (compute-rank n)
                    (cond
                      [(not n) 0]
                      [else (node-rank n)])))

  (contract-eval '(define-opt/c (leftist-heap-greater-than/rank/opt n r)
                    (or/c not
                          (node/dc [val (>=/c n)]
                                   [obj any/c]
                                   [rank (<=/c r)]
                                   [left (val) (leftist-heap-greater-than/rank/opt val +inf.0)]
                                   [right (val left)
                                          (leftist-heap-greater-than/rank/opt 
                                           val
                                           (compute-rank left))]))))

  (contract-eval '(define leftist-heap/c (leftist-heap-greater-than/rank/opt -inf.0 +inf.0)))

  (test/pos-blame 'd-o/c1 '(contract leftist-heap/c 2 'pos 'neg))


  (test/spec-passed 'd-o/c2 '(contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
  (test/spec-passed 'd-o/c3 '(contract leftist-heap/c #f 'pos 'neg))
  (test/spec-passed 'd-o/c4 '(contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
  (test/spec-passed/result 'd-o/c5
                           '(node? (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
                           #t)

  (test/spec-passed/result 'd-o/c6 
                           '(node-val (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
                           1)
  (test/spec-passed/result 'd-o/c7
                           '(node-obj (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
                           2)
  (test/spec-passed/result 'd-o/c8
                           '(node-rank (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
                           3)
  (test/spec-passed/result 'd-o/c9
                           '(node-left (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
                           #f)
  (test/spec-passed/result 'd-o/c10
                           '(node-right (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg))
                           #f)

  (test/spec-passed/result 'd-o/c11
                           '(node-val (contract leftist-heap/c
                                                (contract leftist-heap/c
                                                          (make-node 1 2 3 #f #f)
                                                          'pos 'neg)
                                                'pos 'neg))
                           1)
  (test/spec-passed/result 'd-o/c12
                           '(node-obj (contract leftist-heap/c
                                                (contract leftist-heap/c
                                                          (make-node 1 2 3 #f #f)
                                                          'pos 'neg)
                                                'pos 'neg))
                           2)
  (test/spec-passed/result 'd-o/c13
                           '(node-rank (contract leftist-heap/c
                                                 (contract leftist-heap/c
                                                           (make-node 1 2 3 #f #f)
                                                           'pos 'neg)
                                                 'pos 'neg))
                           3)
  (test/spec-passed/result 'd-o/c14
                           '(node-left (contract leftist-heap/c
                                                 (contract leftist-heap/c
                                                           (make-node 1 2 3 #f #f)
                                                           'pos 'neg)
                                                 'pos 'neg))
                           #f)
  (test/spec-passed/result 'd-o/c15
                           '(node-right (contract leftist-heap/c
                                                  (contract leftist-heap/c
                                                            (make-node 1 2 3 #f #f)
                                                            'pos 'neg)
                                                  'pos 'neg))
                           #f)

  (test/spec-passed/result 'd-o/c16
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-val h)
                              (node-val h))
                           1)
  (test/spec-passed/result 'd-o/c17
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-obj h)
                              (node-obj h))
                           2)

  (test/spec-passed/result 'd-o/c18
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f)'pos 'neg)])
                              (node-rank h)
                              (node-rank h))
                           3)
  (test/spec-passed/result 'd-o/c19
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-left h)
                              (node-left h))
                           #f)
  (test/spec-passed/result 'd-o/c20
                           '(let ([h (contract leftist-heap/c (make-node 1 2 3 #f #f) 'pos 'neg)])
                              (node-right h)
                              (node-right h))
                           #f)

  (test/spec-passed/result 'd-o/c21
                           '(node-val
                             (node-right
                              (contract leftist-heap/c
                                        (make-node 1 2 3
                                                   (make-node 7 8 9 #f #f)
                                                   (make-node 4 5 6 #f #f))
                                        'pos 'neg)))
                           4)
  (test/spec-passed/result 'd-o/c22
                           '(node-val
                             (node-left
                              (contract leftist-heap/c
                                        (make-node 1 2 3
                                                   (make-node 7 8 9 #f #f)
                                                   (make-node 4 5 6 #f #f))
                                        'pos 'neg)))
                           7)

  (test/pos-blame 'd-o/c23
                  '(node-val
                    (node-right
                     (contract leftist-heap/c
                               (make-node 5 2 3
                                          (make-node 7 8 9 #f #f)
                                          (make-node 4 5 6 #f #f))
                               'pos 'neg))))

  (test/pos-blame 'd-o/c24
                  '(node-val
                    (node-left
                     (contract leftist-heap/c
                               (make-node 9 2 3
                                          (make-node 7 8 9 #f #f)
                                          (make-node 11 5 6 #f #f))
                               'pos 'neg))))

  (test/neg-blame 'd-o/c25
                  '((contract (-> leftist-heap/c any)
                              (λ (kh)
                                (node-val
                                 (node-left
                                  kh)))
                              'pos 'neg)
                    (make-node 9 2 3
                               (make-node 7 8 9 #f #f)
                               (make-node 11 5 6 #f #f))))



  (test/spec-passed/result
   'd-o/c26
   '(let ([ai (λ (x) (contract leftist-heap/c x 'pos 'neg))])
      (define (remove-min t) (merge (node-left t) (node-right t)))

      (define (merge t1 t2)
        (cond
          [(not t1) t2]
          [(not t2) t1]
          [else
           (let ([t1-val (node-val t1)]
                 [t2-val (node-val t2)])
             (cond
               [(<= t1-val t2-val)
                (pick t1-val
                      (node-obj t1)
                      (node-left t1)
                      (merge (node-right t1)
                             t2))]
               [else
                (pick t2-val
                      (node-obj t2)
                      (node-left t2)
                      (merge t1
                             (node-right t2)))]))]))

      (define (pick x obj a b)
        (let ([ra (compute-rank a)]
              [rb (compute-rank b)])
          (cond
            [(>= ra rb)
             (make-node x obj (+ rb 1) a b)]
            [else
             (make-node x obj (+ ra 1) b a)])))
      (node-val
       (remove-min (ai (make-node 137 'x 1
                                  (ai (make-node 178 'y 1
                                                 (make-node 178 'z 1 #f #f)
                                                 #f))
                                  #f)))))
   178)

  (test/spec-passed/result
   'd-o/c27
   '(let ()
      (define-opt/c (f x)
        (and/c (>=/c x)
               (g x)))
      (define-opt/c (g x)
        (<=/c x))
      (contract (f 11) 11 'pos 'neg))
   11)

  ;; try-one : syntax -> number
  ;; evaluates the exp and returns the number of opt/c warnings found
  (contract-eval
   '(define (eval-and-count-log-messages exp)
      (define ans (make-channel))
      (define recv (make-log-receiver (current-logger) 'warning))
      (thread
       (λ ()
         (let loop ([opt/c-msgs 0])
           (define res (sync recv))
           (cond
             [(equal? "done" (vector-ref res 1))
              (channel-put ans opt/c-msgs)]
             [else
              (define opt/c-msg? (regexp-match? #rx"opt/c" (vector-ref res 1)))
              (loop (if opt/c-msg?
                        (+ opt/c-msgs 1)
                        opt/c-msgs))]))))
      (let/ec k
        (parameterize ([error-escape-handler k])
          (eval exp)))
      (log-warning "done")
      (channel-get ans)))

  (ctest 1 eval-and-count-log-messages
         '(let ()
            (struct s (a b))
            (opt/c (struct/dc s [a (-> integer? integer?)] [b (a) integer?]))))

  (ctest 1 eval-and-count-log-messages
         '(let ()
            (struct s (a b))
            (define-opt/c (f x)
              (-> integer? integer?))
            (define-opt/c (g x)
              (struct/dc s [a (f 1)] [b (a) integer?]))
            1))

  (ctest 0 eval-and-count-log-messages
         '(let ()
            (struct s (a b))
            (define-opt/c (f x) integer?)
            (opt/c (struct/dc s [a (f 1)] [b (a) integer?]))))

  (ctest 0 eval-and-count-log-messages
         '(let ()
            (define-struct h:kons (hd tl) #:transparent)
            (define-struct h:node (rank val obj children) #:transparent)

            (define-opt/c (binomial-tree-rank=/sco r v)
              (or/c #f
                    (struct/dc h:node
                               [rank (=/c r)]
                               [val (>=/c v)]
                               [children (rank val) #:lazy (heap-ordered/desc/sco (- rank 1) val)])))

            (define-opt/c (binomial-tree-rank>/sco r)
              (or/c #f
                    (struct/dc h:node
                               [rank (>=/c r)]
                               [val any/c]
                               [children (rank val) #:lazy (heap-ordered/desc/sco (- rank 1) val)])))

            (define-opt/c (heap-ordered/desc/sco rank val)
              (or/c #f
                    (struct/dc h:kons
                               [hd #:lazy (binomial-tree-rank=/sco rank val)]
                               [tl () #:lazy (heap-ordered/desc/sco (- rank 1) val)])))

            (define-opt/c (binomial-trees/asc/sco rank)
              (or/c #f
                    (struct/dc h:kons
                               [hd #:lazy (binomial-tree-rank>/sco rank)]
                               [tl (hd) #:lazy (binomial-trees/asc/sco (h:node-rank hd))])))

            (define binomial-heap/sco (binomial-trees/asc/sco -inf.0))
            1)))
