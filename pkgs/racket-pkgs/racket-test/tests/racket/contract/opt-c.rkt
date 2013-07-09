#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])

  (contract-eval '(define-contract-struct couple (hd tl)))
  
  (contract-eval
   '(begin
      (define proj:blame/c
        (make-contract
         #:name 'proj:blame/c
         #:projection
         (lambda (blame)
           (lambda (x)
             (if (blame-swapped? blame) 'negative 'positive)))))

      (define call*0 'dummy)
      (define (call*1 x0) x0)
      (define (call*2 f1 x0) (f1 x0))
      (define (call*3 f2 x1 x0) (f2 x1 x0))))

  (test/spec-passed/result
   'opt/c-blame-0
   '((contract
      (-> (-> (-> proj:blame/c any/c) any/c any/c) (-> any/c any/c) any/c any/c)
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  (test/spec-passed/result
   'opt/c-blame-1
   '((contract
      (opt/c (-> (-> (-> proj:blame/c any/c) any/c any/c) (-> any/c any/c) any/c any/c))
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  (test/spec-passed/result
   'opt/c-blame-2
   '((contract
      (-> (opt/c (-> (-> proj:blame/c any/c) any/c any/c)) (-> any/c any/c) any/c any/c)
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  (test/spec-passed/result
   'opt/c-blame-3
   '((contract
      (-> (-> (opt/c (-> proj:blame/c any/c)) any/c any/c) (-> any/c any/c) any/c any/c)
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  (test/spec-passed/result
   'opt/c-blame-4
   '((contract
      (-> (-> (-> (opt/c proj:blame/c) any/c) any/c any/c) (-> any/c any/c) any/c any/c)
      call*3
      'pos
      'neg)
     call*2
     call*1
     call*0)
   'negative)

  ;; NOT YET RELEASED
  #;
  (test/pos-blame
   'd-c-s/attr-1
   '(let ()
      (define-contract-struct pr (x y))
      (pr-x
       (contract (pr/dc [x integer?]
                        [y integer?]
                        where
                        [x-val x]
                        [y-val y]
                        and
                        (= x-val y-val))
                 (make-pr 4 5)
                 'pos
                 'neg))))

  ;; NOT YET RELEASED
  #;
  (test/spec-passed
   'd-c-s/attr-2
   '(let ()
      (define-contract-struct pr (x y))
      (contract (pr/dc [x integer?]
                       [y integer?]
                       where
                       [x-val x]
                       [y-val y]
                       and
                       (= x-val y-val))
                (make-pr 4 5)
                'pos
                'neg)))

  ;; NOT YET RELEASED
  #;
  (let ()
    (define-contract-struct node (n l r) (make-inspector))

    (define (get-val n attr)
      (if (null? n)
          1
          (let ([h (synthesized-value n attr)])
            (if (unknown? h)
                h
                (+ h 1)))))

    (define (full-bbt lo hi)
      (or/c null?
            (node/dc [n (between/c lo hi)]
                     [l (n) (full-bbt lo n)]
                     [r (n) (full-bbt n hi)]

                     where
                     [lheight (get-val l lheight)]
                     [rheight (get-val r rheight)]

                     and
                     (<= 0 (- lheight rheight) 1))))

    (define t (contract (full-bbt -inf.0 +inf.0)
                        (make-node 0
                                   (make-node -1 null null)
                                   (make-node 2
                                              (make-node 1 null null)
                                              (make-node 3 null null)))
                        'pos
                        'neg))
    (test/spec-passed
     'd-c-s/attr-3
     `(,node-l (,node-l ,t)))

    (test/pos-blame
     'd-c-s/attr-4
     `(,node-r (,node-r (,node-r ,t)))))

  ;; NOT YET RELEASED
  #|

need a test that will revisit a node a second time (when it already has a wrapper)
with a new parent. make sure the new parent is recorded in the parents field
so that propagation occurs.

|#


  ;; test the predicate
  (ctest #t couple? (contract (couple/c any/c any/c) (make-couple 1 2) 'pos 'neg))
  (ctest #t couple? (make-couple 1 2))
  (ctest #t couple? (contract (couple/dc [hd any/c] [tl (hd) any/c]) (make-couple 1 2) 'pos 'neg))
  (ctest #f couple? 1)
  (ctest #f couple? #f))