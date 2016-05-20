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
         #:val-first-projection
         (λ (blame)
           (λ (x)
             (λ (neg-party)
               (if (blame-swapped? blame) 'negative 'positive))))
         #:projection
         (lambda (blame)
           (lambda (x)
             (if (blame-swapped? blame) 'negative 'positive)))))

      (define call*0 'dummy)
      (define (call*1 x0) x0)
      (define (call*2 f1 x0) (f1 x0))
      (define (call*3 f2 x1 x0) (f2 x1 x0))))

  ;; the two are incomparable, but we still want to check, to make sure
  ;; contract-stronger works on contracts that use different kinds of
  ;; projections (late-neg for any/c, regular for proj:blame/c)
  (test/spec-passed/result
   'stronger-with-no-late-neg-projection
   '(contract-stronger? proj:blame/c any/c)
   #f)

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

   (test/spec-passed/result
    "or 1"
    '(contract (opt/c (or/c number? boolean?)) 1 'pos 'neg)
    1)
   
   (test/spec-passed/result
    "or 2"
    '(contract (opt/c (or/c number? boolean?)) #t 'pos 'neg)
    #t)
   
   (test/pos-blame
    "or 3"
    '(contract (opt/c (or/c number? boolean?)) "string" 'pos 'neg))
   
  (test/spec-passed/result
   "or 4"
   '((contract (opt/c (or/c number? (-> boolean? number?)))
               (λ (x) 1) 'pos 'neg) #t)
   1)
   
   (test/spec-passed/result
    "or 5"
    '((contract (opt/c (or/c (-> boolean? boolean? number?) (-> boolean? number?)))
                (λ (x y) 1) 'pos 'neg) #t #f)
    1)
   
   (test/spec-passed/result
    "lifting 1"
    '(let ((volatile 0))
       (contract (opt/c (between/c (begin (set! volatile 1) 3) 5)) 4 'pos 'neg)
       volatile)
    1)
   
   (test/spec-passed/result
    "arrow 1"
    '((contract (opt/c (-> boolean? number?)) (λ (x) 1) 'pos 'neg) #t)
    1)
   
   (test/spec-passed/result
    "arrow 2"
    '(call-with-values
      (λ () ((contract (opt/c (-> boolean? (values number? number?)))
                       (λ (x) (values 1 2)) 'pos 'neg) #t))
      list)
    '(1 2))
   
   (test/spec-passed/result
    "arrow 3"
    '(call-with-values
      (λ () ((contract (opt/c (-> boolean? any)) (λ (x) (values 1 2)) 'pos 'neg) #t))
      list)
    '(1 2))
   
   (test/spec-passed/result
    "arrow 4"
    '((contract (opt/c (-> boolean? any)) (λ (x) 1) 'pos 'neg) #t)
    1)
   
   (test/neg-blame
    "arrow 5"
    '((contract (opt/c (-> boolean? number?)) (λ (x) #t) 'pos 'neg) 1))
   
   (test/pos-blame
    "arrow 6"
    '((contract (opt/c (-> boolean? number?)) (λ (x) #t) 'pos 'neg) #t))
   
  (test/spec-passed/result
   "flat-contract 1"
   '(contract (opt/c (flat-contract (λ (x) (= x 1)))) 1 'pos 'neg)
   1)
   
   (test/spec-passed/result
    "flat-contract 2"
    '(with-handlers ([exn:fail? (λ (x) (regexp-match? #rx"expected: flat-contract[?]"
                                                      (exn-message x)))])
       (contract (opt/c (flat-contract (λ (x y) #f))) 1 'pos 'neg)
       'no-exn)
    #t)
   
   (test/spec-passed/result
    "cons/c 1"
    '(contract (opt/c (cons/c number? (flat-contract (λ (x) (= x 2)))))
               (cons 1 2) 'pos 'neg)
    '(1 . 2))
   
   (test/spec-passed/result
    "cons/c 1b"
    '(contract (opt/c (cons/c number? (flat-contract (λ (x) (= x 2)))))
               (cons 1 2) 'pos 'neg)
    '(1 . 2))
   
   (test/spec-passed/result
    "cons/c 2"
    '(let ([x (contract (opt/c (cons/c number? (-> number? any)))
                        (cons 1 (λ (x) 2)) 'pos 'neg)])
       (and (= (car x) 1) (= ((cdr x) 1) 2)))
    #t)
  
  (test/spec-passed/result
   "between/c 1"
   '(contract (opt/c (between/c 1 2)) 1 'pos 'neg)
   1)
   
   (test/pos-blame
    "between/c 2"
    '(contract (opt/c (between/c 1 2)) 3 'pos 'neg))
   
  (test/spec-passed/result
   "between/c 2"
   '(with-handlers ([exn:fail? (λ (x)
                                 (regexp-match?
                                  #rx"expected: real[?].*argument position: 1st"
                                  (exn-message x)))])
      (contract (opt/c (between/c 'x 'b)) 1 'pos 'neg)
      'no-exn)
   #t)
   
   (test/spec-passed/result
    "between/c 3"
    '(with-handlers ([exn:fail? (λ (x)
                                  (regexp-match?
                                   #rx"expected: real[?].*argument position: 2nd"
                                   (exn-message x)))])
       (contract (opt/c (between/c 1 'b)) 1 'pos 'neg))
    #t)

  
  
  ;; test the predicate
  (ctest #t couple? (contract (couple/c any/c any/c) (make-couple 1 2) 'pos 'neg))
  (ctest #t couple? (make-couple 1 2))
  (ctest #t couple? (contract (couple/dc [hd any/c] [tl (hd) any/c]) (make-couple 1 2) 'pos 'neg))
  (ctest #f couple? 1)
  (ctest #f couple? #f)

  (test/spec-passed/result
   "chaperone-contracts-stay-chaperones"
   '(let ([ctc (-> integer?)]
          [opt-ctc (opt/c (-> integer?))])
      (and (chaperone-contract? ctc)
           (not (impersonator-contract? ctc))
           (chaperone-contract? opt-ctc)
           (not (impersonator-contract? ctc))))
   #t)

  (test/spec-passed/result
   "impersonators-stay-impersonators"
   '(let ([ctc (->i () [_ any/c])]
          [opt-ctc (opt/c (->i () [_ any/c]))])
      (and (impersonator-contract? ctc)
           (not (chaperone-contract? ctc))
           (impersonator-contract? opt-ctc)
           (not (chaperone-contract? opt-ctc))))
   #t))
