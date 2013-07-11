#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])

  (contract-eval '(define-contract-struct couple (hd tl)))
  (contract-eval '(define-contract-struct triple (a b c)))
  
  (ctest #t contract-stronger? any/c any/c)
  (ctest #t contract-stronger? (between/c 1 3) (between/c 0 4))
  (ctest #f contract-stronger? (between/c 0 4) (between/c 1 3))
  (ctest #t contract-stronger? (>=/c 3) (>=/c 2))
  (ctest #f contract-stronger? (>=/c 2) (>=/c 3))
  (ctest #f contract-stronger? (<=/c 3) (<=/c 2))
  (ctest #t contract-stronger? (<=/c 2) (<=/c 3))
  (ctest #f contract-stronger? (recursive-contract (<=/c 2)) (recursive-contract (<=/c 3)))
  (ctest #f contract-stronger? (recursive-contract (<=/c 3)) (recursive-contract (<=/c 2)))
  (let ([f (contract-eval '(λ (x) (recursive-contract (<=/c x))))])
    (test #t (contract-eval 'contract-stronger?) (contract-eval `(,f 1)) (contract-eval `(,f 1))))
  (ctest #t contract-stronger? (-> integer? integer?) (-> integer? integer?))
  (ctest #f contract-stronger? (-> boolean? boolean?) (-> integer? integer?))
  (ctest #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 4) (>=/c 3)))
  (ctest #f contract-stronger? (-> (>=/c 4) (>=/c 3)) (-> (>=/c 3) (>=/c 3)))
  (ctest #t contract-stronger? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 3) (>=/c 2)))
  (ctest #f contract-stronger? (-> (>=/c 3) (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-stronger? (-> (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-stronger? 
         (-> integer? #:x integer? integer?)
         (-> integer? #:y integer? integer?))
  (ctest #f contract-stronger?
         (-> integer? #:y integer? integer?)
         (-> integer? #:x integer? integer?))
  (ctest #t contract-stronger?
         (-> integer? #:x integer? integer?)
         (-> integer? #:x integer? integer?))
  (ctest #t contract-stronger? (-> #:x (>=/c 3) (>=/c 3)) (-> #:x (>=/c 3) (>=/c 2)))

  (let ([c (contract-eval '(->* () () any))])
    (test #t (contract-eval 'contract-stronger?) c c))
  (let ([c (contract-eval '(->d () () any))])
    (test #t (contract-eval 'contract-stronger?) c c))
  (let ([c (contract-eval '(->i () () any))])
    (test #t (contract-eval 'contract-stronger?) c c))

  (ctest #f contract-stronger? 
         (->* () #:pre (zero? (random 10)) any) 
         (->* () #:pre (zero? (random 10)) any))
  (ctest #f contract-stronger? 
         (->* () integer? #:post (zero? (random 10))) 
         (->* () integer? #:post (zero? (random 10))))
  
  (ctest #t contract-stronger? (or/c null? any/c) (or/c null? any/c))
  (ctest #f contract-stronger? (or/c null? any/c) (or/c boolean? any/c))
  (ctest #t contract-stronger? (or/c null? boolean?) (or/c null? boolean?))
  (ctest #f contract-stronger? (or/c null? boolean?) (or/c boolean? null?))
  (ctest #t contract-stronger? 
         (or/c null? (-> integer? integer?))
         (or/c null? (-> integer? integer?)))
  (ctest #f contract-stronger?
         (or/c null? (-> boolean? boolean?))
         (or/c null? (-> integer? integer?)))

  (ctest #t contract-stronger? number? number?)
  (ctest #f contract-stronger? boolean? number?)

  (ctest #t contract-stronger? (parameter/c (between/c 0 5)) (parameter/c (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 0 5)) (parameter/c (between/c 1 4)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4)) (parameter/c (between/c 0 5)))

  (ctest #f contract-stronger? (parameter/c (between/c 1 4) (between/c 0 5))
                               (parameter/c (between/c 0 5)))
  (ctest #t contract-stronger? (parameter/c (between/c 0 5) (between/c 1 4))
                               (parameter/c (between/c 1 4)))
  (ctest #t contract-stronger? (parameter/c (between/c 0 5))
                               (parameter/c (between/c 1 4) (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4))
                               (parameter/c (between/c 0 5) (between/c 0 5)))
  (ctest #t contract-stronger? (parameter/c (between/c 0 5) (between/c 1 4))
                               (parameter/c (between/c 1 4) (between/c 0 5)))
  (ctest #f contract-stronger? (parameter/c (between/c 1 4) (between/c 0 5))
                               (parameter/c (between/c 0 5) (between/c 1 4)))

  (ctest #t contract-stronger? (symbols 'x 'y) (symbols 'x 'y 'z))
  (ctest #f contract-stronger? (symbols 'x 'y 'z) (symbols 'x 'y))
  (ctest #t contract-stronger? (symbols 'x 'y) (symbols 'z 'x 'y))
  (ctest #f contract-stronger? (symbols 'z 'x 'y) (symbols 'x 'y))
  (ctest #t contract-stronger? (one-of/c (expt 2 100)) (one-of/c (expt 2 100) 12))

  (ctest #t contract-stronger?
        (or/c (-> (>=/c 3) (>=/c 3)) (-> string?))
        (or/c (-> (>=/c 4) (>=/c 3)) (-> string?)))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?))
        (or/c (-> string?) (-> any/c integer?)))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> any/c integer?))
        (or/c (-> string?) (-> integer? integer?)))
  (ctest #t contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer? boolean?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer? char?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer?)
        (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-stronger?
        (or/c (-> string?) (-> integer? integer?) integer?)
        (or/c (-> integer? integer?) integer?))

  (contract-eval
   `(let ()
      (define (non-zero? x) (not (zero? x)))
      (define list-of-numbers
        (or/c null?
              (couple/c number?
                        (recursive-contract list-of-numbers))))
      (define (short-list/less-than n)
        (or/c null?
              (couple/c (<=/c n)
                        (or/c null?
                              (couple/c (<=/c n)
                                        any/c)))))
      (define (short-sorted-list/less-than n)
        (or/c null?
              (couple/dc
               [hd (<=/c n)]
               [tl (hd) (or/c null?
                              (couple/c (<=/c hd)
                                        any/c))])))

      (define (sorted-list/less-than n)
        (or/c null?
              (couple/dc
               [hd (<=/c n)]
               [tl (hd) (sorted-list/less-than hd)])))

      ;; for some reason, the `n' makes it harder to optimize. 
      ;; without it, this test isn't as good a test
      (define (closure-comparison-test n)
        (couple/dc
         [hd any/c]
         [tl (hd) any/c]))

      (,test #t contract-stronger? (couple/c any/c any/c) (couple/c any/c any/c))
      (,test #f contract-stronger? (couple/c (>=/c 2) (>=/c 3)) (couple/c (>=/c 4) (>=/c 5)))
      (,test #t contract-stronger? (couple/c (>=/c 4) (>=/c 5)) (couple/c (>=/c 2) (>=/c 3)))
      (,test #f contract-stronger? (couple/c (>=/c 1) (>=/c 5)) (couple/c (>=/c 5) (>=/c 1)))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) any/c])])
        (,test #t contract-stronger? ctc ctc))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) (<=/c hd)])])
        (,test #t contract-stronger? ctc ctc))
      (,test #t contract-stronger? list-of-numbers list-of-numbers)
      (,test #t contract-stronger? (short-list/less-than 4) (short-list/less-than 5))
      (,test #f contract-stronger? (short-list/less-than 5) (short-list/less-than 4))
      (,test #t contract-stronger? (short-sorted-list/less-than 4) (short-sorted-list/less-than 5))
      (,test #f contract-stronger? (short-sorted-list/less-than 5) (short-sorted-list/less-than 4))
      (,test #t contract-stronger? (sorted-list/less-than 4) (sorted-list/less-than 5))
      (,test #f contract-stronger? (sorted-list/less-than 5) (sorted-list/less-than 4))
      (,test #t contract-stronger? (closure-comparison-test 4) (closure-comparison-test 5))

      (letrec ([mk-c
                (λ (x)
                  (triple/dc [a (<=/c x)]
                             [b any/c]
                             [c (a b) (or/c #f (mk-c a))]))])
        (,test #t contract-stronger? (mk-c 1) (mk-c 2)))))


  (contract-eval
   `(let ()

      (struct s (a b))
      (struct t (a b))

      (,test #f contract-stronger?
             (struct/dc s
                        [a (>=/c 1)]
                        [b (>=/c 2)])
             (struct/dc s
                        [a (>=/c 2)]
                        [b (>=/c 3)]))
      (,test #t contract-stronger?
              (struct/dc s
                         [a (>=/c 2)]
                         [b (>=/c 3)])
              (struct/dc s
                         [a (>=/c 1)]
                         [b (>=/c 2)]))

      (,test #f contract-stronger?
             (struct/dc s
                    [a number?]
                    [b number?])
             (struct/dc t
                        [a number?]
                        [b number?]))

      (,test #f contract-stronger?
             (struct/dc t
                        [a number?]
                        [b number?])
             (struct/dc s
                        [a number?]
                        [b number?]))

      (define (mk c)
        (struct/dc s
                   [a (>=/c c)]
                   [b (a) (>=/c a)]))
      (define one (mk 1))
      (define two (mk 2))
      (,test #f contract-stronger? one two)
      (,test #t contract-stronger? two one))))