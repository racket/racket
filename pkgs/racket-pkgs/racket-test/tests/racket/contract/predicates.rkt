#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract
                                               'racket/set)])
  
  (ctest #t flat-contract? (or/c))
  (ctest #t flat-contract? (or/c integer? (lambda (x) (> x 0))))
  (ctest #t flat-contract? (or/c (flat-contract integer?) (flat-contract boolean?)))
  (ctest #t flat-contract? (or/c integer? boolean?))
  
  (ctest #t flat-contract? (-> any/c any/c any))

  (ctest #t flat-contract? (and/c))
  (ctest #t flat-contract? (and/c number? integer?))
  (ctest #t flat-contract? (and/c (flat-contract number?)
				 (flat-contract integer?)))
  (ctest #t flat-contract? (let ()
                             (define-struct s (a b))
                             (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s (a b) #:mutable)
                             (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s (a b) #:mutable)
                             (struct/c s any/c integer?)))
  (ctest #t chaperone-contract? (let ()
                                  (define-struct s (a b) #:mutable)
                                  (struct/c s any/c any/c)))
  (ctest #f impersonator-contract? (let ()
                                    (define-struct s (a b) #:mutable)
                                    (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s ([a #:mutable] b))
                             (struct/c s any/c any/c)))
  (ctest #t chaperone-contract? (let ()
                                  (define-struct s ([a #:mutable] b))
                                  (struct/c s any/c any/c)))
  (ctest #f impersonator-contract? (let ()
                                    (define-struct s ([a #:mutable] b))
                                    (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s (a [b #:mutable]))
                             (struct/c s any/c any/c)))
  (ctest #t chaperone-contract? (let ()
                                  (define-struct s (a [b #:mutable]))
                                  (struct/c s any/c any/c)))
  (ctest #f impersonator-contract? (let ()
                                    (define-struct s (a [b #:mutable]))
                                    (struct/c s any/c any/c)))
  (ctest #f flat-contract? (let ()
                             (define-struct s (f))
                             (struct/c s (-> number? any))))
  (ctest #t chaperone-contract? (let ()
                                  (define-struct s (f))
                                  (struct/c s (-> number? any))))
  (ctest #f impersonator-contract? (let ()
                                    (define-struct s (f))
                                    (struct/c s (-> number? any))))

  (ctest #f flat-contract? (let ()
                             (define-struct s (a) #:mutable)
                             (define alpha (new-∃/c 'alpha))
                             (struct/c s alpha)))
  (ctest #f chaperone-contract? (let ()
                                  (define-struct s (a) #:mutable)
                                  (define alpha (new-∃/c 'alpha))
                                  (struct/c s alpha)))
  (ctest #t impersonator-contract? (let ()
                                    (define-struct s (a) #:mutable)
                                    (define alpha (new-∃/c 'alpha))
                                    (struct/c s alpha)))
  (ctest #t contract? (let ()
                        (define-struct s (a) #:mutable)
                        (define alpha (new-∃/c 'alpha))
                        (struct/c s alpha)))

  (ctest #t chaperone-contract?
         (let ()
           (struct s (a b))
           (let ([x (struct/dc s [a integer?] [b integer?])])
             (opt/c x))))

  (ctest #t flat-contract? (set/c integer?))
  (ctest #f flat-contract? (set/c (-> integer? integer?)))
  (ctest #t chaperone-contract? (set/c (-> integer? integer?)))

  (ctest #t flat-contract? (list/c integer?))
  (ctest #t chaperone-contract? (list/c (-> integer? integer?)))

  (ctest #t chaperone-contract? (promise/c integer?))
  (ctest #f chaperone-contract? (promise/c (new-∃/c 'alpha)))

  ;; Make sure that impersonators cannot be used as the element contract in set/c.
  (contract-error-test
   'contract-error-test-set
   '(let ([proxy-ctc
           (make-contract
            #:name 'proxy-ctc
            #:first-order values
            #:projection (λ (b) values))])
      (set/c proxy-ctc))
   exn:fail?)

  ;; Hash contracts with flat domain/range contracts
  (ctest #t contract?              (hash/c any/c any/c #:immutable #f))
  (ctest #t chaperone-contract?    (hash/c any/c any/c #:immutable #f))
  (ctest #f impersonator-contract? (hash/c any/c any/c #:immutable #f))
  (ctest #t flat-contract?         (hash/c any/c any/c #:immutable #f #:flat? #t))

  (ctest #t flat-contract?      (hash/c any/c any/c #:immutable #t))
  (ctest #t flat-contract?      (hash/c any/c any/c #:immutable #t #:flat? #t))

  (ctest #t contract?              (hash/c any/c any/c))
  (ctest #t chaperone-contract?    (hash/c any/c any/c))
  (ctest #f impersonator-contract? (hash/c any/c any/c))
  (ctest #t flat-contract?         (hash/c any/c any/c #:flat? #t))

  ;; Hash contracts with chaperone range contracts
  (ctest #t contract?              (hash/c number? (hash/c number? number?)))
  (ctest #t chaperone-contract?    (hash/c number? (hash/c number? number?)))
  (ctest #f impersonator-contract? (hash/c number? (hash/c number? number?)))
  (ctest #f flat-contract?         (hash/c number? (hash/c number? number?)))

  ;; Hash contracts with proxy range contracts
  (contract-eval
   '(define trivial-proxy-ctc
      (make-contract
       #:name 'trivial-proxy-ctc
       #:first-order values
       #:projection (λ (b) values))))

  (ctest #t contract?              (hash/c number? trivial-proxy-ctc #:immutable #f))
  (ctest #f chaperone-contract?    (hash/c number? trivial-proxy-ctc #:immutable #f))
  (ctest #t impersonator-contract? (hash/c number? trivial-proxy-ctc #:immutable #f))
  (ctest #f flat-contract?         (hash/c number? trivial-proxy-ctc #:immutable #f))

  (ctest #t contract?              (hash/c number? trivial-proxy-ctc #:immutable #t))
  (ctest #f chaperone-contract?    (hash/c number? trivial-proxy-ctc #:immutable #t))
  (ctest #t impersonator-contract? (hash/c number? trivial-proxy-ctc #:immutable #t))
  (ctest #f flat-contract?         (hash/c number? trivial-proxy-ctc #:immutable #t))

  (ctest #t contract?              (hash/c number? trivial-proxy-ctc))
  (ctest #f chaperone-contract?    (hash/c number? trivial-proxy-ctc))
  (ctest #t impersonator-contract? (hash/c number? trivial-proxy-ctc))
  (ctest #f flat-contract?         (hash/c number? trivial-proxy-ctc))

  ;; Make sure that proxies cannot be used as the domain contract in hash/c.
  (contract-error-test
   'contract-error-test6
   '(let ([proxy-ctc
           (make-contract
            #:name 'proxy-ctc
            #:first-order values
            #:projection (λ (b) values))])
      (hash/c proxy-ctc proxy-ctc))
   exn:fail?)

  (ctest #t contract?              (box/c number? #:flat? #t))
  (ctest #t chaperone-contract?    (box/c number? #:flat? #t))
  (ctest #f impersonator-contract? (box/c number? #:flat? #t))
  (ctest #t flat-contract?         (box/c number? #:flat? #t))

  (ctest #t contract?              (box/c number? #:immutable #t))
  (ctest #t chaperone-contract?    (box/c number? #:immutable #t))
  (ctest #f impersonator-contract? (box/c number? #:immutable #t))
  (ctest #t flat-contract?         (box/c number? #:immutable #t))

  (ctest #t contract?              (box/c number?))
  (ctest #t chaperone-contract?    (box/c number?))
  (ctest #f impersonator-contract? (box/c number?))
  (ctest #f flat-contract?         (box/c number?))

  (ctest #t contract?              (box/c (box/c number?) #:immutable #t))
  (ctest #t chaperone-contract?    (box/c (box/c number?) #:immutable #t))
  (ctest #f impersonator-contract? (box/c (box/c number?) #:immutable #t))
  (ctest #f flat-contract?         (box/c (box/c number?) #:immutable #t))

  (ctest #t contract?              (box/c trivial-proxy-ctc))
  (ctest #f chaperone-contract?    (box/c trivial-proxy-ctc))
  (ctest #t impersonator-contract? (box/c trivial-proxy-ctc))
  (ctest #f flat-contract?         (box/c trivial-proxy-ctc))

  (ctest #t contract?              (box/c trivial-proxy-ctc #:immutable #t))
  (ctest #f chaperone-contract?    (box/c trivial-proxy-ctc #:immutable #t))
  (ctest #t impersonator-contract? (box/c trivial-proxy-ctc #:immutable #t))
  (ctest #f flat-contract?         (box/c trivial-proxy-ctc #:immutable #t))

  ;; Test the ability to create different types of contracts with recursive-contract
  (ctest #t flat-contract? (letrec ([ctc (or/c number?
                                               (cons/c (recursive-contract ctc #:flat)
                                                       (recursive-contract ctc #:flat)))])
                             ctc))

  (ctest #f flat-contract? (letrec ([ctc (or/c number?
                                               (box/c (recursive-contract ctc #:chaperone)))])
                             ctc))
  (ctest #t chaperone-contract? (letrec ([ctc (or/c number?
                                                    (box/c (recursive-contract ctc #:chaperone)))])
                                  ctc))
  (ctest #f impersonator-contract? (letrec ([ctc (or/c number?
                                                  (box/c (recursive-contract ctc #:chaperone)))])
                                    ctc))

  (ctest #t contract? 1)
  (ctest #t contract? (-> 1 1))

  (ctest #t chaperone-contract? (let ()
                                  (struct s (a b))
                                  (struct/dc s [a integer?] [b integer?])))
  (ctest #t flat-contract? (let ()
                             (struct s (a b))
                             (struct/dc s [a integer?] [b integer?])))
  (ctest #t flat-contract? (let ()
                             (struct s (a b))
                             (struct/dc s [a integer?] [b (a) #:flat (>=/c a)])))
  (contract-error-test
   'struct/dc-not-really-flat-dep-field
   #'(let ()
       (struct s (a b))
       (contract (struct/dc s [a integer?] [b (a) #:flat (-> integer? integer?)])
                 (s 1 (λ (x) x))
                 'pos
                 'neg))
   exn:fail?)
  (ctest #t chaperone-contract? (let ()
                                  (struct s (a b))
                                  (struct/dc s [a integer?] [b (a) (>=/c a)]))))