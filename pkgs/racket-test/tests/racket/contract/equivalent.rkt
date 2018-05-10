#lang racket/base
(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract
                                               'racket/list
                                               'racket/class
                                               'racket/math)])
  
  (contract-eval '(define-contract-struct couple (hd tl)))
  (contract-eval '(define-contract-struct triple (a b c)))

  (ctest #t contract-equivalent? any/c any/c)
  (ctest #f contract-equivalent? integer? any/c)

  (ctest #t contract-equivalent? (integer-in 0 4) (integer-in 0 4))
  (ctest #f contract-equivalent? (integer-in 1 3) (integer-in 0 4))
  (ctest #f contract-equivalent? (integer-in 0 4) (integer-in 1 3))
  (ctest #f contract-equivalent? (integer-in 0 4) (integer-in 1 #f))
  (ctest #f contract-equivalent? (integer-in 0 4) (integer-in #f 3))
  (ctest #f contract-equivalent? (integer-in 0 4) (integer-in #f 4))
  (ctest #f contract-equivalent? (integer-in 0 #f) (integer-in #f #f))
  (ctest #f contract-equivalent? (integer-in #f 0) (integer-in #f #f))
  (ctest #t contract-equivalent? (integer-in #f #f) (integer-in #f #f))
  (ctest #t contract-equivalent? (integer-in 0 0) (and/c 0 exact?))
  (ctest #t contract-equivalent? (and/c 0 exact?) (integer-in 0 0))
  (ctest #t contract-equivalent? exact-integer? (integer-in #f #f))
  (ctest #t contract-equivalent? (integer-in #f #f) exact-integer?)
  (ctest #t contract-equivalent? (integer-in 0 #f) exact-nonnegative-integer?)
  (ctest #t contract-equivalent? (integer-in 0 #f) natural?)
  (ctest #t contract-equivalent? natural? (integer-in 0 #f))
  (ctest #t contract-equivalent? (integer-in 1 #f) exact-positive-integer?)
  (ctest #t contract-equivalent? exact-positive-integer? (integer-in 1 #f))
  (ctest #f contract-equivalent? natural? exact-integer?) ;; this actually is `integer-in`

  (ctest #t contract-equivalent? (integer-in 0 5) (and/c natural? (<=/c 5)))
  (ctest #t contract-equivalent? (and/c natural? (<=/c 5)) (integer-in 0 5))
  (ctest #t contract-equivalent? (integer-in 0 5) (and/c exact-nonnegative-integer? (<=/c 5)))
  (ctest #t contract-equivalent? (and/c exact-nonnegative-integer? (<=/c 5)) (integer-in 0 5))
  (ctest #t contract-equivalent? (integer-in 5 #f) (and/c natural? (>=/c 5)))
  (ctest #t contract-equivalent? (and/c natural? (>=/c 5)) (integer-in 5 #f))
  (ctest #t contract-equivalent? (integer-in 0 #f) (and/c exact-nonnegative-integer? (>=/c -4)))
  (ctest #t contract-equivalent? (and/c exact-nonnegative-integer? (>=/c -4)) (integer-in 0 #f))

  (ctest #t contract-equivalent? #\a #\a)
  (ctest #t contract-equivalent? #\a (char-in #\a #\a))
  (ctest #t contract-equivalent? (char-in #\a #\a) (char-in #\a #\a))
  (ctest #f contract-equivalent? #\a (char-in #\a #\c))
  (ctest #f contract-equivalent? #\a (char-in #\b #\c))
  (ctest #f contract-equivalent? (char-in #\f #\q) (char-in #\a #\z))
  (ctest #f contract-equivalent? (char-in #\a #\z) (char-in #\f #\q))
  (ctest #f contract-equivalent? (between/c 1 3) (between/c 0 4))
  (ctest #f contract-equivalent? (between/c 0 4) (between/c 1 3))
  (ctest #t contract-equivalent? (between/c 0 4) (between/c 0 4))
  (ctest #t contract-equivalent? (between/c -inf.0 +inf.0) real?)
  (ctest #t contract-equivalent? real? (between/c -inf.0 +inf.0))
  (ctest #f contract-equivalent? (>=/c 3) (>=/c 2))
  (ctest #f contract-equivalent? (>=/c 2) (>=/c 3))
  (ctest #t contract-equivalent? (>=/c 2) (>=/c 2))
  (ctest #f contract-equivalent? (<=/c 3) (<=/c 2))
  (ctest #f contract-equivalent? (<=/c 2) (<=/c 3))
  (ctest #t contract-equivalent? (<=/c 2) (<=/c 2))
  (ctest #f contract-equivalent? (>/c 3) (>/c 2))
  (ctest #f contract-equivalent? (>/c 2) (>/c 3))
  (ctest #t contract-equivalent? (>/c 2) (>/c 2))
  (ctest #f contract-equivalent? (</c 3) (</c 2))
  (ctest #f contract-equivalent? (</c 2) (</c 3))
  (ctest #t contract-equivalent? (</c 2) (</c 2))
  (ctest #f contract-equivalent? (</c 2) (>/c 2))
  (ctest #f contract-equivalent? (</c 2) (<=/c 2))
  (ctest #f contract-equivalent? (</c 2) (>=/c 2))
  (ctest #f contract-equivalent? (>/c 2) (<=/c 2))
  (ctest #f contract-equivalent? (>/c 2) (>=/c 2))
  (ctest #f contract-equivalent? (</c 2) (<=/c 200))
  (ctest #f contract-equivalent? (<=/c 2) (</c 2))
  (ctest #f contract-equivalent? (<=/c 1) (</c 2))
  (ctest #f contract-equivalent? (>=/c 2) (</c 2))
  (ctest #f contract-equivalent? (<=/c 2) (>/c 2))
  (ctest #f contract-equivalent? (>=/c 2) (>/c 2))
  (ctest #f contract-equivalent? (>=/c 3) (>/c 2))

  (ctest #t contract-equivalent? (>/c 0) (and/c real? positive?))
  (ctest #t contract-equivalent? (and/c real? positive?) (>/c 0))
  (ctest #t contract-equivalent? (</c 0) (and/c real? negative?))
  (ctest #t contract-equivalent? (and/c real? negative?) (</c 0))
  (ctest #t contract-equivalent? (<=/c 0) (and/c real? (not/c positive?)))
  (ctest #t contract-equivalent? (and/c real? (not/c positive?)) (<=/c 0))
  (ctest #t contract-equivalent? (>=/c 0) (and/c real? (not/c negative?)))
  (ctest #t contract-equivalent? (and/c real? (not/c negative?)) (>=/c 0))

  (ctest #t contract-equivalent? (recursive-contract (<=/c 2)) (recursive-contract (<=/c 2)))
  (ctest #f contract-equivalent? (recursive-contract (<=/c 2)) (recursive-contract (<=/c 3)))
  (ctest #f contract-equivalent? (recursive-contract (<=/c 3)) (recursive-contract (<=/c 2)))
  (let ([f (contract-eval '(λ (x) (recursive-contract (<=/c x))))])
    (ctest #t contract-equivalent? (,f 1) (,f 1)))
  (ctest #f contract-equivalent?
         (letrec ([c (recursive-contract (-> (<=/c 5) c))]) c)
         (letrec ([c (recursive-contract (-> (<=/c 3) c))]) c))
  (ctest #t contract-equivalent?
         (letrec ([c (recursive-contract (-> (<=/c 3) c))]) c)
         (letrec ([c (recursive-contract (-> (<=/c 3) c))]) c))
  (ctest #f contract-equivalent?
         (letrec ([c (recursive-contract (-> (<=/c 3) c))]) c)
         (letrec ([c (recursive-contract (-> (<=/c 1) c))]) c))
  (ctest #t contract-equivalent?
         (letrec ([c (recursive-contract (-> (<=/c 1) c))]) c)
         (letrec ([c (recursive-contract (-> (<=/c 1) (-> (<=/c 1) c)))]) c))
  (ctest #t contract-equivalent?
         (letrec ([c (recursive-contract (-> (<=/c 1) (-> (<=/c 1) c)))]) c)
         (letrec ([c (recursive-contract (-> (<=/c 1) (-> (<=/c 1) (-> (<=/c 1) c))))]) c))
  (ctest #t contract-equivalent? (-> integer? integer?) (-> integer? integer?))
  (ctest #f contract-equivalent? (-> boolean? boolean?) (-> integer? integer?))
  (ctest #f contract-equivalent? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 4) (>=/c 3)))
  (ctest #f contract-equivalent? (-> (>=/c 4) (>=/c 3)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-equivalent? (-> (>=/c 3) (>=/c 3)) (-> (>=/c 3) (>=/c 2)))
  (ctest #f contract-equivalent? (-> (>=/c 3) (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-equivalent? (-> (>=/c 2)) (-> (>=/c 3) (>=/c 3)))
  (ctest #f contract-equivalent? 
         (-> integer? #:x integer? integer?)
         (-> integer? #:y integer? integer?))
  (ctest #f contract-equivalent?
         (-> integer? #:y integer? integer?)
         (-> integer? #:x integer? integer?))
  (ctest #t contract-equivalent?
         (-> integer? #:x integer? integer?)
         (-> integer? #:x integer? integer?))
  (ctest #t contract-equivalent? (-> #:x (>=/c 3) (>=/c 3)) (-> #:x (>=/c 3) (>=/c 3)))
  (ctest #f contract-equivalent? (-> #:x (>=/c 3) (>=/c 3)) (-> #:x (>=/c 3) (>=/c 2)))
  (ctest #t contract-equivalent? (-> any/c any/c any) (-> any/c any/c any))
  (ctest #f contract-equivalent? (-> any/c any/c any/c any) (-> any/c any/c any))
  (ctest #f contract-equivalent? (-> (-> any/c) integer?) (-> (-> any/c) any/c))
  (ctest #t contract-equivalent? (-> (-> any/c) integer?) (-> (-> any/c) integer?))
  (ctest #f contract-equivalent? (-> (-> any/c) any/c) (-> (-> any/c) integer?))

  (let ([c (contract-eval '(->* () () any))])
    (ctest #t contract-equivalent? ,c ,c))
  (let ([c (contract-eval '(->d () () any))])
    (ctest #t contract-equivalent? ,c ,c))
  (let ([c (contract-eval '(->i () () any))])
    (ctest #t contract-equivalent? ,c ,c))

  (ctest #f contract-equivalent?
         (->* () #:pre (zero? (random 10)) any) 
         (->* () #:pre (zero? (random 10)) any))
  (ctest #f contract-equivalent? 
         (->* () integer? #:post (zero? (random 10))) 
         (->* () integer? #:post (zero? (random 10))))
  
  (ctest #t contract-equivalent? (or/c null? #f) (or/c null? #f))
  (ctest #f contract-equivalent? (or/c null? #f) (or/c boolean? #f))
  (ctest #t contract-equivalent? (or/c null? boolean?) (or/c null? boolean?))
  (ctest #t contract-equivalent? (or/c null? boolean?) (or/c boolean? null?))
  (ctest #t contract-equivalent? 
         (or/c null? (-> integer? integer?))
         (or/c null? (-> integer? integer?)))
  (ctest #f contract-equivalent?
         (or/c null? (-> boolean? boolean?))
         (or/c null? (-> integer? integer?)))
  (ctest #f contract-equivalent? (or/c number? #f) number?)
  (ctest #f contract-equivalent? number? (or/c number? #f))
  (ctest #f contract-equivalent? (or/c (-> number? number?) #f) (-> number? number?))
  (ctest #f contract-equivalent? (-> number? number?) (or/c (-> number? number?) #f))
  (ctest #f contract-equivalent? (or/c (-> number? number?) (-> number? number? number?) #f) #f)
  (ctest #f contract-equivalent? #f (or/c (-> number? number?) (-> number? number? number?) #f))
  (ctest #f contract-equivalent? (or/c real?) (or/c integer? real?))
  (ctest #f contract-equivalent? (-> number?) (-> (or/c #f number?)))
  (ctest #f contract-equivalent? (-> (or/c #f number?) any/c) (-> number? any/c))
  (ctest #f contract-equivalent? (-> (or/c #f number?)) (-> number?))
  (ctest #f contract-equivalent? (-> number? any/c) (-> (or/c #f number?) any/c))
  (ctest #t contract-equivalent? (first-or/c null? #f) (first-or/c null? #f))
  (ctest #f contract-equivalent? (first-or/c null? #f) (first-or/c boolean? #f))
  (ctest #t contract-equivalent? (first-or/c null? boolean?) (first-or/c null? boolean?))
  (ctest #t contract-equivalent? (first-or/c null? boolean?) (first-or/c boolean? null?))
  (ctest #t contract-equivalent? 
         (first-or/c null? (-> integer? integer?))
         (first-or/c null? (-> integer? integer?)))
  (ctest #f contract-equivalent?
         (first-or/c null? (-> boolean? boolean?))
         (first-or/c null? (-> integer? integer?)))
  (ctest #f contract-equivalent? (first-or/c number? #f) number?)
  (ctest #f contract-equivalent? number? (first-or/c number? #f))
  (ctest #f contract-equivalent? (first-or/c (-> number? number?) #f) (-> number? number?))
  (ctest #f contract-equivalent? (-> number? number?) (first-or/c (-> number? number?) #f))
  (ctest #f contract-equivalent? (first-or/c (-> number? number?) (-> number? number? number?) #f) #f)
  (ctest #f contract-equivalent? #f (first-or/c (-> number? number?) (-> number? number? number?) #f))
  (ctest #f contract-equivalent? (first-or/c real?) (first-or/c integer? real?))
  (ctest #f contract-equivalent? (-> number?) (-> (first-or/c #f number?)))
  (ctest #f contract-equivalent? (-> (first-or/c #f number?) any/c) (-> number? any/c))
  (ctest #f contract-equivalent? (-> (first-or/c #f number?)) (-> number?))
  (ctest #f contract-equivalent? (-> number? any/c) (-> (first-or/c #f number?) any/c))

  (ctest #t contract-equivalent? (first-or/c null? #f) (or/c null? #f))
  (ctest #f contract-equivalent? (first-or/c null? #f) (or/c boolean? #f))
  (ctest #t contract-equivalent? (first-or/c null? boolean?) (or/c null? boolean?))
  (ctest #t contract-equivalent? (first-or/c null? boolean?) (or/c boolean? null?))

  (ctest #t contract-equivalent? (or/c null? #f) (first-or/c null? #f))
  (ctest #f contract-equivalent? (or/c null? #f) (first-or/c boolean? #f))
  (ctest #t contract-equivalent? (or/c null? boolean?) (first-or/c null? boolean?))
  (ctest #t contract-equivalent? (or/c null? boolean?) (first-or/c boolean? null?))
  
  (ctest #t contract-equivalent? number? number?)
  (ctest #f contract-equivalent? boolean? number?)

  (ctest #t contract-equivalent? (parameter/c (between/c 0 5)) (parameter/c (between/c 0 5)))
  (ctest #f contract-equivalent? (parameter/c (between/c 0 5)) (parameter/c (between/c 1 4)))
  (ctest #f contract-equivalent? (parameter/c (between/c 1 4)) (parameter/c (between/c 0 5)))

  (ctest #f contract-equivalent?
         (parameter/c (between/c 1 4) (between/c 0 5))
         (parameter/c (between/c 0 5)))
  (ctest #f contract-equivalent?
         (parameter/c (between/c 0 5) (between/c 1 4))
         (parameter/c (between/c 1 4)))
  (ctest #f contract-equivalent?
         (parameter/c (between/c 0 5))
         (parameter/c (between/c 1 4) (between/c 0 5)))
  (ctest #f contract-equivalent?
         (parameter/c (between/c 1 4))
         (parameter/c (between/c 0 5) (between/c 0 5)))
  (ctest #f contract-equivalent?
         (parameter/c (between/c 0 5) (between/c 1 4))
         (parameter/c (between/c 1 4) (between/c 0 5)))
  (ctest #f contract-equivalent?
         (parameter/c (between/c 1 4) (between/c 0 5))
         (parameter/c (between/c 0 5) (between/c 1 4)))

  (ctest #f contract-equivalent? (symbols 'x 'y) (symbols 'x 'y 'z))
  (ctest #f contract-equivalent? (symbols 'x 'y 'z) (symbols 'x 'y))
  (ctest #f contract-equivalent? (symbols 'x 'y) (symbols 'z 'x 'y))
  (ctest #f contract-equivalent? (symbols 'z 'x 'y) (symbols 'x 'y))
  (ctest #t contract-equivalent? (symbols 'z 'x 'y) (symbols 'z 'x 'y))
  (ctest #t contract-equivalent? (symbols 'z 'x 'y) (symbols 'y 'z 'x))
  (ctest #f contract-equivalent? (one-of/c (expt 2 100)) (one-of/c (expt 2 100) 12))

  (ctest #f contract-equivalent?
         (or/c (-> (>=/c 3) (>=/c 3)) (-> string?))
         (or/c (-> (>=/c 4) (>=/c 3)) (-> string?)))
  (ctest #t contract-equivalent?
         (or/c (-> (>=/c 4) (>=/c 3)) (-> string?))
         (or/c (-> (>=/c 4) (>=/c 3)) (-> string?)))
  (ctest #f contract-equivalent?
         (or/c (-> string?) (-> integer? integer?))
         (or/c (-> string?) (-> any/c integer?)))
  (ctest #f contract-equivalent?
         (or/c (-> string?) (-> #f integer?))
         (or/c (-> string?) (-> integer? integer?)))
  (ctest #t contract-equivalent?
         (or/c (-> string?) (-> integer? integer?) integer? boolean?)
         (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-equivalent?
         (or/c (-> string?) (-> integer? integer?) integer? char?)
         (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-equivalent?
         (or/c (-> string?) (-> integer? integer?) integer?)
         (or/c (-> string?) (-> integer? integer?) integer? boolean?))
  (ctest #f contract-equivalent?
         (or/c (-> string?) (-> integer? integer?) integer?)
         (or/c (-> integer? integer?) integer?))
  
  (ctest #t contract-equivalent? (list/c) '())
  (ctest #t contract-equivalent? '() (list/c))
  (ctest #t contract-equivalent? (cons/c boolean? integer?) (cons/c boolean? integer?))
  (ctest #f contract-equivalent? (cons/c boolean? integer?) (cons/c integer? boolean?))
  (ctest #t contract-equivalent? (cons/c number? (listof number?)) (non-empty-listof number?))
  (ctest #t contract-equivalent? (and/c pair? (listof number?)) (non-empty-listof number?))
  (ctest #t contract-equivalent? (non-empty-listof number?) (and/c (listof number?) pair?))
  (ctest #t contract-equivalent? (non-empty-listof number?) (cons/c number? (listof number?)))
  (ctest #f contract-equivalent? (cons/c number? (list/c number? number?)) (non-empty-listof number?))
  (ctest #f contract-equivalent? (cons/c number? (cons/c number? (listof number?))) (listof number?))
  (ctest #f contract-equivalent?
         (cons/c (<=/c 1) (cons/c (<=/c 2) (listof (<=/c 3)))) 
         (listof (<=/c 4)))
  (ctest #f contract-equivalent? (listof number?) (cons/c number? (cons/c number? (listof any/c))))
  (ctest #f contract-equivalent? (list*of (<=/c 2)) (list*of (<=/c 3)))
  (ctest #f contract-equivalent? (list*of (<=/c 3)) (list*of (<=/c 2)))
  (ctest #t contract-equivalent? (list*of (<=/c 3)) (list*of (<=/c 3)))
  (ctest #f contract-equivalent? (list*of (<=/c 2) char?) (list*of (<=/c 3) char?))
  (ctest #t contract-equivalent? (list*of (<=/c 3) char?) (list*of (<=/c 3) char?))
  (ctest #f contract-equivalent? (list*of (<=/c 3) char?) (list*of (<=/c 2) char?))
  (ctest #f contract-equivalent? (list*of char? (<=/c 2)) (list*of char? (<=/c 3)))
  (ctest #t contract-equivalent? (list*of char? (<=/c 2)) (list*of char? (<=/c 2)))
  (ctest #f contract-equivalent? (list*of char? (<=/c 3)) (list*of char? (<=/c 2)))
  (ctest #t contract-equivalent? (list*of char? null?) (listof char?))
  (ctest #t contract-equivalent? (listof char?) (list*of char? null?))
  (ctest #f contract-equivalent? (list*of char? any/c) (listof char?))
  
  (ctest #f contract-equivalent? (vectorof (<=/c 3)) (vectorof (<=/c 4)))
  (ctest #f contract-equivalent? (vectorof (<=/c 3)) (vectorof (<=/c 4)))
  (ctest #t contract-equivalent? (vectorof (<=/c 3)) (vectorof (<=/c 3)))
  (ctest #f contract-equivalent? (vectorof (<=/c 3) #:immutable #t) (vectorof (<=/c 4) #:immutable #t))
  (ctest #f contract-equivalent? (vectorof (<=/c 3) #:immutable #t) (vectorof (<=/c 3)))
  (ctest #f contract-equivalent? (vectorof (<=/c 3)) (vectorof (<=/c 3) #:immutable #t))
  (ctest #f contract-equivalent? (vectorof (<=/c 3)) (vectorof (<=/c 3) #:immutable #f))
  (ctest #f contract-equivalent? (vectorof (<=/c 3) #:immutable #f) (vectorof (<=/c 3)))
  (ctest #t contract-equivalent? (vectorof (<=/c 3) #:immutable #f) (vectorof (<=/c 3) #:immutable #f))
  (ctest #t contract-equivalent? (vectorof (<=/c 3)) (vectorof (<=/c 3)))
  
  (ctest #f contract-equivalent? (vector/c (<=/c 3) (<=/c 3) (<=/c 3)) (vectorof (<=/c 3)))
  (ctest #f contract-equivalent? (vector/c (<=/c 3) (<=/c 3) (<=/c 3)) (vectorof (<=/c 4)))
  (ctest #f contract-equivalent? (vector/c (<=/c 3) (<=/c 3) (<=/c 3)) (vectorof (<=/c 2)))
  (ctest #t contract-equivalent? (vector/c (<=/c 3) (<=/c 2)) (vector/c (<=/c 3) (<=/c 2)))
  (ctest #f contract-equivalent? (vector/c (<=/c 3) (<=/c 3)) (vector/c (<=/c 3) (<=/c 2)))
  (ctest #f contract-equivalent? (vector/c (<=/c 3) (<=/c 2)) (vector/c (<=/c 3) (<=/c 3)))
  (ctest #f contract-equivalent? (vector/c (<=/c 3) #:immutable #t) (vector/c (<=/c 3)))
  (ctest #f contract-equivalent? (vector/c (<=/c 3) #:immutable #f) (vector/c (<=/c 3)))
  (ctest #t contract-equivalent? (vector/c (<=/c 3) #:immutable #f) (vector/c (<=/c 3) #:immutable #f))
  (ctest #f contract-equivalent? (vector/c (<=/c 3)) (vector/c (<=/c 3) #:immutable #t))
  (ctest #f contract-equivalent? (vector/c (<=/c 3)) (vector/c (<=/c 3) #:immutable #f))
  (ctest #f contract-equivalent? (vector/c (<=/c 2) #:immutable #t) (vector/c (<=/c 3) #:immutable #t))
  (ctest #f contract-equivalent? (vector/c (<=/c 3) #:immutable #t) (vector/c (<=/c 2) #:immutable #t))
  
  (ctest #t contract-equivalent? (box/c (<=/c 3)) (box/c (<=/c 3)))
  (ctest #f contract-equivalent? (box/c (<=/c 3)) (box/c (<=/c 2)))
  (ctest #f contract-equivalent? (box/c (<=/c 2)) (box/c (<=/c 3)))
  (ctest #t contract-equivalent? (box/c (<=/c 3) #:immutable #t) (box/c (<=/c 3) #:immutable #t))
  (ctest #f contract-equivalent? (box/c (<=/c 2) #:immutable #t) (box/c (<=/c 3) #:immutable #t))
  (ctest #f contract-equivalent? (box/c (<=/c 3) #:immutable #t) (box/c (<=/c 2) #:immutable #t))
  (ctest #f contract-equivalent? (box/c (<=/c 3) #:immutable #t) (box/c (<=/c 3)))
  (ctest #f contract-equivalent? (box/c (<=/c 3) #:immutable #f) (box/c (<=/c 3)))
  (ctest #f contract-equivalent? (box/c (<=/c 3)) (box/c (<=/c 3) #:immutable #t))
  (ctest #f contract-equivalent? (box/c (<=/c 3)) (box/c (<=/c 3) #:immutable #f))
  
  (ctest #t contract-equivalent? (hash/c integer? symbol?) (hash/c integer? symbol?))
  (ctest #f contract-equivalent? (hash/c integer? symbol?) (hash/c symbol? integer?))
  (ctest #f contract-equivalent? (hash/c (<=/c 2) symbol?) (hash/c (<=/c 3) symbol?))
  (ctest #f contract-equivalent?
         (hash/c (<=/c 2) symbol? #:immutable #t)
         (hash/c (<=/c 3) symbol? #:immutable #t))
  (ctest #t contract-equivalent?
         (hash/c (<=/c 2) symbol? #:immutable #t)
         (hash/c (<=/c 2) symbol? #:immutable #t))
  (ctest #f contract-equivalent?
         (hash/c (<=/c 3) symbol? #:immutable #t)
         (hash/c (<=/c 2) symbol? #:immutable #t))
  (ctest #t contract-equivalent?
         (hash/c (<=/c 2) symbol? #:immutable #f)
         (hash/c (<=/c 2) symbol? #:immutable #f))
  (ctest #f contract-equivalent?
         (hash/c (<=/c 2) symbol? #:immutable #f)
         (hash/c (<=/c 2) symbol?))
  (ctest #f contract-equivalent?
         (hash/c (<=/c 2) symbol?)
         (hash/c (<=/c 2) symbol? #:immutable #f))
  
  (contract-eval
   `(let ()
      (define x (flat-rec-contract x (or/c (cons/c x '()) '())))
      (,test #:test-case-name 'flat-rec.1 #t contract-equivalent? x (or/c (cons/c x '()) '()))))
  (contract-eval
   `(let ()
      (define x (flat-rec-contract x (first-or/c (cons/c x '()) '())))
      (,test #:test-case-name 'flat-rec.2 #t contract-equivalent? x (first-or/c (cons/c x '()) '()))))
  
  (ctest #f contract-equivalent? "x" string?)
  (ctest #f contract-equivalent? string? "x")

  (ctest #f contract-equivalent? 1 real?)
  (ctest #f contract-equivalent? 1 (between/c -10 10))
  (ctest #f contract-equivalent? real? 1)
  (ctest #t contract-equivalent? real? real?)
  (ctest #t contract-equivalent? 1 1)

  (ctest #f contract-equivalent? 'x symbol?)
  (ctest #f contract-equivalent? symbol? 'x)
  
  (ctest #t contract-equivalent?
         (flat-named-contract 'name1 #f)
         (flat-named-contract 'name2 #f))
  (ctest #t contract-equivalent?
         (flat-named-contract 'name1 (flat-named-contract 'name2 #f))
         (flat-named-contract 'name3 (flat-named-contract 'name4 #f)))
  (ctest #t contract-equivalent? (flat-named-contract 'name1 1) (flat-named-contract 'name2 1))
  (ctest #t contract-equivalent? (flat-named-contract 'name1 "x") (flat-named-contract 'name2 "x"))
  (ctest #t contract-equivalent? 
         (flat-named-contract 'name2 (regexp "x"))
         (flat-named-contract 'name2 (regexp "x")))
  
  (ctest #f contract-equivalent? (listof (<=/c 3)) (listof (<=/c 5)))
  (ctest #t contract-equivalent? (listof (<=/c 5)) (listof (<=/c 5)))
  (ctest #t contract-equivalent? (list/c (<=/c 3) (<=/c 3)) (list/c (<=/c 3) (<=/c 3)))
  (ctest #f contract-equivalent? (list/c (<=/c 3) (<=/c 3)) (list/c (<=/c 3) (<=/c 3) (<=/c 3)))
  (ctest #f contract-equivalent? (list/c (<=/c 3) (<=/c 3) (<=/c 3)) (list/c (<=/c 3) (<=/c 3)))
  (ctest #f contract-equivalent? (list/c (<=/c 3) (<=/c 3)) (listof (<=/c 5)))
  (ctest #f contract-equivalent? (list/c (<=/c 3) (<=/c 3)) (non-empty-listof (<=/c 5)))
  (ctest #f contract-equivalent? (list/c (<=/c 3)) (non-empty-listof (<=/c 5)))
  (ctest #f contract-equivalent? (list/c) (non-empty-listof (<=/c 5)))
  (ctest #f contract-equivalent? (list/c) (listof (<=/c 5)))
  (ctest #t contract-equivalent? (list/c) (list/c))
  (ctest #t contract-equivalent? (listof (<=/c 5)) (listof (<=/c 5)))

  (ctest #t contract-equivalent? (*list/c integer? boolean? char?) (*list/c integer? boolean? char?))
  (ctest #f contract-equivalent? (list/c integer? boolean? char?) (listof (or/c integer? boolean? char?)))
  (ctest #t contract-equivalent? (list/c integer? boolean? char?) (list/c integer? boolean? char?))
  (ctest #t contract-equivalent? (listof (or/c integer? boolean? char?)) (listof (or/c integer? boolean? char?)))
  
  (ctest #f contract-equivalent? (promise/c (<=/c 2)) (promise/c (<=/c 3)))
  (ctest #f contract-equivalent? (promise/c (<=/c 3)) (promise/c (<=/c 2)))
  (ctest #t contract-equivalent? (promise/c (<=/c 2)) (promise/c (<=/c 2)))
  
  (ctest #f contract-equivalent? (syntax/c (<=/c 3)) (syntax/c (<=/c 4)))
  (ctest #f contract-equivalent? (syntax/c (<=/c 4)) (syntax/c (<=/c 3)))
  (ctest #t contract-equivalent? (syntax/c (<=/c 4)) (syntax/c (<=/c 4)))
  
  (ctest #t contract-equivalent? (parametric->/c (x) (-> x x)) (parametric->/c (x) (-> x x)))
  (ctest #f contract-equivalent? (parametric->/c (x) (-> x x)) (parametric->/c (x) (-> x (or/c x #f))))
  (ctest #f contract-equivalent? (parametric->/c (x) (-> x x)) (parametric->/c (x) (-> x (first-or/c x #f))))
  (ctest #f contract-equivalent? (parametric->/c (x y) (-> x y)) (parametric->/c (x y) (-> x x y)))
  (contract-eval `(define α (new-∀/c)))
  (ctest #f contract-equivalent? (-> α α) (-> α (or/c #f α)))
  (ctest #t contract-equivalent? (-> α α) (-> α α))
  (ctest #f contract-equivalent? (-> α (or/c #f α)) (-> α α))
  (ctest #f contract-equivalent? (-> α α) (-> α (first-or/c #f α)))
  (ctest #f contract-equivalent? (-> α (first-or/c #f α)) (-> α α))
  
  (ctest #t contract-equivalent?
         (class/c (m (-> any/c (<=/c 3))))
         (class/c (m (-> any/c (<=/c 3)))))
  (ctest #f contract-equivalent?
         (class/c (m (-> any/c (<=/c 3))))
         (class/c (m (-> any/c (<=/c 4)))))
  (ctest #f contract-equivalent?
         (class/c (m (-> any/c (<=/c 4))))
         (class/c (m (-> any/c (<=/c 3)))))
  (ctest #t contract-equivalent?
         (class/c (field [f integer?]))
         (class/c (field [f integer?])))
  (ctest #f contract-equivalent?
         (class/c (field [f (<=/c 3)]))
         (class/c (field [f (<=/c 4)])))
  (ctest #f contract-equivalent?
         (class/c (field [f (<=/c 4)]))
         (class/c (field [f (<=/c 3)])))
  (ctest #t contract-equivalent?
         (class/c (init [f (<=/c 3)]))
         (class/c (init [f (<=/c 3)])))
  (ctest #f contract-equivalent?
         (class/c (init [f (<=/c 3)]))
         (class/c (init [f (<=/c 4)])))
  (ctest #f contract-equivalent?
         (class/c (init [f (<=/c 4)]))
         (class/c (init [f (<=/c 3)])))
  (ctest #t contract-equivalent?
         (class/c (inherit [m (-> any/c (<=/c 3))]))
         (class/c (inherit [m (-> any/c (<=/c 3))])))
  (ctest #f contract-equivalent?
         (class/c (inherit [m (-> any/c (<=/c 3))]))
         (class/c (inherit [m (-> any/c (<=/c 4))])))
  (ctest #f contract-equivalent?
         (class/c (inherit [m (-> any/c (<=/c 4))]))
         (class/c (inherit [m (-> any/c (<=/c 3))])))
  (ctest #t contract-equivalent?
         (class/c (super [m (-> any/c (<=/c 3))]))
         (class/c (super [m (-> any/c (<=/c 3))])))
  (ctest #f contract-equivalent?
         (class/c (super [m (-> any/c (<=/c 3))]))
         (class/c (super [m (-> any/c (<=/c 4))])))
  (ctest #f contract-equivalent?
         (class/c (super [m (-> any/c (<=/c 4))]))
         (class/c (super [m (-> any/c (<=/c 3))])))
  (ctest #t contract-equivalent?
         (class/c (inner [m (-> any/c (<=/c 3))]))
         (class/c (inner [m (-> any/c (<=/c 3))])))
  (ctest #f contract-equivalent?
         (class/c (inner [m (-> any/c (<=/c 3))]))
         (class/c (inner [m (-> any/c (<=/c 4))])))
  (ctest #f contract-equivalent?
         (class/c (inner [m (-> any/c (<=/c 4))]))
         (class/c (inner [m (-> any/c (<=/c 3))])))
  (ctest #t contract-equivalent?
         (class/c (override [m (-> any/c (<=/c 3))]))
         (class/c (override [m (-> any/c (<=/c 3))])))
  (ctest #f contract-equivalent?
         (class/c (override [m (-> any/c (<=/c 3))]))
         (class/c (override [m (-> any/c (<=/c 4))])))
  (ctest #f contract-equivalent?
         (class/c (override [m (-> any/c (<=/c 4))]))
         (class/c (override [m (-> any/c (<=/c 3))])))
  (ctest #t contract-equivalent?
         (class/c (augment [m (-> any/c (<=/c 3))]))
         (class/c (augment [m (-> any/c (<=/c 3))])))
  (ctest #f contract-equivalent?
         (class/c (augment [m (-> any/c (<=/c 3))]))
         (class/c (augment [m (-> any/c (<=/c 4))])))
  (ctest #f contract-equivalent?
         (class/c (augment [m (-> any/c (<=/c 4))]))
         (class/c (augment [m (-> any/c (<=/c 3))])))
  (ctest #t contract-equivalent?
         (class/c (augride [m (-> any/c (<=/c 3))]))
         (class/c (augride [m (-> any/c (<=/c 3))])))
  (ctest #f contract-equivalent?
         (class/c (augride [m (-> any/c (<=/c 3))]))
         (class/c (augride [m (-> any/c (<=/c 4))])))
  (ctest #f contract-equivalent?
         (class/c (augride [m (-> any/c (<=/c 4))]))
         (class/c (augride [m (-> any/c (<=/c 3))])))
  (ctest #t contract-equivalent?
         (class/c (absent m))
         (class/c (absent m)))
  (ctest #f contract-equivalent?
         (class/c (absent m n))
         (class/c (absent m)))
  (ctest #f contract-equivalent?
         (class/c (absent m))
         (class/c (absent m n)))
  (ctest #t contract-equivalent?
         (class/c (absent (field f)))
         (class/c (absent (field f))))
  (ctest #f contract-equivalent?
         (class/c (absent (field f g)))
         (class/c (absent (field f))))
  (ctest #f contract-equivalent?
         (class/c (absent (field f)))
         (class/c (absent (field f g))))
  (ctest #f contract-equivalent?
         (class/c (absent (field x)))
         (class/c (absent x)))
  (ctest #f contract-equivalent?
         (class/c (absent x))
         (class/c (absent (field x))))
  
  (ctest #t contract-equivalent?
         (instanceof/c (class/c (m (-> any/c (<=/c 3)))))
         (instanceof/c (class/c (m (-> any/c (<=/c 3))))))
  (ctest #f contract-equivalent?
         (instanceof/c (class/c (m (-> any/c (<=/c 3)))))
         (instanceof/c (class/c (m (-> any/c (<=/c 4))))))
  (ctest #f contract-equivalent?
         (instanceof/c (class/c (m (-> any/c (<=/c 4)))))
         (instanceof/c (class/c (m (-> any/c (<=/c 3))))))

  (ctest #t contract-equivalent?
         (object/c (m (-> any/c (<=/c 3))))
         (object/c (m (-> any/c (<=/c 3)))))
  (ctest #f contract-equivalent?
         (object/c (m (-> any/c (<=/c 3))))
         (object/c (m (-> any/c (<=/c 4)))))
  (ctest #t contract-equivalent?
         (object/c (field (f (<=/c 4))))
         (object/c (field (f (<=/c 4)))))
  (ctest #f contract-equivalent?
         (object/c (m (-> any/c (<=/c 3)))
                   (n (-> any/c any/c)))
         (object/c (m (-> any/c (<=/c 4)))))
  (ctest #f contract-equivalent?
         (object/c (m (-> any/c (<=/c 4))))
         (object/c (m (-> any/c (<=/c 3)))))
  (ctest #f contract-equivalent?
         (object/c (field (f (<=/c 4))))
         (object/c (field (f (<=/c 3)))))
  (ctest #f contract-equivalent?
         (object/c (m (-> any/c (<=/c 3))))
         (object/c (n (-> any/c (<=/c 4)))))
  (ctest #f contract-equivalent?
         (object/c (field (x any/c)))
         (object/c (field (y any/c))))
  (ctest #f contract-equivalent?
         (object/c (m (-> any/c (<=/c 4))))
         (object/c (m (-> any/c (<=/c 3)))
                   (n (-> any/c any/c))))
  
  (ctest #t contract-equivalent? (is-a?/c object%) (is-a?/c object%))
  (ctest #f contract-equivalent? (is-a?/c (class object% (super-new))) (is-a?/c object%))
  (ctest #f contract-equivalent? (is-a?/c object%) (is-a?/c (class object% (super-new))))
  (contract-eval `(define one-interface<%> (interface ())))
  (contract-eval `(define another-interface<%> (interface (one-interface<%>))))
  (ctest #t contract-equivalent? (is-a?/c one-interface<%>) (is-a?/c one-interface<%>))
  (ctest #f contract-equivalent? (is-a?/c another-interface<%>) (is-a?/c one-interface<%>))
  (ctest #f contract-equivalent? (is-a?/c one-interface<%>) (is-a?/c another-interface<%>))
  (ctest #f contract-equivalent? 
         (is-a?/c (class* object% (one-interface<%>) (super-new)))
         (is-a?/c one-interface<%>))
  (ctest #f contract-equivalent?
         (is-a?/c one-interface<%>) 
         (is-a?/c (class* object% (one-interface<%>) (super-new))))
  
  (ctest #t contract-equivalent? (subclass?/c object%) (subclass?/c object%))
  (ctest #f contract-equivalent? (subclass?/c (class object% (super-new))) (subclass?/c object%))
  (ctest #f contract-equivalent? (subclass?/c object%) (subclass?/c (class object% (super-new))))
  (ctest #t contract-equivalent?
         (implementation?/c one-interface<%>)
         (implementation?/c one-interface<%>))
  (ctest #f contract-equivalent?
         (implementation?/c another-interface<%>)
         (implementation?/c one-interface<%>))
  (ctest #f contract-equivalent?
         (implementation?/c one-interface<%>)
         (implementation?/c another-interface<%>))

  (ctest #t contract-equivalent? (evt/c integer?) (evt/c integer?))
  (ctest #f contract-equivalent? (evt/c integer?) (evt/c boolean?))
  
  ;; chances are, this predicate will accept "x", but
  ;; we don't want to consider it stronger, since it 
  ;; will not always accept "x".
  (ctest #f contract-equivalent? "x" (λ (x) (not (zero? (random 10000)))))

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
         [tl (hd) (if (< (random n) -1) none/c any/c)]))

      (,test #:test-case-name 'dsc.1 #t contract-equivalent? (couple/c any/c any/c) (couple/c any/c any/c))
      (,test #:test-case-name 'dsc.2 #f contract-equivalent? (couple/c (>=/c 2) (>=/c 3)) (couple/c (>=/c 4) (>=/c 5)))
      (,test #:test-case-name 'dsc.3 #f contract-equivalent? (couple/c (>=/c 4) (>=/c 5)) (couple/c (>=/c 2) (>=/c 3)))
      (,test #:test-case-name 'dsc.4 #f contract-equivalent? (couple/c (>=/c 1) (>=/c 5)) (couple/c (>=/c 5) (>=/c 1)))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) any/c])])
        (,test #:test-case-name 'dsc.5 #t contract-equivalent? ctc ctc))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) (<=/c hd)])])
        (,test #:test-case-name 'dsc.6 #t contract-equivalent? ctc ctc))
      (,test #:test-case-name 'dsc.7 #t contract-equivalent? list-of-numbers list-of-numbers)
      (,test #:test-case-name 'dsc.8 #t contract-equivalent? (short-list/less-than 4) (short-list/less-than 4))
      (,test #:test-case-name 'dsc.9 #f contract-equivalent? (short-list/less-than 4) (short-list/less-than 5))
      (,test #:test-case-name 'dsc.10 #f contract-equivalent? (short-list/less-than 5) (short-list/less-than 4))
      (,test #:test-case-name 'dsc.11 #t contract-equivalent? (short-sorted-list/less-than 4) (short-sorted-list/less-than 4))
      (,test #:test-case-name 'dsc.12 #f contract-equivalent? (short-sorted-list/less-than 4) (short-sorted-list/less-than 5))
      (,test #:test-case-name 'dsc.13 #f contract-equivalent? (short-sorted-list/less-than 5) (short-sorted-list/less-than 4))
      (,test #:test-case-name 'dsc.14 #t contract-equivalent? (sorted-list/less-than 4) (sorted-list/less-than 4))
      (,test #:test-case-name 'dsc.15 #f contract-equivalent? (sorted-list/less-than 4) (sorted-list/less-than 5))
      (,test #:test-case-name 'dsc.16 #f contract-equivalent? (sorted-list/less-than 5) (sorted-list/less-than 4))
      (,test #:test-case-name 'dsc.17 #t contract-equivalent? (closure-comparison-test 4) (closure-comparison-test 4))
      (,test #:test-case-name 'dsc.18 #f contract-equivalent? (closure-comparison-test 4) (closure-comparison-test 5))

      (letrec ([mk-c
                (λ (x)
                  (triple/dc [a (<=/c x)]
                             [b any/c]
                             [c (a b) (or/c #f (mk-c a))]))])
        (,test #:test-case-name 'dsc.19 #t contract-equivalent? (mk-c 1) (mk-c 1)))
      (letrec ([mk-c
                (λ (x)
                  (triple/dc [a (<=/c x)]
                             [b any/c]
                             [c (a b) (or/c #f (mk-c a))]))])
        (,test #:test-case-name 'dsc.20 #f contract-equivalent? (mk-c 1) (mk-c 2)))))

  (contract-eval
   `(let ()
      (define (non-zero? x) (not (zero? x)))
      (define list-of-numbers
        (first-or/c null?
                    (couple/c number?
                              (recursive-contract list-of-numbers))))
      (define (short-list/less-than n)
        (first-or/c null?
                    (couple/c (<=/c n)
                              (first-or/c null?
                                          (couple/c (<=/c n)
                                                    any/c)))))
      (define (short-sorted-list/less-than n)
        (first-or/c null?
                    (couple/dc
                     [hd (<=/c n)]
                     [tl (hd) (first-or/c null?
                                          (couple/c (<=/c hd)
                                                    any/c))])))

      (define (sorted-list/less-than n)
        (first-or/c null?
                    (couple/dc
                     [hd (<=/c n)]
                     [tl (hd) (sorted-list/less-than hd)])))

      (define (closure-comparison-test n)
        (couple/dc
         [hd any/c]
         [tl (hd) (if (< (random 10) (- n)) none/c  any/c)]))

      (,test #:test-case-name 'couple.1
             #t contract-equivalent? (couple/c any/c any/c) (couple/c any/c any/c))
      (,test #:test-case-name 'couple.2
             #f contract-equivalent? (couple/c (>=/c 2) (>=/c 3)) (couple/c (>=/c 4) (>=/c 5)))
      (,test #:test-case-name 'couple.3
             #f contract-equivalent? (couple/c (>=/c 4) (>=/c 5)) (couple/c (>=/c 2) (>=/c 3)))
      (,test #:test-case-name 'couple.4
             #f contract-equivalent? (couple/c (>=/c 1) (>=/c 5)) (couple/c (>=/c 5) (>=/c 1)))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) any/c])])
        (,test #:test-case-name 'couple.5 #t contract-equivalent? ctc ctc))
      (let ([ctc (couple/dc [hd any/c] [tl (hd) (<=/c hd)])])
        (,test #:test-case-name 'couple.6 #t contract-equivalent? ctc ctc))
      (,test #:test-case-name 'couple.7 #t contract-equivalent? list-of-numbers list-of-numbers)
      (,test #:test-case-name 'couple.8 #f contract-equivalent? (short-list/less-than 4) (short-list/less-than 5))
      (,test #:test-case-name 'couple.9 #f contract-equivalent? (short-list/less-than 5) (short-list/less-than 4))
      (,test #:test-case-name 'couple.10 #t contract-equivalent? (short-sorted-list/less-than 4) (short-sorted-list/less-than 4))
      (,test #:test-case-name 'couple.11 #f contract-equivalent? (short-sorted-list/less-than 4) (short-sorted-list/less-than 5))
      (,test #:test-case-name 'couple.12 #f contract-equivalent? (short-sorted-list/less-than 5) (short-sorted-list/less-than 4))
      (,test #:test-case-name 'couple.13 #t contract-equivalent? (sorted-list/less-than 4) (sorted-list/less-than 4))
      (,test #:test-case-name 'couple.14 #f contract-equivalent? (sorted-list/less-than 4) (sorted-list/less-than 5))
      (,test #:test-case-name 'couple.15 #f contract-equivalent? (sorted-list/less-than 5) (sorted-list/less-than 4))
      (,test #:test-case-name 'couple.16 #t contract-equivalent? (closure-comparison-test 4) (closure-comparison-test 4))
      (,test #:test-case-name 'couple.17 #f contract-equivalent? (closure-comparison-test 4) (closure-comparison-test 5))

      (letrec ([mk-c
                (λ (x)
                  (triple/dc [a (<=/c x)]
                             [b any/c]
                             [c (a b) (or/c #f (mk-c a))]))])
        (,test #:test-case-name 'couple.18 #t contract-equivalent? (mk-c 1) (mk-c 1)))

      (letrec ([mk-c
                (λ (x)
                  (triple/dc [a (<=/c x)]
                             [b any/c]
                             [c (a b) (or/c #f (mk-c a))]))])
        (,test #:test-case-name 'couple.19 #f contract-equivalent? (mk-c 1) (mk-c 2)))))


  (contract-eval
   `(let ()

      (struct s (a b))
      (struct t (a b))

      (,test #:test-case-name 'struct/dc.1 #f contract-equivalent?
             (struct/dc s
                        [a (>=/c 1)]
                        [b (>=/c 2)])
             (struct/dc s
                        [a (>=/c 2)]
                        [b (>=/c 3)]))
      (,test #:test-case-name 'struct/dc.2 #t contract-equivalent?
             (struct/dc s
                        [a (>=/c 2)]
                        [b (>=/c 3)])
             (struct/dc s
                        [a (>=/c 2)]
                        [b (>=/c 3)]))
      (,test #:test-case-name 'struct/dc.3 #f contract-equivalent?
             (struct/dc s
                        [a (>=/c 2)]
                        [b (>=/c 3)])
             (struct/dc s
                        [a (>=/c 1)]
                        [b (>=/c 2)]))

      (,test #:test-case-name 'struct/dc.4 #f contract-equivalent?
             (struct/dc s
                        [a number?]
                        [b number?])
             (struct/dc t
                        [a number?]
                        [b number?]))

      (,test #:test-case-name 'struct/dc.5 #f contract-equivalent?
             (struct/dc t
                        [a number?]
                        [b number?])
             (struct/dc s
                        [a number?]
                        [b number?]))
      
      (,test #:test-case-name 'struct/dc.6 #f
             contract-equivalent?
             (struct/dc s
                        [a integer?]
                        [b integer?])
             (struct/dc s
                        [a integer?]
                        [b integer?]
                        #:inv (a b) #f))
      
      (,test #:test-case-name 'struct/dc.7 #f
             contract-equivalent?
             (struct/dc s
                        [a integer?]
                        [b integer?]
                        #:inv (a b) #f)
             (struct/dc s
                        [a integer?]
                        [b integer?]))
      

      (define (mk c)
        (struct/dc s
                   [a (>=/c c)]
                   [b (a) (>=/c a)]))
      (define one (mk 1))
      (define two (mk 2))
      (,test #:test-case-name 'struct/dc.8 #t contract-equivalent? one one)
      (,test #:test-case-name 'struct/dc.9 #f contract-equivalent? one two)
      (,test #:test-case-name 'struct/dc.10 #f contract-equivalent? two one)))

  (contract-eval
   `(define imp-ctc
      (make-contract
       #:late-neg-projection (λ (blame) (λ (val neg) (add1 val))))))
  (contract-eval
   `(define imp-struct-ctc
      (let ()
        (struct imp-ctc-struct ()
          #:property prop:contract
          (build-contract-property
           #:late-neg-projection
           (λ (ctc)
             (λ (blame)
               (λ (val neg)
                 (add1 val))))))
        (imp-ctc-struct))))

  (ctest #f contract-equivalent? imp-ctc imp-ctc)
  (ctest #f contract-equivalent? imp-struct-ctc imp-struct-ctc))
