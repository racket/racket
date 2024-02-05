#lang racket/base
(require racket/contract/base racket/match)

(define ((hash-duplicate-error name) key value1 value2)
  (error name "duplicate values for key ~e: ~e and ~e" key value1 value2))

(define (merge one two combine/key)
  (for/fold ([one one]) ([(k v) (in-hash two)])
    (hash-set one k (if (hash-has-key? one k)
                        (combine/key k (hash-ref one k) v)
                        v))))

(define (hash-union
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                            (lambda (_ x y) (combine x y))
                            (hash-duplicate-error 'hash-union))]
         one . rest)
  (define one-empty (hash-clear one))
  (for/fold ([one one]) ([two (in-list rest)])
    (cond
      [(and (immutable? two)
            (< (hash-count one) (hash-count two))
            (equal? one-empty (hash-clear two)))
       (merge two one (λ (k a b) (combine/key k b a)))]
      [else (merge one two combine/key)])))

(define (hash-union!
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                            (lambda (_ x y) (combine x y))
                            (hash-duplicate-error 'hash-union!))]
         one . rest)
  (for* ([two (in-list rest)] [(k v) (in-hash two)])
    (hash-set! one k (if (hash-has-key? one k)
                         (combine/key k (hash-ref one k) v)
                         v))))

(define (hash-intersect
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                            (λ (_ x y) (combine x y))
                            (hash-duplicate-error 'hash-intersect))]
         one . rest)
  (define hashes (cons one rest))
  (define empty-h (hash-clear one)) ;; empty hash of same type as one
  (define (argmin f lst) ;; avoid racket/list to improve loading time
    (for/fold ([best (car lst)]
               [fbest (f (car lst))]
               #:result best)
              ([x (in-list lst)])
      (define fx (f x))
      (if (< fx fbest) (values x fx) (values best fbest))))
  (for/fold ([res empty-h])
            ([k (in-hash-keys (argmin hash-count hashes))])
    (if (for/and ([h (in-list hashes)]) (hash-has-key? h k))
        (hash-set res k
                  (for/fold ([v (hash-ref one k)])
                            ([hm (in-list rest)])
                    (combine/key k v (hash-ref hm k))))
        res)))

(define (hash-filter ht pred)
  (define constructor
    (match (map (λ (f) (f ht)) (list immutable? hash-ephemeron? hash-weak? hash-eq? hash-eqv? hash-equal-always?))
      ;; Ephemerons
      [(list _ #t _ #t _ _) make-ephemeron-hasheq]
      [(list _ #t _ _ #t _) make-ephemeron-hasheqv]
      [(list _ #t _ _ _ #t) make-ephemeron-hashalw]
      [(list _ #t _ _ _ _) make-ephemeron-hash]
      ;; Weak Hashes
      [(list _ _ #t #t _ _) make-weak-hasheq]
      [(list _ _ #t _ #t _) make-weak-hasheqv]
      [(list _ _ #t _ _ #t) make-weak-hashalw]
      [(list _ _ #t _ _ _) make-weak-hash]
      ;; Immutable Hashes
      [(list #t _ _ #t _ _) make-immutable-hasheq]
      [(list #t _ _ _ #t _) make-immutable-hasheqv]
      [(list #t _ _ _ _ #t) make-immutable-hashalw]
      [(list #t _ _ _ _ _) make-immutable-hash]
      ;; Mutable Hashes
      [(list _ _ _ #t _ _) make-hasheq]
      [(list _ _ _ _ #t _) make-hasheqv]
      [(list _ _ _ _ _ #t) make-hashalw]
      ;; mutable hash with equal? comparator
      [_ make-hash]))
  (constructor (for/list ([(k v) (in-hash ht)] #:when (pred k v)) (cons k v))))

(define (hash-filter-keys ht pred)
  (hash-filter ht (lambda (k _) (pred k))))

(define (hash-filter-values ht pred)
  (hash-filter ht (lambda (_ v) (pred v))))

(provide/contract
 [hash-union (->* [(and/c hash? immutable?)]
                  [#:combine
                   (-> any/c any/c any/c)
                   #:combine/key
                   (-> any/c any/c any/c any/c)]
                  #:rest (listof hash?)
                  (and/c hash? immutable?))]
 [hash-union! (->* [(and/c hash? (not/c immutable?))]
                   [#:combine
                    (-> any/c any/c any/c)
                    #:combine/key
                    (-> any/c any/c any/c any/c)]
                   #:rest (listof hash?)
                   void?)]
 [hash-intersect  (->* [(and/c hash? immutable?)]
                       [#:combine
                        (-> any/c any/c any/c)
                        #:combine/key
                        (-> any/c any/c any/c any/c)]
                       #:rest (listof hash?)
                       (and/c hash? immutable?))]
 [hash-filter (-> hash? procedure? hash?)]
 [hash-filter-values (-> hash? procedure? hash?)]
 [hash-filter-keys (-> hash? procedure? hash?)])
