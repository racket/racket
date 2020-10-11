#lang racket/base
(require racket/contract/base)

(define ((hash-duplicate-error name) key value1 value2)
  (error name "duplicate values for key ~e: ~e and ~e" key value1 value2))

(define (hash-union
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (hash-duplicate-error 'hash-union))]
         one . rest)
  (for*/fold ([one one]) ([two (in-list rest)] [(k v) (in-hash two)])
    (hash-set one k (if (hash-has-key? one k)
                        (combine/key k (hash-ref one k) v)
                        v))))

(define (hash-union!
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (hash-duplicate-error 'hash-union))]
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
         . hashes)
  (define one (car hashes))
  (define rest (cdr hashes))
  (define empty-h (hash-clear one)) ;; empty hash of same type as one
  (define (argmin f lst) ;; avoid racket/list to improve loading time
    (for/fold ([best (car lst)] [fbest (f (car lst))]
               #:result best)
              ([x lst])
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
                       (and/c hash? immutable?))])
