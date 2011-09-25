#lang racket/base
(require racket/contract/base)

;; Eli: See comment for `dict-union' and `dict-union!' -- these two do
;; make sense, but if they're in, then generalizing them to dictionaries
;; seems to make little sense.  If they are useful, I'd much rather see
;; a more direct connection -- for example, make the dict functions
;; convert all dicts to hashes and then union them -- this will also
;; make the performance cost more obvious (and will actually be faster
;; in most cases of non-hash dictionaries).

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
                   void?)])
