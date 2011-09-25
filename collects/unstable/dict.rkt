#lang racket/base
(require racket/dict
         racket/contract/base)

(define (dict-empty? dict)
  (not (dict-iterate-first dict)))
;; Eli: This encourages ignoring the actual representation, and the fact
;; that `dict-count' can in some cases be an O(N) operation.  (And to
;; make things worse, it's not even mentioned in the docs.)
;; Ryan: Fixed complexity.

(define ((dict-duplicate-error name) key value1 value2)
  (error name "duplicate values for key ~e: ~e and ~e" key value1 value2))

;; Eli: If this is useful, then at least make it worth using instead of
;; writing your own code.  For example, inspect the arguments and choose
;; an efficient order for the loops, or use a temporary hash table for a
;; union of two alists, etc.  Alternatively, just write a function for
;; merging two hash tables (and call it `hash-union', of course).

;; Ryan: I prefer the names dict-add-all and dict-add-all!---no connotations
;; of symmetry, and it makes it clear that the first argument determines the
;; representation (and key constraints, etc).

(define (dict-union
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (dict-duplicate-error 'dict-union))]
         one . rest)
  (for*/fold ([one one]) ([two (in-list rest)] [(k v) (in-dict two)])
    (dict-set one k (if (dict-has-key? one k)
                        (combine/key k (dict-ref one k) v)
                        v))))

(define (dict-union!
         #:combine [combine #f]
         #:combine/key [combine/key
                        (if combine
                          (lambda (k x y) (combine x y))
                          (dict-duplicate-error 'dict-union))]
         one . rest)
  (for* ([two (in-list rest)] [(k v) (in-dict two)])
    (dict-set! one k (if (dict-has-key? one k)
                         (combine/key k (dict-ref one k) v)
                         v))))

(provide/contract
 [dict-empty? (-> dict? boolean?)]
 [dict-union (->* [(and/c dict? dict-can-functional-set?)]
                  [#:combine
                   (-> any/c any/c any/c)
                   #:combine/key
                   (-> any/c any/c any/c any/c)]
                  #:rest (listof dict?)
                  (and/c dict? dict-can-functional-set?))]
 [dict-union! (->* [(and/c dict? dict-mutable?)]
                   [#:combine
                   (-> any/c any/c any/c)
                   #:combine/key
                   (-> any/c any/c any/c any/c)]
                   #:rest (listof dict?)
                   void?)])
