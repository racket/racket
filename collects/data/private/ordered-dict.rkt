#lang racket/base
(require racket/dict
         racket/contract/base
         unstable/prop-contract)

(define-values (prop:ordered-dict ordered-dict? ordered-dict-ref)
  (make-struct-type-property 'ordered-dict #f))

(define extreme-contract
  (->i ([d ordered-dict?])
       [_ (d) (or/c #f (dict-iter-contract d))]))

(define search-contract
  (->i ([d ordered-dict?]
        [k (d) (dict-key-contract d)])
       [_ (d) (or/c #f (dict-iter-contract d))]))

(define prop:ordered-dict-contract
  (let ([e extreme-contract]
        [s search-contract])
    (vector-immutable/c e   ;; iterate-min
                        e   ;; iterate-max
                        s   ;; iterate-least/>?
                        s   ;; iterate-least/>=?
                        s   ;; iterate-greatest/<?
                        s)));; iterate-greatest/<=?

;; --------

(define-syntax-rule (appd d offset arg ...)
  (let ([dv d])
    ((vector-ref (ordered-dict-ref dv) offset) dv arg ...)))

(define (dict-iterate-min d)
  (appd d 0))
(define (dict-iterate-max d)
  (appd d 1))
(define (dict-iterate-least/>? d k)
  (appd d 2 k))
(define (dict-iterate-least/>=? d k)
  (appd d 3 k))
(define (dict-iterate-greatest/<? d k)
  (appd d 4 k))
(define (dict-iterate-greatest/<=? d k)
  (appd d 5 k))

(provide/contract
 [prop:ordered-dict
  (struct-type-property/c prop:ordered-dict-contract)]
 [ordered-dict? (-> any/c boolean?)]
 [dict-iterate-min extreme-contract]
 [dict-iterate-max extreme-contract]
 [dict-iterate-least/>? search-contract]
 [dict-iterate-least/>=? search-contract]
 [dict-iterate-greatest/<? search-contract]
 [dict-iterate-greatest/<=? search-contract])
