#lang racket/base
(require racket/contract
         racket/dict
         "private/ordered-dict.rkt")
;; owned by ryanc

#|
reference
  Skip Lists: A Probabilistic Alternative to Balanced Trees
  by William Pugh

I take the "fix the dice" approach to avoiding level jumps.
Levels are indexed starting at 1, as in the paper.
|#

#|
(require (rename-in racket/unsafe/ops
                    [unsafe-vector-length vector-length]
                    [unsafe-vector-ref vector-ref]
                    [unsafe-vector-set! vector-set!]))
|#

(define PROBABILITY-FACTOR 4)
(define MAX-LEVEL 16)

(define DATA-SLOTS 2)

;; An Item is
;;  - (vector key data Item/#f Item/#f ...)

;; The Level of an Item is the number of next links it has (at least 1).
;; The head is an Item with key and data #f (never examined)
;; The end of the list is represented by #f

(define (item? x) (vector? x))
(define (item-level item)
  (- (vector-length item) DATA-SLOTS))

(define (item-key item)
  (vector-ref item 0))
(define (item-data item)
  (vector-ref item 1))
(define (item-next item level)
  (vector-ref item (+ (+ level DATA-SLOTS) -1)))

(define (set-item-key! item key)
  (vector-set! item 0 key))
(define (set-item-data! item data)
  (vector-set! item 1 data))
(define (set-item-next! item level next)
  (vector-set! item (+ (+ level DATA-SLOTS) -1) next))

(define (resize-item item level)
  (define new-size (+ DATA-SLOTS level))
  (define new-item (make-vector new-size #f))
  (vector-copy! new-item 0 item 0 (min (vector-length item) new-size))
  new-item)

;; search : Item Nat Key Cmp Cmp -> Item/#f
;; Returns item(R) s.t. key(R) =? key
(define (search head level key =? <?)
  (let* ([closest (closest head level key <?)]
         [item (item-next closest 1)])
    (and (item? item)
         (=? key (item-key item))
         item)))

;; closest : Item Nat Key Cmp Cmp -> Item
;; Returns greatest item R s.t. key(R) <? key.
;; Pre: level(item) >= level, key <? key(item) OR item = head
(define (closest item level key <?)
  (if (zero? level)
      item
      (closest (advance item level key <?) (sub1 level) key <?)))

;; advance : Item Nat Key Cmp -> Item
;; Returns greatest item R s.t. key(R) <? key and level(R) >= level.
;; Pre: level(item) >= level, key <? key(item) OR item = head
(define (advance item level key <?)
  (let ([next (item-next item level)])
    (if (and next (<? (item-key next) key))
        (advance next level key <?)
        item)))

;; pick-random-level : Nat -> Nat
;; Returns number in [1, max] (with exp. prob. dist.)
(define (pick-random-level max)
  (let loop ([level 1])
    (if (and (< level max) (zero? (random PROBABILITY-FACTOR)))
        (loop (add1 level))
        level)))

;; update/insert : ... -> Item/#f
;; Updates skip-list so that key |-> data
;; Returns #f to indicate update (existing item changed);
;; returns item to indicate insertion (context's links need updating)
;; Pre: level(item) >= level, key <? key(item) OR item = head
(define (update/insert item level key data =? <? max-level)
  (cond [(positive? level)
         (let* ([item (advance item level key <?)]
                [result (update/insert item (sub1 level)
                                       key data =? <? max-level)])
           (when (and result (>= (item-level result) level))
             (let ([link (item-next item level)])
               (set-item-next! item level result)
               (set-item-next! result level link)))
           result)]
        [else
         (let ([next (item-next item 1)])
           (cond [(and next (=? (item-key next) key))
                  ;; Update!
                  (set-item-data! next data)
                  #f]
                 [else
                  ;; Insert!
                  (let ([new-item
                         (make-vector (+ DATA-SLOTS (pick-random-level max-level)) #f)])
                    (set-item-key! new-item key)
                    (set-item-data! new-item data)
                    new-item)]))]))

;; delete : ... -> Item/#f
;; Returns item to indicate deletion (context's links need updating);
;; returns #f if not found.
;; Pre: level(item) >= level; key <? key(item) OR item = head
(define (delete item level key =? <?)
  (cond [(positive? level)
         (let* ([item (advance item level key <?)]
                [result (delete item (sub1 level) key =? <?)])
           (when (and result (eq? (item-next item level) result))
             (let ([link (item-next result level)])
               (set-item-next! item level link)
               (set-item-next! result level #f)))
           result)]
        [else
         (let ([next (item-next item 1)])
           (cond [(and next (=? (item-key next) key))
                  ;; Delete!
                  next]
                 [else
                  ;; Not found!
                  #f]))]))


;; Skip list

(define (make-skip-list =? <?)
  (skip-list (vector 'head 'head #f) 0 =? <?))

(define (skip-list-ref s key [default (skip-list-error key)])
  (define head (skip-list-head s))
  (define result
    (search head (item-level head) key (skip-list-=? s) (skip-list-<? s)))
  (cond [result (item-data result)]
        [(procedure? default) (default)]
        [else default]))

(define ((skip-list-error x))
  (error 'skip-list-ref "no mapping found for: ~e" x))

(define (skip-list-set! s key data)
  (define head (skip-list-head s))
  (define =? (skip-list-=? s))
  (define <? (skip-list-<? s))
  (define max-level (max MAX-LEVEL (add1 (item-level head))))
  (define result ;; new Item or #f
    (update/insert head (item-level head) key data =? <? max-level))
  (when result
    (set-skip-list-num-entries! s (add1 (skip-list-count s)))
    (when (> (item-level result) (item-level head))
      (let ([new-head (resize-item head (item-level result))])
        (set-item-next! new-head (item-level result) result)
        (set-skip-list-head! s new-head)))))

(define (skip-list-remove! s key)
  (define head (skip-list-head s))
  (define =? (skip-list-=? s))
  (define <? (skip-list-<? s))
  (define deleted
    (delete head (item-level head) key =? <?))
  (when deleted
    (set-skip-list-num-entries! s (sub1 (skip-list-count s))))
  (unless (or (item? (item-next head (item-level head)))
              (= 1 (item-level head)))
    ;; Trim head
    (let ([new-head (resize-item head (sub1 (item-level head)))])
      (set-skip-list-head! s new-head))))

;; Dict methods

(define (skip-list-count s) (skip-list-num-entries s))

(struct skip-list-iter (s item))

(define (check-iter who s iter)
  (unless (skip-list-iter? iter)
    (raise-type-error who "skip-list-iter" iter))
  (unless (eq? (skip-list-iter-s iter) s)
    (raise-mismatch-error who "skip-list-iter does not match skip-list" iter)))

(define (skip-list-iterate-first s)
  (let ([next (item-next (skip-list-head s) 1)])
    (and next (skip-list-iter s next))))

(define (skip-list-iterate-next s iter)
  (check-iter 'skip-list-iterate-next s iter)
  (let ([next (item-next (skip-list-iter-item iter) 1)])
    (and next (skip-list-iter s next))))

(define (skip-list-iterate-key s iter)
  (check-iter 'skip-list-iterate-key s iter)
  (item-key (skip-list-iter-item iter)))

(define (skip-list-iterate-value s iter)
  (check-iter 'skip-list-iterate-key s iter)
  (item-data (skip-list-iter-item iter)))

;; Extensions

;; Returns greatest/rightmost item s.t. key(item) < key
(define (skip-list-iterate-greatest/<? s key)
  (let* ([head (skip-list-head s)]
         [<? (skip-list-<? s)]
         [item (closest head (item-level head) key <?)])
    (and (not (eq? item head)) (skip-list-iter s item))))

;; Returns greatest/rightmost item s.t. key(item) <= key
(define (skip-list-iterate-greatest/<=? s key)
  (let* ([head (skip-list-head s)]
         [<? (skip-list-<? s)]
         [=? (skip-list-=? s)]
         [item< (closest head (item-level head) key <?)]
         [item1 (item-next item< 1)])
    (cond [(and item1 (=? (item-key item1) key))
           (skip-list-iter s item1)]
          [(eq? item< head)
           #f]
          [else
           (skip-list-iter s item<)])))

;; Returns least/leftmost item s.t. key(item) > key
(define (skip-list-iterate-least/>? s key)
  (let* ([head (skip-list-head s)]
         [<? (skip-list-<? s)]
         [item< (closest head (item-level head) key <?)]
         ;; If head, nudge forward one so comparisons are valid.
         [item< (if (eq? item< head) (item-next item< 1) item<)])
    (let loop ([item item<])
      (and item
           (if (<? key (item-key item))
               (skip-list-iter s item)
               (loop (item-next item 1)))))))

;; Returns least/leftmost item s.t. key(item) >= key
(define (skip-list-iterate-least/>=? s key)
  (let* ([head (skip-list-head s)]
         [<? (skip-list-<? s)]
         [item (closest head (item-level head) key <?)]
         [item (item-next item 1)])
    (and item (skip-list-iter s item))))

(define (skip-list-iterate-min s)
  (let* ([head (skip-list-head s)]
         [item (item-next head 1)])
    (and item (skip-list-iter s item))))

(define (skip-list-iterate-max s)
  (let* ([head (skip-list-head s)]
         [item (closest head (item-level head)
                        ;; replace standard comparison with "always <",
                        ;; so closest yields max item
                        'unused
                        (lambda (x y) #t))])
    (and item (skip-list-iter s item))))

(define (skip-list-iterate-set-key! s iter key)
  (check-iter 'skip-list-iterate-set-key! s iter)
  (set-item-key! (skip-list-iter-item iter) key))

(define (skip-list-iterate-set-value! s iter value)
  (check-iter 'skip-list-iterate-set-value! s iter)
  (set-item-data! (skip-list-iter-item iter) value))

(struct skip-list ([head #:mutable] [num-entries #:mutable] =? <?)
  #:property prop:dict/contract
             (list (vector-immutable skip-list-ref
                                     skip-list-set!
                                     #f ;; set
                                     skip-list-remove!
                                     #f ;; remove
                                     skip-list-count
                                     skip-list-iterate-first
                                     skip-list-iterate-next
                                     skip-list-iterate-key
                                     skip-list-iterate-value)
                   (vector-immutable any/c any/c skip-list-iter?
                                     #f #f #f))
  #:property prop:ordered-dict
             (vector-immutable skip-list-iterate-min
                               skip-list-iterate-max
                               skip-list-iterate-least/>?
                               skip-list-iterate-least/>=?
                               skip-list-iterate-greatest/<?
                               skip-list-iterate-greatest/<=?))

(provide/contract
 [make-skip-list
  (-> procedure? procedure? skip-list?)]
 [skip-list?
  (-> any/c boolean?)]
 [skip-list-ref
  (->* (skip-list? any/c) (any/c) any)]
 [skip-list-set!
  (-> skip-list? any/c any/c void?)]
 [skip-list-remove!
  (-> skip-list? any/c void?)]
 [skip-list-count
  (-> skip-list? exact-nonnegative-integer?)]
 [skip-list-iterate-first
  (-> skip-list? (or/c skip-list-iter? #f))]
 [skip-list-iterate-next
  (-> skip-list? skip-list-iter? (or/c skip-list-iter? #f))]
 [skip-list-iterate-key
  (-> skip-list? skip-list-iter? any)]
 [skip-list-iterate-value
  (-> skip-list? skip-list-iter? any)]

 [skip-list-iterate-greatest/<?
  (-> skip-list? any/c (or/c skip-list-iter? #f))]
 [skip-list-iterate-greatest/<=?
  (-> skip-list? any/c (or/c skip-list-iter? #f))]
 [skip-list-iterate-least/>?
  (-> skip-list? any/c (or/c skip-list-iter? #f))]
 [skip-list-iterate-least/>=?
  (-> skip-list? any/c (or/c skip-list-iter? #f))]

 [skip-list-iterate-min
  (-> skip-list? (or/c skip-list-iter? #f))]
 [skip-list-iterate-max
  (-> skip-list? (or/c skip-list-iter? #f))]

 [skip-list-iterate-set-key!
  (-> skip-list? skip-list-iter? any/c any)]
 [skip-list-iterate-set-value!
  (-> skip-list? skip-list-iter? any/c any)]

 [skip-list-iter?
  (-> any/c any)])

#|
;; Testing

(define s (make-skip-list* = <))
s
(dict-map s list)
(skip-list-set! s 1 'apple)
(skip-list-set! s 3 'pear)
(skip-list-set! s 2 'orange)
(dict-map s list)

(define h
  (time
   (for/hash ([n (in-range 1 50000)])
     (values (random 1000) n))))

(define s2 (make-skip-list* = <))
(time
 (for ([n (in-range 1 50000)])
   (skip-list-set! s2 (random 1000) n)))

(define d (make-skip-list* = <))
(time
 (for ([n (in-range 1 50000)])
   (dict-set! d (random 1000) n)))


(define (find-a-bunch t)
  (for ([n (in-range 1 10000)])
    (dict-ref t (random 1000) #f)))

(display "\nlookup 10000 times\n")
;(time (find-a-bunch h))
(time (find-a-bunch s2))
|#
