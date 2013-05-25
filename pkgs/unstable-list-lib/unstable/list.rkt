#lang racket/base
(require racket/contract/base
         racket/dict
         (for-syntax racket/base))

; list-prefix? : list? list? -> boolean?
; Is l a prefix or r?
(define (list-prefix? ls rs)
  (or (null? ls)
      (and (pair? rs)
           (equal? (car ls) (car rs))
           (list-prefix? (cdr ls) (cdr rs)))))

;; Eli: How about a version that removes the equal prefix from two lists
;; and returns the tails -- this way you can tell if they're equal, or
;; one is a prefix of the other, or if there was any equal prefix at
;; all.  (Which can be useful for things like making a path relative to
;; another path.)  A nice generalization is to make it get two or more
;; lists, and return a matching number of values.

(define (internal-split-common-prefix as bs same? keep-prefix?)
  (let loop ([as as] [bs bs])
    (if (and (pair? as) (pair? bs) (same? (car as) (car bs)))
        (let-values ([(prefix atail btail) (loop (cdr as) (cdr bs))])
          (values (and keep-prefix? (cons (car as) prefix)) atail btail))
        (values null as bs))))

(define (split-common-prefix as bs #:same? [same? equal?])
  (internal-split-common-prefix as bs same? #t))

(define (take-common-prefix as bs #:same? [same? equal?])
  (let-values ([(prefix atail btail) (internal-split-common-prefix as bs same? #t)])
    prefix))

(define (drop-common-prefix as bs #:same? [same? equal?])
  (let-values ([(prefix atail btail) (internal-split-common-prefix as bs same? #f)])
    (values atail btail)))

(provide/contract
 [list-prefix? (list? list? . -> . boolean?)]
 [split-common-prefix
  (->* (any/c any/c) (#:same? procedure?) (values list? any/c any/c))]
 [take-common-prefix
  (->* (any/c any/c) (#:same? procedure?) list?)]
 [drop-common-prefix
  (->* (any/c any/c) (#:same? procedure?) (values any/c any/c))])


(define (filter-multiple l . fs)
  (apply values
         (map (lambda (f) (filter f l)) fs)))

;; Listof[A] Listof[B] B -> Listof[B]
;; pads out t to be as long as s
(define (extend s t extra)
  (append t (build-list (max 0 (- (length s) (length t))) (lambda _ extra))))

(provide filter-multiple extend)

;; ryanc added:

(provide/contract
 [check-duplicate
  (->* (list?)
       (#:key (-> any/c any/c)
        #:same? (or/c dict? (-> any/c any/c any/c)))
       any)])

;; check-duplicate : (listof X)
;;                   #:key (X -> K)
;;                   #:same? (or/c (K K -> bool) dict?)
;;                -> X or #f
(define (check-duplicate items
                        #:key [key values]
                        #:same? [same? equal?])
  (cond [(procedure? same?)
         (cond [(eq? same? equal?)
                (check-duplicate/t items key (make-hash) #t)]
               [(eq? same? eq?)
                (check-duplicate/t items key (make-hasheq) #t)]
               [(eq? same? eqv?)
                (check-duplicate/t items key (make-hasheqv) #t)]
               [else
                (check-duplicate/list items key same?)])]
        [(dict? same?)
         (let ([dict same?])
           (if (dict-mutable? dict)
               (check-duplicate/t items key dict #t)
               (check-duplicate/t items key dict #f)))]))
(define (check-duplicate/t items key table mutating?)
  (let loop ([items items] [table table])
    (and (pair? items)
         (let ([key-item (key (car items))])
           (if (dict-ref table key-item #f)
               (car items)
               (loop (cdr items) (if mutating?
                                     (begin (dict-set! table key-item #t) table)
                                     (dict-set table key-item #t))))))))
(define (check-duplicate/list items key same?)
  (let loop ([items items] [sofar null])
    (and (pair? items)
         (let ([key-item (key (car items))])
           (if (for/or ([prev (in-list sofar)])
                 (same? key-item prev))
               (car items)
               (loop (cdr items) (cons key-item sofar)))))))

;; Eli: Just to have a record of this: my complaint about having this
;; code separately from `remove-duplicates' still stands.  Specifically,
;; that function decides when to use a hash table to make things faster,
;; and this code would benefit from the same.  It would be much better
;; to extend that function so it can be used for both tasks rather than
;; a new piece of code that does it (only do it in a worse way, re
;; performance).  Doing this can also benefit `remove-duplicates' -- for
;; example, make it accept a container so that users can choose how
;; when/if to use a hash table.

;; sam added from carl

(define-syntax (values->list stx)
  (syntax-case stx ()
    [(vl expr)
     (syntax/loc stx
       (call-with-values (lambda () expr) list))]))

(define (map/list n f ls)
  (cond
   [(andmap null? ls) (build-list n (lambda (i) null))]
   [(andmap pair? ls)
    (let* ([vs (values->list (apply f (map car ls)))]
           [k (length vs)])
      (unless (= k n)
        (error 'map/values
               "~a produced ~a values, not ~a: ~e"
               f k n vs))
      (map cons vs (map/list n f (map cdr ls))))]
   [else (error 'map/values "list lengths differ")]))

(define (map/values n f . ls)
  (apply values (map/list n f ls)))

(define (map2 f . ls)
  (apply values (map/list 2 f ls)))

(provide map2 map/values)

;; dvanhorn added:

(define (remf f ls)
  (cond [(null? ls) '()]
        [(f (car ls)) (cdr ls)]
        [else 
         (cons (car ls)
               (remf f (cdr ls)))]))

(provide/contract [remf (-> procedure? list? list?)])


;; stamourv added:

;; (y y -> bool) (listof x) #:key (x -> y) -> (listof (listof x))
;; groups together elements that are considered equal
;; =? should be reflexive, transitive and commutative
(define (group-by =? l #:key [key values])
  (for/fold ([res '()]) ; list of lists
      ([elt (in-list l)])
    (let loop ([classes     res] ; "zipper" of the equivalence classes
               [rev-classes '()])
      (cond [(null? classes)
             ;; did not find an equivalence class, create a new one
             (cons (list elt) res)]
            [(=? (key elt) (key (car (car classes))))
             ;; found the equivalence class
             (append rev-classes ; we keep what we skipped
                     ;; we extend the current class
                     (list (cons elt (car classes)))
                     (cdr classes))] ; and add the rest
            [else ; keep going
             (loop (cdr classes)
                   (cons (car classes) rev-classes))]))))

(provide/contract
 [group-by (->* (procedure? list?) (#:key procedure?)
                list?)])

;; endobson added:

(define (list-update l i f)
  (cond
    [(zero? i) (cons (f (car l)) (cdr l))]
    [else (cons (car l) (list-update (cdr l) (sub1 i) f))]))

(define (list-set l k v)
  (list-update l k (lambda (_) v)))

(provide/contract
 [list-update (->i [(list list?)
                    (index (list) (and/c (>=/c 0) (</c (length list))))
                    (updater (-> any/c any/c))]
                   [_ list?])]
 [list-set  (->i [(list list?)
                  (index (list) (and/c (>=/c 0) (</c (length list))))
                  (value any/c)]
                 [_ list?])])
