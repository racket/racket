#lang scheme/base
(require scheme/contract
         scheme/dict
         (for-syntax scheme/base))

; list-prefix : list? list? -> boolean?
; Is l a prefix or r?
(define (list-prefix? ls rs)
  (or (null? ls)
      (and (pair? rs)
           (equal? (car ls) (car rs))
           (list-prefix? (cdr ls) (cdr rs)))))

;; Eli: Is this some `match' obsession syndrom?  The simple definition:
;;   (define (list-prefix? ls rs)
;;     (or (null? ls) (and (pair? rs) (equal? (car ls) (car rs))
;;                         (list-prefix? (cdr ls) (cdr rs)))))
;;   is shorter, and faster.  As for making this a library function: how
;;   about a version that removes the equal prefix from two lists and
;;   returns the tails -- this way you can tell if they're equal, or one
;;   is a prefix of the other, or if there was any equal prefix at all.
;;   (Which can be useful for things like making a path relative to
;;   another path.)  A nice generalization is to make it get two or more
;;   lists, and return a matching number of values.
;; ryanc: changed to use Eli's version

(provide/contract
 [list-prefix? (list? list? . -> . boolean?)])

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

(provide map/values)

