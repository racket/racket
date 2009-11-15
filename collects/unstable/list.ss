#lang scheme

; list-prefix : list? list? -> (or/c list? false/c)
; Is l a prefix or r?, and what is that prefix?
(define (list-prefix? ls rs)
  (match ls
    [(list)
     #t]
    [(list-rest l0 ls)
     (match rs
       [(list)
        #f]
       [(list-rest r0 rs)
        (if (equal? l0 r0)
            (list-prefix? ls rs)
            #f)])]))
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

(provide/contract
 [list-prefix? (list? list? . -> . boolean?)])

(define (filter-multiple l . fs)
  (apply values
         (map (lambda (f) (filter f l)) fs)))

;; Listof[A] Listof[B] B -> Listof[B]
;; pads out t to be as long as s
(define (extend s t extra)
  (append t (build-list (- (length s) (length t)) (lambda _ extra))))

(provide filter-multiple extend)