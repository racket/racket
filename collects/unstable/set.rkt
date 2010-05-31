#lang racket/base
(require racket/set racket/contract)

(define (set->list set)
  (for/list ([elem (in-set set)]) elem))

(define (list->set elems) (apply set elems))
(define (list->seteq elems) (apply seteq elems))
(define (list->seteqv elems) (apply seteqv elems))

(define (set=? one two)
  (and (subset? one two)
       (subset? two one)))

(define (proper-subset? one two)
  (and (subset? one two)
       (not (subset? two one))))

(define (set-exclusive-or s0 . rest)
  (for/fold ([s s0]) ([s* (in-list rest)])
    (define-values [ big small ]
      (if (>= (set-count s) (set-count s*))
        (values s s*)
        (values s* s)))
    (for/fold ([s big]) ([e (in-set small)])
      (if (set-member? s e)
        (set-remove s e)
        (set-add s e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Exports
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [list->set (-> list? set?)]
 [list->seteq (-> list? set?)]
 [list->seteqv (-> list? set?)]
 [set->list (-> set? list?)]
 [set=? (-> set? set? boolean?)]
 [proper-subset? (-> set? set? boolean?)]
 [set-exclusive-or (->* [set?] [] #:rest (listof set?) set?)])
