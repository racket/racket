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
;; Eli: Seeing the code in "racket/set.rkt", my guess is that this could
;; be much faster if it used two `set-subtract' instead.  Also, using
;; `set-count' would make this *much* faster since the common case is
;; two sets with different number of elements.  (And if the code moves
;; into "racket/set.rkt", then it could be even faster by using the
;; representation directly.)

;; Ryan: Sets implement prop:equal+hash, so isn't this just 'equal?'?

(define (proper-subset? one two)
  (and (subset? one two)
       (not (subset? two one))))
;; Eli: Same comment here -- both re using `set-subtract', and using the
;; count first.

;; Ryan: better yet:
;;   (and (subset? one two) (not (= (set-count one) (set-count two))))

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
