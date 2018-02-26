#lang racket/base

(provide fallback?
         fallback-first
         fallback-rest
         fallback-push
         fallback-update-first
         fallback-map
         fallback->list)

;; When a syntax object is expanded in namespace A and then
;; re-expanded in namespace B, then the scopes of B are added to rhe
;; syntax object, but a failed binding search will fall back to the
;; scope set that doesn't include the additional scope for B. This
;; fallback makes it easier to work across namespaces (including
;; moving from the top level to a module body or vice versa), and it
;; accomodates existing Racket programs.
;;
;; A syntax object contains a fallback search list only if
;; `push-scope` has been used. The fallback chain is in the
;; `shifted-multi-scopes` part of a syntax object (since the relevant
;; namespace scope is always a multi scope).
;;
;; A fallback is created by `push-scope`, which creates a new fallback
;; layer if the given multi-scope is not in the current set of scopes.

(struct fallback (search-list)
  ;; Can appear in serialized:
  #:prefab)

(define (fallback-first smss)
  (if (fallback? smss)
      (car (fallback-search-list smss))
      smss))

(define (fallback-rest smss)
  (define l (cdr (fallback-search-list smss)))
  (if (null? (cdr l))
      (car l)
      (fallback l)))

(define (fallback-push smss smss/maybe-fallback)
  (fallback
   (cons smss
         (if (fallback? smss/maybe-fallback)
             (fallback-search-list smss/maybe-fallback)
             (list smss/maybe-fallback)))))

(define (fallback-update-first smss f)
  (if (fallback? smss)
      (let ([l (fallback-search-list smss)])
        (fallback (cons (f (car l)) (cdr l))))
      (f smss)))

(define (fallback-map smss f)
  (if (fallback? smss)
      (fallback (for/list ([smss (in-list (fallback-search-list smss))])
                  (f smss)))
      (f smss)))

(define (fallback->list smss)
  (if (fallback? smss)
      (fallback-search-list smss)
      (list smss)))
