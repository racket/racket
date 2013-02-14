#lang racket/unit

(require "../utils/utils.rkt")
(require (rep type-rep rep-utils free-variance)
         (types abbrev utils union subtype remove-intersect resolve substitute)
         racket/list
         "signatures.rkt"
         racket/match)
(require racket/trace)

(import infer^)
(export restrict^)

;; we don't use union map directly, since that might produce too many elements
  (define (union-map f l)
    (match l
      [(Union: es)
       (let ([l (map f es)])
         (apply Un l))]))

;; Replace Top types in covariant positions with corresponding parametric types
;; with free variables.
;; This is done by replacing all likely types with free variables and then
;; computing which of those are in covariant positions. The ones that are are
;; replaced by the parametric type, the others are returned to their original
;; values.
(define (make-existential t)
  (define new-syms (make-hash))
  (define (recur type)
    (define (other-constructor f (n 1))
      (let* ((new-sym-list (build-list n (λ (_) (gensym))))
             (ts (map make-F new-sym-list)))
         (for ((new-sym new-sym-list)
               (t ts))
           (hash-set! new-syms new-sym
             (list type (apply f ts))))
         (apply Un ts)))
    (type-case (#:Type recur #:Filter (sub-f recur) #:Object (sub-o recur))
      type
      [#:Univ (other-constructor values)]
      [#:BoxTop (other-constructor -box)]
      [#:ChannelTop (other-constructor -channel)]
      [#:VectorTop (other-constructor -vec)]
      [#:HashtableTop (other-constructor -HT 2)]
      [#:MPairTop (other-constructor -mpair 2)]
      [#:ThreadCellTop (other-constructor -thread-cell)]))


  (define new-t (recur t))
  (define existentials (make-hash))
  (define substitution
    (for/hash (((free-var variance) (free-vars-hash (free-vars* new-t))))
      (values
        free-var
        (t-subst
          (match (hash-ref new-syms free-var #f)
           [(list old-type new-type)
            (if (or (equal? Covariant variance)
                    (equal? Constant variance))
                (begin
                  (hash-set! existentials free-var #t)
                  new-type)
                old-type)]
           [#f (make-F free-var)])))))
  (list (hash-keys existentials)
        (subst-all substitution new-t)))

;; NEW IMPL
;; restrict t1 to be a subtype of t2
;; if `f' is 'new, use t2 when giving up, otherwise use t1
(define (restrict* t1 t2 [f 'new])
  (cond
    [(subtype t1 t2) t1] ;; already a subtype
    [(match t2
       [(Poly: vars t)
        (let ([subst (infer vars null (list t1) (list t) t1)])
          (and subst (restrict* t1 (subst-all subst t1) f)))]
       [_ #f])]
    [(Union? t1) (union-map (lambda (e) (restrict* e t2 f)) t1)]
    [(Union? t2) (union-map (lambda (e) (restrict* t1 e f)) t2)]
    [(needs-resolving? t1) (restrict* (resolve-once t1) t2 f)]
    [(needs-resolving? t2) (restrict* t1 (resolve-once t2) f)]
    [(match (make-existential t2)
      ;; we don't actually want this - want something that's a part of t1
      [(list (list) _) (and (subtype t2 t1) t2)]
      [(list vars t)
       (printf "Vars: ~a, t: ~a, t1: ~a~n" vars t t1)
       (let ([subst (infer vars null (list t) (list t1) (->* (list t) Univ))])
          (and subst (subst-all subst t)))])]
    [(not (overlap t1 t2)) (Un)] ;; there's no overlap, so the restriction is empty
    [else (if (eq? f 'new) t2 t1)])) ;; t2 and t1 have a complex relationship, so we punt
(trace restrict*)

(define restrict restrict*)
