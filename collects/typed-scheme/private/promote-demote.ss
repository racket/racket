#lang scheme/unit

(require "type-effect-convenience.ss" "type-rep.ss" 
         "type-utils.ss" "union.ss" 
         "signatures.ss" 
         scheme/list)

(import)
(export promote-demote^)

(define (V-in? V . ts)
  (for/or ([e (append* (map fv ts))])
          (memq e V)))

(define (var-promote T V)
  (define (vp t) (var-promote t V))
  (define (inv t) (if (V-in? V t) Univ t))
  (type-case vp T
             [#:F name (if (memq name V) Univ T)]
             [#:Vector t (make-Vector (inv t))]
             [#:Box t (make-Box (inv t))]
             [#:Hashtable k v
                          (if (V-in? V v)
                              Univ
                              (make-Hashtable (vp k) v))]
             [#:Param in out
                          (make-Param (var-demote in V)
                                          (vp out))]
             [#:arr dom rng rest drest thn els
                    (cond
                      [(apply V-in? V (append thn els))
                       (make-arr null (Un) Univ #f null null)]
                      [(and drest (memq (cdr drest) V))
                       (make-arr (for/list ([d dom]) (var-demote d V))
                                 (vp rng)
                                 (var-demote (car drest) V)
                                 #f
                                 thn
                                 els)]
                      [else
                       (make-arr (for/list ([d dom]) (var-demote d V))
                                 (vp rng)
                                 (and rest (var-demote rest V))
                                 (and drest
                                      (cons (var-demote (car drest) V)
                                            (cdr drest)))
                                 thn
                                 els)])]))

(define (var-demote T V)
  (define (vd t) (var-demote t V))
  (define (inv t) (if (V-in? V t) (Un) t))
  (type-case vd T
             [#:F name (if (memq name V) (Un) T)]
             [#:Vector t (make-Vector (inv t))]
             [#:Box t (make-Box (inv t))]
             [#:Hashtable k v
                          (if (V-in? V v)
                              (Un)
                              (make-Hashtable (vd k) v))]
             [#:Param in out
                          (make-Param (var-promote in V)
                                          (vd out))]
             [#:arr dom rng rest drest thn els
                    (cond
                      [(apply V-in? V (append thn els))
                       (make-arr null (Un) Univ #f null null)]
                      [(and drest (memq (cdr drest) V))
                       (make-arr (for/list ([d dom]) (var-promote d V))
                                 (vd rng)
                                 (var-promote (car drest) V)
                                 #f
                                 thn
                                 els)]
                      [else
                       (make-arr (for/list ([d dom]) (var-promote d V))
                                 (vd rng)
                                 (and rest (var-promote rest V))
                                 (and drest
                                      (cons (var-promote (car drest) V)
                                            (cdr drest)))
                                 thn
                                 els)])]))
