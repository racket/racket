#lang scheme/base

(require (rename-in "../utils/utils.ss" [infer infer-in]))
(require (rename-in (types subtype convenience remove-intersect union)                   
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (infer-in infer)
         (rep type-rep)
         (utils tc-utils)
         (types resolve)
         (only-in (env type-environments lexical-env) env? update-type/lexical env-map)
         scheme/contract scheme/match
         stxclass/util mzlib/trace
         (for-syntax scheme/base))

(provide env+)

(define (replace-nth l i f)
  (cond [(null? l) (error 'replace-nth "list not long enough" l i f)]
        [(zero? i) (cons (f (car l)) (cdr l))]
        [else (cons (car l) (replace-nth (cdr l) (sub1 i) f))]))

(trace replace-nth)

(define/contract (update t lo)
  (Type/c Filter/c . -> . Type/c)
  (match* ((resolve t) lo)
    ;; pair ops
    [((Pair: t s) (TypeFilter: u (list* (CarPE:) rst) x))
     (make-Pair (update t (make-TypeFilter u rst x)) s)]
    [((Pair: t s) (NotTypeFilter: u (list* (CarPE:) rst) x))
     (make-Pair (update t (make-NotTypeFilter u rst x)) s)]
    [((Pair: t s) (TypeFilter: u (list* (CdrPE:) rst) x))
     (make-Pair t (update s (make-TypeFilter u rst x)))]
    [((Pair: t s) (NotTypeFilter: u (list* (CdrPE:) rst) x))
     (make-Pair t (update s (make-NotTypeFilter u rst x)))]
    
    ;; struct ops
    [((Struct: nm par flds proc poly pred cert) 
      (TypeFilter: u (list* (StructPE: (? (lambda (s) (subtype t s)) s) idx) rst) x))     
     (make-Struct nm par (replace-nth flds idx (lambda (e) (update e (make-TypeFilter u rst x)))) proc poly pred cert)]
    [((Struct: nm par flds proc poly pred cert) 
      (NotTypeFilter: u (list* (StructPE: (? (lambda (s) (subtype t s)) s) idx) rst) x))
     (make-Struct nm par (replace-nth flds idx (lambda (e) (update e (make-NotTypeFilter u rst x)))) proc poly pred cert)]
    
    ;; otherwise
    [(t (TypeFilter: u (list) _))
     (restrict t u)]
    [(t (NotTypeFilter: u (list) _))
     (remove t u)]
    [(t lo)
     (int-err "update along ill-typed path: ~a ~a" t lo)]))

(define/contract (env+ env fs)
  (env? (listof Filter/c) . -> . env?)
  (for/fold ([Γ env]) ([f fs])
    (match f
      [(Bot:) (env-map (lambda (x) (cons (car x) (Un))) Γ)]
      [(or (TypeFilter: _ _ x) (NotTypeFilter: _ _ x))
       (update-type/lexical (lambda (x t) (update t f)) x Γ)])))
