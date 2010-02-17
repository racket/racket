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
         (only-in (env type-environments lexical-env) env? update-type/lexical env-map env-props replace-props)
         scheme/contract scheme/match
         mzlib/trace unstable/debug
         (typecheck tc-metafunctions)
         (for-syntax scheme/base))

(provide env+)

(define (replace-nth l i f)
  (cond [(null? l) (error 'replace-nth "list not long enough" l i f)]
        [(zero? i) (cons (f (car l)) (cdr l))]
        [else (cons (car l) (replace-nth (cdr l) (sub1 i) f))]))

;(trace replace-nth)

(define/contract (update t lo)
  (Type/c Filter/c . -> . Type/c)
  (match* ((resolve t) lo)
    ;; pair ops
    [((Pair: t s) (TypeFilter: u (list rst ... (CarPE:)) x))
     (make-Pair (update t (make-TypeFilter u rst x)) s)]
    [((Pair: t s) (NotTypeFilter: u (list rst ... (CarPE:)) x))
     (make-Pair (update t (make-NotTypeFilter u rst x)) s)]
    [((Pair: t s) (TypeFilter: u (list rst ... (CdrPE:)) x))
     (make-Pair t (update s (make-TypeFilter u rst x)))]
    [((Pair: t s) (NotTypeFilter: u (list rst ... (CdrPE:)) x))
     (make-Pair t (update s (make-NotTypeFilter u rst x)))]
    
    ;; struct ops
    [((Struct: nm par flds proc poly pred cert acc-ids) 
      (TypeFilter: u (list rst ... (StructPE: (? (lambda (s) (subtype t s)) s) idx)) x))     
     (make-Struct nm par (replace-nth flds idx (lambda (e) (update e (make-TypeFilter u rst x)))) proc poly pred cert )]
    [((Struct: nm par flds proc poly pred cert acc-ids) 
      (NotTypeFilter: u (list rst ... (StructPE: (? (lambda (s) (subtype t s)) s) idx)) x))
     (make-Struct nm par (replace-nth flds idx (lambda (e) (update e (make-NotTypeFilter u rst x)))) proc poly pred cert acc-ids)]
    
    ;; otherwise
    [(t (TypeFilter: u (list) _))
     (restrict t u)]
    [(t (NotTypeFilter: u (list) _))
     (remove t u)]
    [((Union: ts) lo)
     (apply Un (map (lambda (t) (update t lo)) ts))]
    [(t* lo)
     (int-err "update along ill-typed path: ~a ~a ~a" t t* lo)]))

;; sets the flag box to #f if anything becomes (U)
(d/c (env+ env fs flag)
  (env? (listof Filter/c) (box/c #t). -> . env?)
  (define-values (imps atoms) (combine-props fs (env-props env)))
  (for/fold ([Γ (replace-props env imps)]) ([f atoms])
    (match f
      [(Bot:) (set-box! flag #f) (env-map (lambda (x) (cons (car x) (Un))) Γ)]
      [(ImpFilter: _ _) Γ]
      [(or (TypeFilter: _ _ x) (NotTypeFilter: _ _ x))
       (update-type/lexical (lambda (x t) (let ([new-t (update t f)])
                                            (when (type-equal? new-t (Un))
                                              (set-box! flag #f))
                                            new-t))
                            x Γ)])))
