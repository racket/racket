#lang scheme/base

(require (rename-in "../utils/utils.ss" [infer infer-in]))
(require (rename-in (types subtype convenience remove-intersect)                   
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (infer-in infer)
         (rep type-rep)
         scheme/contract scheme/match
         stxclass/util
         (for-syntax scheme/base))

(define (replace-nth l i f)
  (cond [(null? l) (error 'replace-nth "list not long enough" l i f)]
        [(zero? i) (cons (f (car l)) (cdr l))]
        [else (cons (car l) (replace-nth (cdr l) (sub1 i) f))]))

(define/contract (update t lo)
  (Type/c Filter/c . -> . Type/c)
  (match* (t lo)
    ;; pair ops
    [((Pair: t s) (TypeFilter: u (list* (CarPE:) rst) x))
     (make-Pair (update t (make-TypeFilter u rst x)) s)]
    [((Pair: t s) (NotTypeFilter: u (list* (CarPE:) rst) x))
     (make-Pair (update t (make-NotTypeFilter u rst x)) s)]
    [((Pair: t s) (TypeFilter: u (list* (CarPE:) rst) x))
     (make-Pair t (update s (make-TypeFilter u rst x)))]
    [((Pair: t s) (NotTypeFilter: u (list* (CdrPE:) rst) x))
     (make-Pair t (update s (make-NotTypeFilter u rst x)))]
    
    ;; struct ops
    [((Struct: nm par flds proc poly pred cert) 
      (TypeFilter: u (list* (StructPE: (? (lambda (s) (subtype t s)) s) idx) rst) x))
     (make-Struct nm par (replace-nth flds idx (lambda (e) (update e (make-TypeFilter u rst x)))))]
    [((Struct: nm par flds proc poly pred cert) 
      (NotTypeFilter: u (list* (StructPE: (? (lambda (s) (subtype t s)) s) idx) rst) x))
     (make-Struct nm par (replace-nth flds idx (lambda (e) (update e (make-NotTypeFilter u rst x)))))]
    
    ;; otherwise
    [(t (TypeFilter: u (list) _))
     (restrict t u)]
    [(t (NotTypeFilter: u (list) _))
     (remove t u)]))
