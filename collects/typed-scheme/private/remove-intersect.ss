#lang scheme/base

(require (except-in "../utils/utils.ss" extend))
(require (rep type-rep)
	 (private union subtype resolve-type type-effect-convenience type-utils)
         mzlib/plt-match mzlib/trace)

(provide (rename-out [*remove remove]) overlap)


(define (overlap t1 t2)
  (match (list t1 t2)        
    [(list (Univ:) _) #t]
    [(list _ (Univ:)) #t]
    [(list (F: _) _) #t]
    [(list _ (F: _)) #t]
    [(list (Name: n) (Name: n*)) (free-identifier=? n n*)]
    [(list (? Mu?) _) (overlap (unfold t1) t2)]
    [(list _ (? Mu?)) (overlap t1 (unfold t2))]
    [(list (Union: e) t)
     (ormap (lambda (t*) (overlap t* t)) e)]
    [(list t (Union: e))
     (ormap (lambda (t*) (overlap t t*)) e)]
    [(or (list _ (? Poly?)) (list (? Poly?) _))
     #t] ;; these can have overlap, conservatively
    [(list (Base: s1) (Base: s2)) (eq? s1 s2)]
    [(list (Base: _) (Value: _)) (subtype t2 t1)] ;; conservative
    [(list (Value: _) (Base: _)) (subtype t1 t2)] ;; conservative
    [(list (Syntax: t) (Syntax: t*))
     (overlap t t*)]
    [(or (list (Syntax: _) _)
         (list _ (Syntax: _)))
     #f]    
    [(list (Base: _) _) #f]
    [(list _ (Base: _)) #f]
    [(list (Value: (? pair? v)) (Pair: _ _)) #t]
    [(list (Pair: _ _) (Value: (? pair? v))) #t]
    [(list (Pair: a b) (Pair: a* b*))
     (and (overlap a a*)
          (overlap b b*))]
    [(or (list (Pair: _ _) _)
         (list _ (Pair: _ _)))
     #f]
    [else #t]))



;(trace restrict)

;; also not yet correct
;; produces old without the contents of rem
(define (*remove old rem)
  (define initial
    (if (subtype old rem) 
        (Un) ;; the empty type
        (match (list old rem)
          [(list (or (App: _ _ _) (Name: _)) t)
           ;; must be different, since they're not subtypes
           ;; and n must refer to a distinct struct type
           old]          
          [(list (Union: l) rem)
           (apply Un (map (lambda (e) (*remove e rem)) l))]                  
          [(list (? Mu? old) t) (*remove (unfold old) t)]
          [(list (Poly: vs b) t) (make-Poly vs (*remove b rem))]
          [_ old])))
  (if (subtype old initial) old initial))

;(trace *remove)
;(trace restrict)
