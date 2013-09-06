#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (types abbrev union subtype resolve utils)
         racket/match)

(provide (rename-out [*remove remove]) overlap)

(define (simple-datum? v)
  (or (null? v)
      (symbol? v)
      (number? v)
      (boolean? v)
      (pair? v)
      (string? v)
      (keyword? v)))


(define (overlap t1 t2)
  (let ([ks (Type-key t1)] [kt (Type-key t2)])
    (cond
      [(type-equal? t1 t2) #t]
      [(and (symbol? ks) (symbol? kt) (not (eq? ks kt))) #f]
      [(and (symbol? ks) (pair? kt) (not (memq ks kt))) #f]
      [(and (symbol? kt) (pair? ks) (not (memq kt ks))) #f]
      [(and (pair? ks) (pair? kt)
            (for/and ([i (in-list ks)]) (not (memq i kt))))
       #f]
      [else
       (match (list t1 t2)
         [(list (Univ:) _) #t]
         [(list _ (Univ:)) #t]
         [(list (F: _) _) #t]
         [(list _ (F: _)) #t]
         [(list (Opaque: _) _) #t]
         [(list _ (Opaque: _)) #t]
         [(list (Name: n) (Name: n*))
          (or (free-identifier=? n n*)
              (overlap (resolve-once t1) (resolve-once t2)))]
         [(list _ (Name: _))
           (overlap t1 (resolve-once t2))]
         [(list (Name: _) _)
           (overlap (resolve-once t1) t2)]
         [(list (? Mu?) _) (overlap (unfold t1) t2)]
         [(list _ (? Mu?)) (overlap t1 (unfold t2))]

         [(list (Refinement: t _) t2) (overlap t t2)]
         [(list t1 (Refinement: t _)) (overlap t1 t)]

         [(list (Union: e) t)
          (ormap (lambda (t*) (overlap t* t)) e)]
         [(list t (Union: e))
          (ormap (lambda (t*) (overlap t t*)) e)]
         [(or (list _ (? Poly?)) (list (? Poly?) _))
          #t] ;; these can have overlap, conservatively
         [(list (Base: s1 _ _ _) (Base: s2 _ _ _)) (or (subtype t1 t2) (subtype t2 t1))]
         [(list (Base: _ _ _ _) (Value: _)) (subtype t2 t1)] ;; conservative
         [(list (Value: _) (Base: _ _ _ _)) (subtype t1 t2)] ;; conservative
         [(list (Syntax: t) (Syntax: t*))
          (overlap t t*)]
         [(or (list (Syntax: _) _)
              (list _ (Syntax: _)))
          #f]
         [(list (Base: _ _ _ _) _) #f]
         [(list _ (Base: _ _ _ _)) #f]
         [(list (Value: (? pair? v)) (Pair: _ _)) #t]
         [(list (Pair: _ _) (Value: (? pair? v))) #t]
         [(list (Pair: a b) (Pair: a* b*))
          (and (overlap a a*)
               (overlap b b*))]
         ;; lots of things are sequences
         [(list (Sequence: _) _) #t]
         [(list _ (Sequence: _)) #t]
         [(or (list (Pair: _ _) _)
              (list _ (Pair: _ _)))
          #f]
         [(list (Value: (? simple-datum? v1))
                (Value: (? simple-datum? v2)))
          (equal? v1 v2)]
         [(or (list (Value: (? simple-datum?))
                    (Struct: n _ flds _ _ _))
              (list (Struct: n _ flds _ _ _) 
                    (Value: (? simple-datum?))))
          #f]
         [(list (Struct: n _ flds _ _ _)
                (Struct: n* _ flds* _ _ _)) (=> nevermind)
          (unless (free-identifier=? n n*) (nevermind))
          (for/and ([f (in-list flds)] [f* (in-list flds*)])
            (match* (f f*)
              [((fld: t _ _) (fld: t* _ _)) (overlap t t*)]))]
         [(list (Struct: n #f _ _ _ _)
                (StructTop: (Struct: n* #f _ _ _ _))) (=> nevermind)
          (unless (free-identifier=? n n*) (nevermind))
          #t]
         ;; n and n* must be different, so there's no overlap
         [(list (Struct: n #f flds _ _ _)
                (Struct: n* #f flds* _ _ _))
          #f]
         [(list (Struct: n #f flds _ _ _)
                (StructTop: (Struct: n* #f flds* _ _ _)))
          #f]
         [(list (and t1 (Struct: _ _ _ _ _ _))
                (and t2 (Struct: _ _ _ _ _ _)))
          (or (subtype t1 t2) (subtype t2 t1))]
         [(list (== (-val eof))
                (Function: _))
          #f]
         [else #t])])))


;(trace overlap)

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
