#lang scheme/base

(require "planet-requires.ss" 
         "type-rep.ss"
         "effect-rep.ss"
         "type-comparison.ss"
         "resolve-type.ss"
         "type-utils.ss") 
(require mzlib/plt-match         
         mzlib/trace)

(provide unify1)


;; the types in the constraint list must be well-formed
(define (unify cl) (unify/acc cl '()))

(define (unify1 t1 t2) (unify (list (list t1 t2))))

(define (unify/acc constraint-list acc)
  (parameterize ([match-equality-test type-equal?])
    (match constraint-list
      ;; done!
      [(list) acc]
      ;; equal constraints can be discarded
      [(list (list t t) rest ...) (unify/acc rest acc)]
      ;; name resolution
      [(list (list (Name: n) (Name: n*)) rest ...) 
       (if (free-identifier=? n n*)
           (unify/acc rest acc)
           #f)]        
      ;; type application
      [(list (list (App: r args _) (App: r* args* _)) rest ...)
       (unify/acc (append (map list (cons r args) (cons r* args*)) rest) acc)]
      ;; unequal bound variables do not unify
      [(list (list (B: _) (B: _)) rest ...) #f]
      ;; unify a variable (skipping the occurs check for now)
      [(list (list (F: v) t) rest ...) 
       (unify/acc (map (lambda (p) (map (lambda (e) (subst v t e)) p)) rest) 
                  (cons (list v t) acc))]
      [(list (list t (F: v)) rest ...) 
       (unify/acc (map (lambda (p) (map (lambda (e) (subst v t e)) p)) rest) 
                  (cons (list v t) acc))]
      ;; arrow types - just add a whole bunch of new constraints
      [(list (list (Function: (list (arr: ts t t-rest t-thn-eff t-els-eff) ...)) 
                   (Function: (list (arr: ss s s-rest s-thn-eff s-els-eff) ...)))
             rest ...)
       #;(printf "arrow unification~n")
       (let ()
         (define (compatible-rest t-rest s-rest)
           (andmap (lambda (x y) (or (and x y) (and (not x) (not y)))) ;; either both #f or both not #f
                   t-rest s-rest))
         (define (flatten/zip x y) (map list (apply append x) (apply append y)))
         (if (and (= (length ts) (length ss))
                  (compatible-rest t-rest s-rest)
                  (effects-equal? t-thn-eff s-thn-eff)
                  (effects-equal? t-els-eff s-els-eff))
             (let ([ret-constraints (map list t s)]
                   ;; remove the #f's before adding to the constraints
                   [rest-constraints (map list (filter values t-rest) (filter values s-rest))]
                   ;; transform ((a b c) (d e)) ((1 2 3) (4 5)) into ((a 1) (b 2) (c 3) (d 4) (e 5))
                   [arg-constraints (flatten/zip ts ss)])
               #;(printf "constraints ~a~n"(append ret-constraints rest-constraints arg-constraints))
               (unify/acc (append arg-constraints rest-constraints ret-constraints rest) acc))
             (begin #;(printf "failure!~n") #f)))]
      ;; aggregate types are simple
      [(list (list (Vector: t) (Vector: s)) rest ...) (unify/acc (cons (list t s) rest) acc)]
      [(list (list (Pair: t1 t2) (Pair: s1 s2)) rest ...) 
       (unify/acc (list* (list t1 s1) (list t2 s2) rest) acc)]
      [(list (list (Hashtable: t1 t2) (Hashtable: s1 s2)) rest ...) 
       (unify/acc (list* (list t1 s1) (list t2 s2) rest) acc)]
      [(list (list (Param: t1 t2) (Param: s1 s2)) rest ...) 
       (unify/acc (list* (list t1 s1) (list t2 s2) rest) acc)]
      ;; structs
      [(list (list (Struct: nm p elems proc _ _ _) (Struct: nm p elems* proc* _ _ _)) rest ...)
       (cond [(and proc proc*)
              (unify/acc (append rest (map list elems elems*) (list (list proc proc*))) acc)]
             [(or proc proc*) #f]
             [else (unify/acc (append rest (map list elems elems*)) acc)])]
      ;; union types - oh no!
      [(list (list (Union: l1) (Union: l2)) rest ...)
       (and (= (length l1) (length l2))
            (unify/acc (append (map list l1 l2) rest) acc))]
      [(list (or (list (Union: _) _) (list _ (Union: _))) rest ...) 
       #;(printf "FIXME: union type ~n~a~n---------~n~a~n in unifier~n" 
                 (caar constraint-list)
                 (cadar constraint-list)) 
       #f]
      ;; mu types
      [(list (list (Mu-unsafe: s) (Mu-unsafe: t)) rest ...) 
       (unify/acc (cons (list s t) rest) acc)]
      [(list (list t (? Mu? s)) rest ...) (unify/acc (cons (list t (unfold s)) rest) acc)]
      [(list (list (? Mu? s) t) rest ...) (unify/acc (cons (list (unfold s) t) rest) acc)]
      #;[((or (($ mu _ _) _) (_ ($ mu _ _))) rest ...) 
         (printf "FIXME: mu types ~a in unifier~n" constraint-list)
         #f]
      ;; polymorphic types - don't do that
      [(list (or (list _ (? Poly?)) (list _ (? Poly?))) rest ...)
       ;(printf "FIXME: poly type in unifier~n")
       #f]
      ;; nothing else can have type variables
      [else #f]
      )))

;(trace unify/acc)

