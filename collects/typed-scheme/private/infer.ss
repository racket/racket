#lang scheme/base

;; NO LONGER USED
;; NOT YET REMOVED AS DOCUMENTATION

(require "unify.ss" "type-comparison.ss" "type-rep.ss" "effect-rep.ss" "subtype.ss"
         "planet-requires.ss" "tc-utils.ss" "union.ss"
         "resolve-type.ss"
         "type-effect-convenience.ss"
         (lib "trace.ss")
         (lib "plt-match.ss")
         (lib "list.ss"))
(require (galore))

#;(provide infer infer/list infer/list/vararg combine table:un exn:infer?)

;; exn representing failure of inference
;; s,t both types
#;
(define-struct (exn:infer exn:fail) (s t))

(define-values (fail-sym exn:infer?)
  (let ([sym (gensym)])
    (values sym (lambda (s) (eq? s sym)))))

;; inference failure - masked before it gets to the user program
(define-syntax fail!
  (syntax-rules ()
    [(_ s t) (raise fail-sym)
             #;(raise (make-exn:infer "inference failed" (current-continuation-marks) s t))
             #;(error "inference failed" s t)]))

;; conveneice function
(define (alist->mapping vars) (table:alist->eq (map (lambda (x) (cons x 'fail)) vars)))

;; flag is one of: 'co, 'contra, 'both, #f

;; Mapping is a table that maps symbols to Results

;; A Result is one of:
;; - 'fail (not yet filled in)
;; - #f (not a variable we're concerned with)
;; - (list flag type)
;; 

;; s, t : Type
;; vars : Listof[Symbol]
;; produces a substitution for vars, or #f
;; the substitution makes s a supertype of t
;; only vars will be substituted, regardless of other free vars
(define ((mk-infer f) s t vars)
  (let ([mapping (alist->mapping vars)])
    (with-handlers
        ([exn:infer? (lambda _ #f)])
      (mapping->subst (f s t mapping 'co)))))

;; table[symbol, (list flag type)] -> substitution
;; convert a mapping to a substitution
(define (mapping->subst x) 
  (define sexp (table:to-sexp x))    
  (define result (filter (lambda (x) (list? (cadr x))) sexp))    
  ;(printf "sexp: ~a~n" sexp)
  (map (lambda (x) (list (car x) (cadr (cadr x)))) result))

;(trace mapping->subst)

;; least upper bound of two flags
;; the lattice is like this:
;;         'both
;;        /      \
;;     'co      'contra
;;        \      /
;;           #f
(define (lub a b)
  (match (list a b)
    [(list x  x)  x]
    [(list #f x)  x]
    [(list x  #f) x]
    [(list 'both x) 'both]
    [(list x 'both) 'both]
    [(list x y) 'both]))

;; combine: flag -> Result Result -> Result
;; combine two results into one
(define ((combine flag*) s t)
  (define (type-lub s t)
    (cond [(subtype s t) t]
          [(subtype t s) s]
          [else (Un s t)]))
  (define (type-glb s t)
    (cond [(subtype s t) s]
          [(subtype t s) t]
          [else (fail! s t)]))
  (define (go flag s t)
    (cond [(and (eq? flag 'both) (type-equal? s t)) s]
          [(eq? flag 'both) (fail! s t)]
          [(eq? flag 'co) (type-lub s t)]
          [(eq? flag 'contra) (type-glb s t)]
          [(eq? flag #f) (go flag* s t)]
          [else (int-err "bad flag value ~a" flag)]))
  (match (list s t)
    [(list 'fail t) t]
    [(list t 'fail) t]
    [(list (list sf s) (list tf t))
     (let* ([flag (lub flag* (lub sf tf))]
            [new-ty (go flag s t)])
       (list flag new-ty))]))
       #;
     
     ;(printf "flags : ~a ~a~n" sf tf)
     (cond 
       [(and sf tf (type-equal? s t)) (list (if (eq? sf tf) sf 'both) s)] ;; equal is fine
       [(memq 'both (list sf tf)) (fail! s t)] ;; not equal, needed to be
       [(and sf tf (not (eq? sf tf))) (fail! s t)] ;; not equal, needed to be
       [else
        (let ([flag (or sf tf flag)])
          (printf "flag is ~a~n" flag)
          (cond 
            [(eq? 'co flag) (list 'co (Un s t))]
            [(and (eq? 'contra flag) (subtype s t)) (list 'contra s)]
            [(and (eq? 'contra flag) (subtype t s)) (list 'contra t)]
            [else (fail! s t)]))])


;(trace combine)

;; combine two tables
;; table:un : flag -> Mapping Mapping -> Mapping
(define ((table:un flag) a b) (table:union/value a b (combine flag)))

;; infer/int/union : Listof[Type] Listof[Type] Mapping Flag -> Mapping
;; ss and ts represent unions of types
(define (infer/int/union ss ts mapping flag)
  (unless (= (length ss) (length ts))
    (fail! ss ts))
  ;; first, we remove common elements of ss and ts
  (let-values ([(ss* ts*)
                (values (filter (lambda (se) (not (memq se ts))) ss)
                        (filter (lambda (te) (not (memq te ss))) ts))])
    ;; we need to try all the pairwise possibilites
    (let ([l (map (lambda (x y) (infer/int x y mapping flag)) ss* ts*)])
      (foldl (table:un flag) (table:make-eq) l))))

;; infer/int/list : Listof[Type] Listof[Type] Mapping Flag -> Mapping
(define (infer/int/list ss ts mapping flag)
  (unless (= (length ss) (length ts))
    (fail! ss ts))
  (let ([l (map (lambda (x y) (infer/int x y mapping flag)) ss ts)])
    (foldl (table:un flag) (table:make-eq) l)))

;; infer/int/list/eff : Listof[Effect] Listof[Effect] Mapping Flag -> Mapping
(define (infer/int/list/eff ss ts mapping flag)
  (cond [(or (null? ss) (null? ts)) mapping]        
        [(not (= (length ss) (length ts)))
         ;(error 'bad "~a ~a" ss ts)
         (fail! ss ts)]
        [else (let ([l (map (lambda (x y) (infer/int/eff x y mapping flag)) ss ts)])
                (foldl (table:un flag) (table:make-eq) l))]))


;; infer/int/list/vararg : Listof[Type] Type Listof[Type] Mapping Flag Boolean -> Mapping
(define (infer/int/list/vararg ss rest ts mapping flag [direction #t])
  (unless (<= (length ss) (length ts))
    (printf "failing!~n")
    (fail! ss ts))
  (let loop-types
    ([ss ss]
     [ts ts]
     [tbl mapping])
    (define (ii a b)
      (if direction
          (infer/int a b tbl flag)
          (infer/int b a tbl flag)))
    (cond [(null? ts) tbl]
          [(and rest (null? ss))
           (let ([tbl* (ii rest (car ts))])
             (loop-types ss (cdr ts) tbl*))]
          [else 
           (let ([tbl* (ii (car ss) (car ts))])
             (loop-types (cdr ss) (cdr ts) tbl*))])))

(define (infer/list/vararg ss rest ts vars)
  (let ([mapping (alist->mapping vars)])
    (with-handlers
        ([exn:infer? (lambda _ #f)])
      (mapping->subst (infer/int/list/vararg ss rest ts mapping 'co)))))


;; Flag -> Flag
(define (swap flag) (case flag
                      [(co) 'contra]
                      [(contra) 'co]
                      [(both) 'both]
                      [(start) 'start]
                      [else (int-err "bad flag: ~a" flag)]))

(define (co? x) (eq? x 'co))
(define (contra? x) (eq? x 'contra))

(define (infer/int/eff s t mapping flag)
  (let ([fail! (case-lambda [() (fail! s t)]
                            [(s t) (fail! s t)])])
    (parameterize ([match-equality-test type-equal?])
      (match (list s t)
        [(list t t) mapping]
        [(list (Latent-Restrict-Effect: t1) (Latent-Restrict-Effect: t2)) (infer/int t1 t2 mapping flag)]
        [(list (Latent-Remove-Effect: t1) (Latent-Remove-Effect: t2)) (infer/int t1 t2 mapping flag)]
        ))))

;(trace fail!)

;; type type mapping -> mapping
(define (infer/int s t mapping flag)
  (let ([fail! (case-lambda [() (fail! s t)]
                            [(s t) (fail! s t)])])
    (define (i/i s t) (infer/int s t mapping flag))
    (parameterize ([match-equality-test type-equal?])
      (match (list s t)
        [(list t t) mapping]
        [(list (F: v) t)
         (let ([cur (table:lookup v mapping)])
           (match cur
             ;; we haven't yet seen this variable
             ['fail 
              (cond [(and (eq? flag 'contra) (type-equal? Univ t)) mapping]
                    [(and (eq? flag 'co) (type-equal? (Un) t)) mapping]
                    [else 
                     (table:insert v (list #f t) mapping)])]
             ;; we are ignoring this variable, but they weren't the same
             [#f (fail!)]
             ;; this variable has already been unified
             [(list cur-flag cur-t)
              (cond
                [(or (not cur-flag) (eq? flag cur-flag))
                 ;; this variable has only been handled once, or
                 ;; we're still going in the correct direction
                 (cond
                   ;; this is the same type we've already seen, so don't change the flag
                   [(type-equal? cur-t t) mapping]
                   ;; this is a supertype of what's been found before
                   [(and (eq? flag 'co) (subtype cur-t t)) 
                    (table:insert v (list flag t) mapping)] 
                   ;; this is a subtype of what's been found before
                   [(and (eq? flag 'co) (subtype t cur-t))
                    (table:insert v (list flag cur-t) mapping)]
                   [(eq? flag 'co)
                    (table:insert v (list flag (Un t cur-t)) mapping)]
                   ;; this is a subtype of what we found before
                   [(and (eq? flag 'contra) (subtype t cur-t))  
                    (table:insert v (list flag t) mapping)]
                   ;; this is a supertype of what we found before
                   [(and (eq? flag 'contra) (subtype t cur-t))
                    (table:insert v (list flag cur-t) mapping)]
                   [(and (eq? flag 'both) (subtype t cur-t) (subtype cur-t t))
                    mapping]
                   [(eq? flag 'both)
                    (fail! cur-t t)]
                   ;; impossible
                   [else (int-err "bad flag value: ~a" flag)])]
                ;; we've switched directions at least once
                [(type-equal? cur-t t) 
                 ;; we're still ok
                 (table:insert (list 'both cur-t) mapping)]
                [else
                 ;; we're not ok
                 (fail! cur-t t)])]))]
        ;; names are compared for equality
        [(list (Name: n) (Name: n*))
         (if (free-identifier=? n n*)
             mapping
             (fail!))]
        ;; type application          
        [(list (App: (Name: n) args _)
               (App: (Name: n*) args* _))
         (unless (free-identifier=? n n*)
           (fail!))
         (infer/int/list args args* mapping flag)]
        ;; vectors and boxes just recur, but are invariant
        [(list (Vector: s) (Vector: t)) (infer/int s t mapping 'both)]
        [(list (Box: s) (Box: t)) (infer/int s t mapping 'both)]
        ;; pairs just recur
        [(list (Pair: s1 s2) (Pair: t1 t2))
         (infer/int/list (list s1 s2) (list t1 t2) mapping flag)]
        ;; ht just recur
        [(list (Hashtable: s1 s2) (Hashtable: t1 t2))
         (infer/int/list (list s1 s2) (list t1 t2) mapping 'both)]
        [(list (Syntax: s1) (Syntax: s2))
         (infer/int s1 s2 mapping flag)]
        ;; structs just recur
        [(list (Struct: nm p flds proc _ _ _) (Struct: nm p flds* proc* _ _ _))
         (cond [(and proc proc*)
                (infer/int/list (cons proc flds) (cons proc* flds*) mapping flag)]
               [(or proc proc*)
                (fail!)]
               [else (infer/int/list flds flds* mapping flag)])]
        ;; parameters just recur
        [(list (Param: in1 out1) (Param: in2 out2))
         (infer/int/list (list in1 out1) (list in2 out2) mapping flag)]
        ;; if we have two mu's, we rename them to have the same variable
        ;; and then compare the bodies
        [(list (Mu-unsafe: s) (Mu-unsafe: t)) 
         (infer/int s t mapping flag)]
        ;; other mu's just get unfolded
        [(list s (? Mu? t)) (infer/int s (unfold t) mapping flag)]
        [(list (? Mu? s) t) (infer/int (unfold s) t mapping flag)]
        ;; two unions with the same number of elements, so we just try to unify them pairwise
        [(list (Union: l1) (Union: l2)) 
         (=> unmatch)
         (unless (= (length l1) (length l2))
           (unmatch))
         (infer/int/union l1 l2 mapping flag)]
        
        ;; new impl for functions
        [(list (Function: (list t-arr ...))
               (Function: (list s-arr ...)))
         (=> unmatch)
         (let loop ([t-arr t-arr] [s-arr s-arr] [mapping mapping])
           (define (U a b) ((table:un flag) a b))
           (define (unmatch!) (unmatch))
           (cond [(and (null? t-arr) (null? s-arr)) mapping]
                 [(or (null? t-arr) (null? s-arr)) (unmatch!)]
                 [else (match (list (car t-arr) (car s-arr))
                         [(list (arr: ts t t-rest t-thn-eff t-els-eff) (arr: ss s s-rest s-thn-eff s-els-eff))
                          (let ([arg-mapping 
                                 (cond [(and t-rest s-rest (= (length ts) (length ss)))
                                        (infer/int/list (cons t-rest ts) (cons s-rest ss) mapping (swap flag))]
                                       [(and (not t-rest) (not s-rest) (= (length ts) (length ss)))
                                        (infer/int/list ts ss mapping (swap flag))]
                                       [(and t-rest (not s-rest) (<= (length ts) (length ss)))
                                        (infer/int/list/vararg ts t-rest ss mapping (swap flag) #t)]
                                       [(and s-rest (not t-rest) (>= (length ts) (length ss)))
                                        (infer/int/list/vararg ss s-rest ts mapping (swap flag) #f)]
                                       [else (unmatch!)])]
                                [ret-mapping (infer/int t s mapping flag)]
                                [thn-mapping (infer/int/list/eff t-thn-eff s-thn-eff mapping flag)]
                                [els-mapping (infer/int/list/eff t-els-eff s-els-eff mapping flag)])
                            (loop (cdr t-arr) (cdr s-arr)
                                  (U mapping (U (U arg-mapping ret-mapping) (U thn-mapping els-mapping)))))])]))]
        ;; arrow types - just add a whole bunch of new constraints
        #;
        [(list (Function: (list (arr: ts t t-rest t-thn-eff t-els-eff) ...))
               (Function: (list (arr: ss s s-rest s-thn-eff s-els-eff) ...)))
         (=> unmatch)
         (define (compatible-rest t-rest s-rest)
           (andmap (lambda (x y) (or (and x y) (and (not x) (not y)))) ;; either both #f or both not #f
                   t-rest s-rest))
         (define (U a b) ((table:un flag) a b))
         (let-values ([(s-thn-eff s-els-eff) (if (and (null? (car t-thn-eff)) (null? (cdr t-thn-eff))
                                                      (null? (car t-els-eff)) (null? (cdr t-els-eff)))
                                                 (values (list null) (list null))
                                                 (values s-thn-eff s-els-eff))])
           (unless (and (= (length ts) (length ss))
                        (= (length t-thn-eff) (length s-thn-eff))
                        (= (length t-els-eff) (length s-els-eff))
                        (compatible-rest t-rest s-rest))
             (unmatch))
           (let ([arg-mapping (infer/int/list (apply append ts) (apply append ss) mapping (swap flag))]
                 [ret-mapping (infer/int/list t s mapping flag)]
                 [thn-mapping (infer/int/list/eff (apply append t-thn-eff) (apply append s-thn-eff) mapping flag)]
                 [els-mapping (infer/int/list/eff (apply append t-els-eff) (apply append s-els-eff) mapping flag)])             
             (U (U arg-mapping ret-mapping) (U thn-mapping els-mapping))))]
        ;; here, we try to handle a case-lambda as the argument to a polymorphic function
        [(list (Function: ftys) (and t (Function: (list (arr: ss s s-rest s-thn-eff s-els-eff)))))
         (=> unmatch)
         (when (= 1 (length ftys)) (unmatch)) ;; we should have handled this case already
         (or 
          (ormap
           (lambda (fty)
             (with-handlers
                 ([exn:infer? (lambda _ #f)])
               (infer/int (make-Function (list fty)) t mapping flag)))
           ftys)
          (fail!))]
        [(list (and t (Function: (list (arr: ss s s-rest s-thn-eff s-els-eff)))) (Function: ftys))
         (=> unmatch)          
         (when (= 1 (length ftys))  (unmatch)) ;; we should have handled this case already
         (or 
          (ormap
           (lambda (fty)
             (with-handlers
                 ([exn:infer? (lambda _ #f)])
               (infer/int t (make-Function (list fty)) mapping flag)))
           ftys)
          (fail!))]
        ;; if t is a union, all of the elements have to match
        [(list s (Union: e1))
         (infer/int/list (map (lambda (_) s) e1) e1 mapping flag)]
        ;; if s is a union, we can just try to find one of its elements that works
        [(list (Union: e1) t) 
         (or 
          (ormap
           (lambda (e)
             (with-handlers
                 ([exn:infer? (lambda _ #f)])
               (infer/int e t mapping flag)))
           e1)
          (fail!))]
        ;; otherwise, if we have a {sub,super}type, we're all good
        [else (cond [(and (co? flag) (subtype t s)) mapping]
                    [(and (contra? flag) (subtype s t)) mapping]
                    ;; or, nothing worked, and we fail
                    [else (fail!)])]
        ))))

;; infer: Type Type List[Symbol] -> Substitution
(define infer (mk-infer infer/int))
;; infer/list: Listof[Type] Listof[Type] List[Symbol] -> Substitution
(define infer/list (mk-infer infer/int/list))

;(trace infer infer/int/list infer/int infer/list)
