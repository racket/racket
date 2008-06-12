#lang scheme/unit

(require "type-effect-convenience.ss" "type-rep.ss" 
         "type-utils.ss" "union.ss" "tc-utils.ss" 
         "subtype.ss" "utils.ss"
         "signatures.ss"
         scheme/match)

(import restrict^ dmap^)
(export constraints^)


(define-values (fail-sym exn:infer?)
  (let ([sym (gensym)])
    (values sym (lambda (s) (eq? s sym)))))

;; why does this have to be duplicated?
;; inference failure - masked before it gets to the user program
(define-syntaxes (fail!)
  (syntax-rules ()
    [(_ s t) (raise fail-sym)]))


;; S, T types
;; X a var
(define-struct c (S X T) #:prefab)

;; Struct containing a list of cs
(define-struct clist (cs) #:prefab)

;; maps is a list of pairs of
;;    - functional maps from vars to c's
;;    - functional mappings from vars to either
;;      - a list of vars generated for ...
;;      - a clist containing possible constraints on the ... bound
;; we need a bunch of mappings for each cset to handle case-lambda
;; because case-lambda can generate multiple possible solutions, and we
;; don't want to rule them out too early
(define-struct cset (maps) #:prefab)

(define (empty-cset X)
  (make-cset (list (cons (for/hash ([x X]) (values x (make-c (Un) x Univ)))
                         (make-immutable-hash null)))))


#;
(define (lookup cset var)
  (hash-ref (cset-map cset) var (make-c (Un) var Univ)))

(define (insert cs var S T)
  (match cs
    [(struct cset (maps))
     (make-cset (for/list ([(map dmap) (in-pairs maps)])
                  (cons (hash-set map var (make-c S var T))
                        dmap)))]))

;; a stupid impl
(define (meet S T) 
  (let ([s* (restrict S T)])
    (if (and (subtype s* S)
             (subtype s* T))
        s*
        (Un))))

(define (join T U) (Un T U))


(define (c-meet c1 c2 [var #f])
  (match* (c1 c2)
    [((struct c (S X T)) (struct c (S* X* T*)))
     (unless (or var (eq? X X*))
       (int-err "Non-matching vars in c-meet: ~a ~a" X X*))
     (let ([S (join S S*)] [T (meet T T*)])
       (unless (subtype S T)
         (fail! S T))
       (make-c S (or var X) T))]))

(define (subst-all/c sub -c)
  (match -c
    [(struct c (S X T))
     (make-c (subst-all sub S)
             (F-n (subst-all sub (make-F X)))
             (subst-all sub T))]))

(define (cset-meet x y)
  (match* (x y)
   [((struct cset (maps1)) (struct cset (maps2)))
    (let ([maps (filter values
                        (for*/list
                         ([(map1 dmap1) (in-pairs maps1)]
                          [(map2 dmap2) (in-pairs maps2)])
                         (with-handlers ([exn:infer? (lambda (_) #f)])
                           (let* ([new-dmap (hash-union dmap1 dmap2
                                                        (lambda (k vars1 vars2)
                                                          (cond [(and (list? vars1) (list? vars2))
                                                                 (unless (= (length vars1) (length vars2))
                                                                   (fail! vars1 vars2))
                                                                 vars1]
                                                                [else 
                                                                 (int-err "nyi : stars and dots together: ~a ~a" vars1 vars2)])))]
                                  [subst
                                   (apply append
                                          (for/list ([(dvar vars) dmap1])
                                            (let ([vars2 (hash-ref dmap2 dvar #f)])                                              
                                              (if vars2 (map list vars2 (map make-F vars)) null))))])
                             (cons 
                              (hash-union map1 map2 (lambda (k v1 v2) (c-meet v1 (subst-all/c subst v2))))
                              new-dmap)))))])
      (when (null? maps)
        (fail! maps1 maps2))
      (make-cset maps))]))

(define (cset-meet* X args)
  (for/fold ([c (empty-cset X)])
    ([a args])
    (cset-meet a c)))


(define (cset-combine l)
  (let ([mapss (map cset-maps l)])
    (make-cset (apply append mapss))))
