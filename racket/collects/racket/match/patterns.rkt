#lang racket/base

(require syntax/boundmap
         racket/contract racket/list
         "stxtime.rkt"
         (for-syntax racket/base))

(provide (except-out (combine-out 
                      (all-defined-out)
                      (all-from-out "stxtime.rkt"))
                     struct-key-ht
                     get-key
                     (struct-out Row)))

(define orig-stx (make-parameter #f))

(define-struct Pat () #:transparent)
;; v is an identifier
(define-struct (Var Pat) (v)
  #:transparent
  #:property
  prop:custom-write (lambda (v p w?)
                      (fprintf p "(Var ~a)" (syntax-e (Var-v v)))))
(define-struct (Dummy Var) ()
  #:transparent
  #:property
  prop:custom-write (lambda (v p w?) (fprintf p "_")))

;; constructor patterns
(define-struct (CPat Pat) () #:transparent)

;; start is what index to start at
(define-struct (Vector CPat) (ps) #:transparent)

(define-struct (Pair CPat) (a d) #:transparent)
(define-struct (MPair CPat) (a d) #:transparent)

(define-struct (Box CPat) (p) #:transparent)

;; p is a pattern to match against the literal
;;(define-struct (Atom CPat) (p) #:transparent)
;(define-struct (String Atom) () #:transparent)
;; (define-struct (Number Atom) () #:transparent)
;; (define-struct (Symbol Atom) () #:transparent)
;; (define-struct (Keyword Atom) () #:transparent)
;; (define-struct (Char Atom) () #:transparent)
;; (define-struct (Bytes Atom) () #:transparent)
;; (define-struct (Regexp Atom) () #:transparent)
;; (define-struct (Boolean Atom) () #:transparent)
(define-struct (Null CPat) (p) #:transparent)

;; expr is an expression
;; ps is a list of patterns
(define-struct (App Pat) (expr ps) #:transparent)

;; pred is an expression
(define-struct (Pred Pat) (pred) #:transparent
  #:property prop:equal+hash
  (list (lambda (a b e?)
          (and (identifier? (Pred-pred a)) (identifier? (Pred-pred b))
               (free-identifier=? (Pred-pred a) (Pred-pred b))))
        (lambda (v r)
          (if (identifier? (Pred-pred v))
              (r (syntax-e (Pred-pred v)))
              (r (Pred-pred v))))
        (lambda (v r)
          (if (identifier? (Pred-pred v))
              (r (syntax-e (Pred-pred v)))
              (r (Pred-pred v))))))

;; pred is an identifier
;; super is an identifier, or #f
;; complete? is a boolean
;; accessors is a listof identifiers (NB in reverse order from the struct info)
;; ps is a listof patterns
(define-struct (Struct CPat) (id pred super complete? accessors ps) #:transparent)

;; both fields are lists of pats
(define-struct (HashTable CPat) (key-pats val-pats) #:transparent)

;; ps are patterns
(define-struct (Or Pat) (ps) #:transparent)
(define-struct (And Pat) (ps) #:transparent)
(define-struct (OrderedAnd And) () #:transparent)
;; p is a pattern
(define-struct (Not Pat) (p) #:transparent)

;; headss : listof listof pattern
;; mins : listof option number
;; maxs : listof option number
;; onces? : listof boolean -- is this pattern being bound only once (take the
;;                            car of the variables)
;; tail : pattern
;; mutable? : is this for mutable lists?
(define-struct (GSeq Pat) (headss mins maxs onces? tail mutable?) #:transparent)

;; match with equal?
;; v is a quotable racket value
(define-struct (Exact Pat) (v) #:transparent)

;; pats is a Listof Pat
;; rhs is an expression
;; unmatch is an identifier
;; vars-seen is a listof identifiers
(define-struct Row (pats rhs unmatch vars-seen) #:transparent
  #:property
  prop:custom-write
  (lambda (v p w?) (fprintf p "(Row ~a <expr>)" (Row-pats v))))

(define struct-key-ht (make-free-identifier-mapping))
(define (get-key id)
  (free-identifier-mapping-get
   struct-key-ht id
   (lambda ()
     (let ([k (box-immutable (syntax-e id))])
       (free-identifier-mapping-put! struct-key-ht id k)
       k))))

;; pat-key returns either an immutable box, or a symbol., or #f
;; the result is a box iff the argument was a struct pattern
;; (eq? (pat-key p) (pat-key q)) if p and q match the same constructor
;; the result is #f if p is not a constructor pattern
(define (pat-key p)
  (cond [(Struct? p) (get-key (Struct-id p))]
        [(Box? p) 'box]
        [(Vector? p) 'vector]
        [(Pair? p) 'pair]
        [(MPair? p) 'mpair]
        [(Null? p) 'null]
        [else #f]))

;; (require mzlib/trace)
;; (trace pat-key)

;; Row-first-pat : Row -> Pat
;; Row must not have empty list of pats
(define (Row-first-pat r)
  (car (Row-pats r)))

(define (Row-split-pats r)
  (define p (Row-pats r))
  (values (car p) (cdr p)))

;; merge : (liftof (listof id)) -> (listof id)
;;  merges lists of identifiers, removing module-identifier=? duplicates
(define (merge l)
  (cond [(null? l) null]
        [(null? (cdr l)) (car l)]
        [else (let ([m (make-module-identifier-mapping)]
                    [in-order null])
                (for* ([ids l] [id ids])
                  (unless (module-identifier-mapping-get m id (lambda () #f))
                    (set! in-order (cons id in-order))
                    (module-identifier-mapping-put! m id #t)))
                (reverse in-order))]))
;; bound-vars : Pat -> listof identifiers
(define (bound-vars p)
  (cond
    [(Dummy? p) null]
    [(Pred? p) null]
    [(Var? p)
     (let ([v (Var-v p)])
       (list (free-identifier-mapping-get (current-renaming) v (lambda () v))))]
    [(Or? p)
     (bound-vars (car (Or-ps p)))]
    [(Box? p)
     (bound-vars (Box-p p))]
    [(Null? p) null]
    [(Pair? p)
     (merge (list (bound-vars (Pair-a p)) (bound-vars (Pair-d p))))]
    [(MPair? p)
     (merge (list (bound-vars (MPair-a p)) (bound-vars (MPair-d p))))]
    [(GSeq? p)
     (merge (cons (bound-vars (GSeq-tail p))
                  (for/list ([pats (GSeq-headss p)])
                    (merge (for/list ([pat pats])
                             (bound-vars pat))))))]
    [(Vector? p)
     (merge (map bound-vars (Vector-ps p)))]
    [(Struct? p)
     (merge (map bound-vars (Struct-ps p)))]
    [(App? p)
     (merge (map bound-vars (App-ps p)))]
    [(Not? p) null]
    [(And? p)
     (merge (map bound-vars (And-ps p)))]
    [(Exact? p) null]
    [else (error 'match "bad pattern: ~a" p)]))

(define (pats->bound-vars parse-id pats)
  (remove-duplicates
   (foldr (λ (pat vars) (append (bound-vars (parse-id pat)) vars)) '() pats)
   bound-identifier=?))

(define current-renaming (make-parameter (make-free-identifier-mapping)))

(define (copy-mapping ht)
  (define new-ht (make-free-identifier-mapping))
  (free-identifier-mapping-for-each
   ht (lambda (k v) (free-identifier-mapping-put! new-ht k v)))
  new-ht)

#|
;; EXAMPLES

(define p-x (make-Var #'x))
(define p-y (make-Var #'y))
(define p-d (make-Dummy #'_))

(define p-cons (make-Pair p-x p-y))
(define p-vec (make-Vector (list p-x p-y p-d)))

(define r1 (make-Row (list p-x) #'#f #f null))
(define r2 (make-Row (list p-y) #'#f #f null))
(define r3 (make-Row (list p-cons) #'#f #f null))
(define r4 (make-Row (list p-vec p-d) #'#f #f null))
|#

(provide/contract (struct Row ([pats (listof Pat?)]
                               [rhs syntax?]
                               [unmatch (or/c identifier? false/c)]
                               [vars-seen (listof (cons/c identifier?
                                                          (or/c #f identifier?)))])))

