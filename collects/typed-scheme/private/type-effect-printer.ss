#lang scheme/base

(require "../utils/utils.ss")
(require (rep type-rep effect-rep rep-utils)
	 (utils tc-utils)
	 scheme/match)

;; do we attempt to find instantiations of polymorphic types to print? 
;; FIXME - currently broken
(define print-poly-types? #f)
;; do we use simple type aliases in printing
(define print-aliases #t)

;; does t have a type name associated with it currently?
;; has-name : Type -> Maybe[Symbol]
(define (has-name? t) 
  (define ns ((current-type-names)))
  (let/cc return
    (unless print-aliases
      (return #f))
    (for-each
     (lambda (pair)
       (cond [(eq? t (cdr pair))
              (return (car pair))]))
     ns)
    #f))

;; print out an effect
;; print-effect : Effect Port Boolean -> Void
(define (print-effect c port write?)
  (define (fp . args) (apply fprintf port args))    
  (match c
    [(Restrict-Effect: t v) (fp "(restrict ~a ~a)" t (syntax-e v))]
    [(Remove-Effect: t v) (fp "(remove ~a ~a)" t (syntax-e v))]
    [(Latent-Restrict-Effect: t) (fp "(restrict ~a)" t)]
    [(Latent-Remove-Effect: t) (fp "(remove ~a)" t)]
    [(Latent-Var-True-Effect:) (fp "(var #t)")]
    [(Latent-Var-False-Effect:) (fp "(var #f)")]
    [(True-Effect:) (fp "T")]
    [(False-Effect:) (fp "F")]
    [(Var-True-Effect: v) (fp "(var #t ~a)" (syntax-e v))]
    [(Var-False-Effect: v) (fp "(var #f ~a)" (syntax-e v))]))


;; print out a type
;; print-type : Type Port Boolean -> Void
(define (print-type c port write?)
  (define (fp . args) (apply fprintf port args)) 
  (define (print-arr a)
    (match a
      [(top-arr:)
       (fp "Procedure")]
      [(arr: dom rng rest drest kws thn-eff els-eff)
       (fp "(")
       (for-each (lambda (t) (fp "~a " t)) dom)
       (for ([kw kws])
         (match kw
           [(Keyword: k t req?)
            (if req?
                (fp "~a ~a " k t)
                (fp "[~a ~a] " k t))]))
       (when rest
         (fp "~a* " rest))
       (when drest
         (fp "~a ... ~a " (car drest) (cdr drest)))
       (fp "-> ~a" rng)
       (match* (thn-eff els-eff)
         [((list) (list)) (void)]
         [((list (Latent-Restrict-Effect: t)) (list (Latent-Remove-Effect: t))) (fp " : ~a" t)]
         [(_ _) (fp " : ~a ~a" thn-eff els-eff)])
       (fp ")")]))
  (define (tuple? t)
    (match t
      [(Pair: a (? tuple?)) #t]
      [(Value: '()) #t]
      [_ #f]))
  (define (tuple-elems t)
    (match t
      [(Pair: a e) (cons a (tuple-elems e))]
      [(Value: '()) null]))
  (match c 
    [(Univ:) (fp "Any")]
    [(? has-name?) (fp "~a" (has-name? c))]
    ;; names are just the printed as the original syntax
    [(Name: stx) (fp "~a" (syntax-e stx))]
    [(App: rator rands stx) 
     (fp "~a" (cons rator rands))]
    ;; special cases for lists
    [(Mu: var (Union: (list (Value: '()) (Pair: elem-ty (F: var)))))
     (fp "(Listof ~a)" elem-ty)]
    [(Mu: var (Union: (list (Pair: elem-ty (F: var)) (Value: '()))))
     (fp "(Listof ~a)" elem-ty)]
    [(Value: v) (cond [(or (symbol? v) (null? v))
                       (fp "'~a" v)]
                      [else (fp "~a" v)])]
    [(? tuple? t)
     (fp "~a" (cons 'List (tuple-elems t)))]
    [(Base: n cnt) (fp "~a" n)]      
    [(Opaque: pred _) (fp "(Opaque ~a)" (syntax->datum pred))]
    [(Struct: 'Promise par (list fld) proc _ _ _) (fp "(Promise ~a)" fld)]      
    [(Struct: nm par flds proc _ _ _) 
     (fp "#(struct:~a ~a" nm flds)
     (when proc
       (fp " ~a" proc))
     (fp ")")]
    [(Function: arities)
     (let ()         
       (match arities
         [(list) (fp "(case-lambda)")]
         [(list a) (print-arr a)]
         [(list a b ...) (fp "(case-lambda ")
                         (print-arr a) 
                         (for-each 
                          (lambda (e) (fp " ") (print-arr e))
                          b)
                         (fp ")")]))]
    [(arr: _ _ _ _ _ _ _) (print-arr c)]
    [(Vector: e) (fp "(Vectorof ~a)" e)]
    [(Box: e) (fp "(Box ~a)" e)]
    [(Union: elems) (fp "~a" (cons 'U elems))]
    [(Pair: l r) (fp "(Pair ~a ~a)" l r)]
    [(F: nm) (fp "~a" nm)]      
    [(Values: (list v ...)) (fp "~a" (cons 'values v))]
    [(ValuesDots: v dty dbound) (fp "~a" (cons 'values (append v (list dty '... dbound))))]
    [(Param: in out) 
     (if (equal? in out)
         (fp "(Parameter ~a)" in)           
         (fp "(Parameter ~a ~a)" in out))]
    [(Hashtable: k v) (fp "(HashTable ~a ~a)" k v)]
    
    #;[(Poly-unsafe: n b) (fp "(unsafe-poly ~a ~a ~a)" (Type-seq c) n b)]
    [(Poly-names: names body) 
     #;(fprintf (current-error-port) "POLY SEQ: ~a~n" (Type-seq body))
     (fp "(All ~a ~a)" names body)]
    #;[(PolyDots-unsafe: n b) (fp "(unsafe-polydots ~a ~a ~a)" (Type-seq c) n b)]
    [(PolyDots-names: (list names ... dotted) body)
     (fp "(All ~a ~a)" (append names (list dotted '...)) body)]
    #;
    [(Mu-unsafe: b) (fp "(unsafe-mu ~a ~a)" (Type-seq c) b)]
    [(Mu: x (Syntax: (Union: (list
                              (Base: 'Number _) 
                              (Base: 'Boolean _)
                              (Base: 'Symbol _)
                              (Base: 'String _)
                              (Mu: var (Union: (list (Value: '()) (Pair: (F: x) (F: var)))))
                              (Mu: y (Union: (list (F: x) (Pair: (F: x) (F: y)))))
                              (Vector: (F: x))
                              (Box: (F: x))))))
     (fp "SyntaxObject")]
    [(Mu-name: name body) (fp "(mu ~a ~a ~a)" (Type-seq c) name body)]
    ;; FIXME - this should not be used
    #;
    [(Scope: sc) (fp "(Scope ~a)" sc)]
    
    [(B: idx) (fp "(B ~a)" idx)]      
    [(Syntax: t) (fp "(Syntax ~a)" t)]
    [(Instance: t) (fp "(Instance ~a)" t)]
    [(Class: pf nf ms) (fp "(Class)")]
    [else (fp "Unknown Type: ~a" (struct->vector c))]
    ))

(set-box! print-type* print-type)
(set-box! print-effect* print-effect)
