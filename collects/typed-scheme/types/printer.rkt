#lang scheme/base

(require unstable/sequence racket/require racket/match
         (path-up "rep/type-rep.rkt" "rep/filter-rep.rkt" "rep/object-rep.rkt"
                  "rep/rep-utils.rkt" "utils/utils.rkt" "utils/tc-utils.rkt"))

;; do we attempt to find instantiations of polymorphic types to print? 
;; FIXME - currently broken
(define print-poly-types? #f)
;; do we use simple type aliases in printing
(define print-aliases #t)

(define special-dots-printing? (make-parameter #f))
(define print-complex-filters? (make-parameter #f))
(provide special-dots-printing? print-complex-filters?)

;; does t have a type name associated with it currently?
;; has-name : Type -> Maybe[Symbol]
(define (has-name? t) 
  (and print-aliases
       (for/first ([(n t*) (in-pairs (in-list ((current-type-names))))]
                   #:when (and (Type? t*) (type-equal? t t*)))
         n)))

(define (print-filter c port write?)
  (define (fp . args) (apply fprintf port args))
  (match c
    [(FilterSet: thn els) (fp "(~a | ~a)" thn els)]
    [(NoFilter:) (fp "-")]
    [(NotTypeFilter: type (list) (? syntax? id)) (fp "(! ~a @ ~a)" type (syntax-e id))]
    [(NotTypeFilter: type (list) id) (fp "(! ~a @ ~a)" type id)]
    [(NotTypeFilter: type path (? syntax? id)) (fp "(! ~a @ ~a ~a)" type path (syntax-e id))]
    [(NotTypeFilter: type path id) (fp "(! ~a @ ~a ~a)" type path id)]
    [(TypeFilter: type (list) (? syntax? id)) (fp "(~a @ ~a)" type (syntax-e id))]
    [(TypeFilter: type (list) id) (fp "(~a @ ~a)" type id)]
    [(TypeFilter: type path (? syntax? id)) (fp "(~a @ ~a ~a)" type path (syntax-e id))]
    [(TypeFilter: type path id) (fp "(~a @ ~a ~a)" type path id)]
    [(Bot:) (fp "Bot")]
    [(Top:) (fp "Top")]
    [(ImpFilter: a c) (fp "(ImpFilter ~a ~a)" a c)]
    [(AndFilter: a) (fp "(AndFilter") (for ([a0 a]) (fp " ~a" a0))  (fp ")")]
    [(OrFilter: a) (fp "(OrFilter") (for ([a0 a]) (fp " ~a" a0)) (fp ")")]
    [else (fp "(Unknown Filter: ~a)" (struct->vector c))]))

(define (print-pathelem c port write?)
  (define (fp . args) (apply fprintf port args))
  (match c
    [(CarPE:) (fp "car")]
    [(CdrPE:) (fp "cdr")]
    [(StructPE: t i) (fp "(~a ~a)" t i)]
    [else (fp "(Unknown Path Element: ~a)" (struct->vector c))]))

(define (print-object c port write?)
  (define (fp . args) (apply fprintf port args))
  (match c
    [(NoObject:) (fp "-")]
    [(Empty:) (fp "-")]
    [(Path: pes i) (fp "~a" (append pes (list i)))]    
    [else (fp "(Unknown Object: ~a)" (struct->vector c))]))

;; print out a type
;; print-type : Type Port Boolean -> Void
(define (print-type c port write?)
  (define (fp . args) (apply fprintf port args)) 
  (define (fp/filter fmt ret . rest)
    (if (print-complex-filters?)
        (apply fp fmt ret rest)
        (fp "-> ~a" ret)))
  (define (print-arr a)
    (match a
      [(top-arr:)
       (fp "Procedure")]
      [(arr: dom rng rest drest kws)
       (fp "(")
       (for-each (lambda (t) (fp "~a " t)) dom)
       (for ([kw kws])
         (match kw
           [(Keyword: k t req?)
            (if req?
                (fp "~a ~a " k t)
                (fp "[~a ~a] " k t))]))
       (when rest
         (fp "~a ~a " rest (if (special-dots-printing?) "...*" "*")))
       (when drest
         (fp "~a ...~a~a " 
             (car drest) (if (special-dots-printing?) "" " ") (cdr drest)))
       (match rng
         [(Values: (list (Result: t (FilterSet: (Top:) (Top:)) (Empty:))))
          (fp "-> ~a" t)]
         [(Values: (list (Result: t
				  (FilterSet: (TypeFilter: ft pth id)
					      (NotTypeFilter: ft pth id))
				  (Empty:)))) 
          (if (null? pth)
              (fp "-> ~a : ~a" t ft)
              (begin (fp "-> ~a : ~a @" t ft)
                     (for ([pe pth]) (fp " ~a" pe))))]
         [(Values: (list (Result: t fs (Empty:)))) 
          (fp/filter "-> ~a : ~a" t fs)]
         [(Values: (list (Result: t lf lo)))
          (fp/filter "-> ~a : ~a ~a" t lf lo)]
         [_
          (fp "-> ~a" rng)])
       (fp ")")]      
      [else (fp "(Unknown Function Type: ~a)" (struct->vector a))]))
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
    ;; if we know how it was written, print that
    [(? Rep-stx a)
     (fp "~a" (syntax->datum (Rep-stx a)))]
    [(Univ:) (fp "Any")]
    ;; special case number until something better happens
    ;;[(Base: 'Number _) (fp "Number")]
    [(app has-name? (? values name))
     (fp "~a" name)]
    [(StructTop: st) (fp "~a" st)]
    [(BoxTop:) (fp "Box")]
    [(ChannelTop:) (fp "Channel")]
    [(VectorTop:) (fp "Vector")]
    [(MPairTop:) (fp "MPair")]
    ;; names are just the printed as the original syntax
    [(Name: stx) (fp "~a" (syntax-e stx))]
    [(App: rator rands stx) 
     (fp "~a" (list* rator rands))]
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
    [(Struct: 'Promise par (list fld) proc _ _ _ _ _) (fp "(Promise ~a)" fld)]      
    [(Struct: nm par flds proc _ _ _ _ _) 
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
    [(arr: _ _ _ _ _) (fp "(arr ") (print-arr c) (fp ")")]
    [(Vector: e) (fp "(Vectorof ~a)" e)]
    [(HeterogenousVector: e) (fp "(Vector") 
                             (for ([i (in-list e)])
                               (fp " ~a" i))
                             (fp ")")]
    [(Box: e) (fp "(Boxof ~a)" e)]
    [(Channel: e) (fp "(Channelof ~a)" e)]
    [(Union: elems) (fp "~a" (cons 'U elems))]
    [(Pair: l r) (fp "(Pairof ~a ~a)" l r)]
    [(F: nm) (fp "~a" nm)]   
    ;; FIXME
    [(Values: (list v)) (fp "~a" v)]
    [(Values: (list v ...)) (fp "~a" (cons 'values v))]
    [(ValuesDots: v dty dbound) (fp "~a" (cons 'values (append v (list dty '... dbound))))]
    [(Param: in out) 
     (if (equal? in out)
         (fp "(Parameterof ~a)" in)           
         (fp "(Parameterof ~a ~a)" in out))]
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
     (fp "Syntax")]
    [(Mu-name: name body) (fp "(Rec ~a ~a)" name body)]
    ;; FIXME - this should not be used
    #;
    [(Scope: sc) (fp "(Scope ~a)" sc)]
    
    [(B: idx) (fp "(B ~a)" idx)]      
    [(Syntax: t) (fp "(Syntaxof ~a)" t)]
    [(Instance: t) (fp "(Instance ~a)" t)]
    [(Class: pf nf ms) (fp "(Class)")]
    [(Result: t (FilterSet: (Top:) (Top:)) (Empty:)) (fp "~a" t)]
    [(Result: t fs (Empty:)) (fp "(~a : ~a)" t fs)]
    [(Result: t fs lo) (fp "(~a : ~a : ~a)" t fs lo)]
    [(Refinement: parent p? _)
     (fp "(Refinement ~a ~a)" parent (syntax-e p?))]
    [(Sequence: ts)
     (fp "(Sequenceof")
     (for ([t ts]) (fp " ~a" t))
     (fp ")")]
    [(Error:) (fp "Error")]
    [else (fp "(Unknown Type: ~a)" (struct->vector c))]
    ))

(set-box! print-type* print-type)
(set-box! print-filter* print-filter)
;(set-box! print-latentfilter* print-latentfilter)
(set-box! print-object* print-object)
;(set-box! print-latentobject* print-latentobject)
(set-box! print-pathelem* print-pathelem)
