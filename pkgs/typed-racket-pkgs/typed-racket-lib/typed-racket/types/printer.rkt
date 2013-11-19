#lang racket/base

;; This module provides functions for printing types and related
;; data structures such as filters and objects

(require racket/require racket/match unstable/sequence racket/string racket/promise
         (prefix-in s: srfi/1)
         (path-up "rep/type-rep.rkt" "rep/filter-rep.rkt" "rep/object-rep.rkt"
                  "rep/rep-utils.rkt" "types/subtype.rkt"
                  "utils/utils.rkt"
                  "utils/tc-utils.rkt")
         (for-syntax racket/base syntax/parse))

;; printer-type: (one-of/c 'custom 'debug)
(define-for-syntax printer-type 'custom)

(define-syntax (provide-printer stx)
  (if (eq? printer-type 'debug)
      #'(provide (rename-out [debug-printer print-type]
                             [debug-printer print-filter]
                             [debug-printer print-object]
                             [debug-printer print-pathelem]))
      #'(provide print-type print-filter print-object print-pathelem)))
(provide-printer)

(provide print-multi-line-case-> special-dots-printing? print-complex-filters?
         current-print-type-fuel current-print-unexpanded)


;; do we attempt to find instantiations of polymorphic types to print?
;; FIXME - currently broken
(define print-poly-types? #t)
;; do we use simple type aliases in printing
(define print-aliases #t)

(define print-multi-line-case-> (make-parameter #f))
(define special-dots-printing? (make-parameter #f))
(define print-complex-filters? (make-parameter #f))

;; this parameter controls how far down the type to expand type names
;; interp. 0 -> don't expand
;;         1 -> expand one level, etc.
;;    +inf.0 -> expand always
(define current-print-type-fuel (make-parameter 0))

;; this parameter allows the printer to communicate unexpanded
;; type aliases to its clients, which is used to cue the user
(define current-print-unexpanded (make-parameter (box '())))

;; does t have a type name associated with it currently?
;; has-name : Type -> Maybe[Listof<Symbol>]
(define (has-name? t)
  (cond 
   [print-aliases
    (define candidates 
      (for/list ([(n t*) (in-pairs (in-list (force (current-type-names))))]
		 #:when (and (Type? t*) (type-equal? t t*)))
	 n))
    (if (null? candidates)
	#f
	(sort candidates string>? #:key symbol->string))]
   [else #f]))

;; print-filter : Filter Port Boolean
;; Print a Filter (see filter-rep.rkt) to the given port
(define (print-filter filt port write?)
  (define (fp . args) (apply fprintf port args))
  (match filt
    [(FilterSet: thn els) (fp "(~a | ~a)" thn els)]
    [(NoFilter:) (fp "-")]
    [(NotTypeFilter: type (list) (? syntax? id))
     (fp "(! ~a @ ~a)" type (syntax-e id))]
    [(NotTypeFilter: type (list) id)
     (fp "(! ~a @ ~a)" type id)]
    [(NotTypeFilter: type path (? syntax? id))
     (fp "(! ~a @ ~a ~a)" type path (syntax-e id))]
    [(NotTypeFilter: type path id)
     (fp "(! ~a @ ~a ~a)" type path id)]
    [(TypeFilter: type (list) (? syntax? id))
     (fp "(~a @ ~a)" type (syntax-e id))]
    [(TypeFilter: type (list) id)
     (fp "(~a @ ~a)" type id)]
    [(TypeFilter: type path (? syntax? id))
     (fp "(~a @ ~a ~a)" type path (syntax-e id))]
    [(TypeFilter: type path id)
     (fp "(~a @ ~a ~a)" type path id)]
    [(Bot:) (fp "Bot")]
    [(Top:) (fp "Top")]
    [(ImpFilter: a c)
     (fp "(ImpFilter ~a ~a)" a c)]
    [(AndFilter: a)
     (fp "(AndFilter") (for ([a0 a]) (fp " ~a" a0))  (fp ")")]
    [(OrFilter: a)
     (fp "(OrFilter") (for ([a0 a]) (fp " ~a" a0)) (fp ")")]
    [else (fp "(Unknown Filter: ~a)" (struct->vector filt))]))

;; print-pathelem : PathElem Port Boolean
;; Print a PathElem (see object-rep.rkt) to the given port
(define (print-pathelem pathelem port write?)
  (define (fp . args) (apply fprintf port args))
  (match pathelem
    [(CarPE:) (fp "car")]
    [(CdrPE:) (fp "cdr")]
    [(ForcePE:) (fp "force")]
    [(StructPE: t i) (fp "(~a ~a)" t i)]
    [else (fp "(Unknown Path Element: ~a)" (struct->vector pathelem))]))

;; print-object : Object Port Boolean
;; Print an Object (see object-rep.rkt) to the given port
(define (print-object object port write?)
  (define (fp . args) (apply fprintf port args))
  (match object
    [(NoObject:) (fp "-")]
    [(Empty:) (fp "-")]
    [(Path: pes i) (fp "~a" (append pes (list i)))]
    [else (fp "(Unknown Object: ~a)" (struct->vector object))]))

;; print-union : Type LSet<Type> -> Void
;; Unions are represented as a flat list of branches. In some cases, it would
;; be nicer to print them using higher-level descriptions instead.
;; We do set coverage, with the elements of the union being what we want to
;; cover, and all the names types we know about being the sets.
(define (print-union t ignored-names)
  (match-define (Union: elems) t)
  (define valid-names
    ;; We keep only unions, and only those that are subtypes of t.
    ;; It's no use attempting to cover t with things that go outside of t.
    (filter (lambda (p)
              (match p
                [(cons name (and t* (Union: elts)))
                 (and (not (member name ignored-names))
                      (subtype t* t))]
                [_ #f]))
            (force (current-type-names))))
  ;; names and the sets themselves (not the union types)
  ;; we use srfi/1 lsets as sets, to use custom type equality.
  (define candidates
    (map (match-lambda [(cons name (Union: elts)) (cons name elts)])
         valid-names))
  ;; some types in the union may not be coverable by the candidates
  ;; (e.g. type variables, etc.)
  (define-values (uncoverable coverable)
    (apply s:lset-diff+intersection type-equal? elems (map cdr candidates)))
  ;; set cover, greedy algorithm, ~lg n approximation
  (let loop ([to-cover   coverable]
             [candidates candidates]
             [coverage   '()])
    (cond [(null? to-cover) ; done
           (define coverage-names (map car coverage))
           ;; to allow :type to cue the user on unexpanded aliases
           ;; only union types can flow here, and any of those could be expanded
           (set-box! (current-print-unexpanded)
                     (append coverage-names (unbox (current-print-unexpanded))))
           (append coverage-names uncoverable)] ; we want the names
          [else
           ;; pick the candidate that covers the most uncovered types
           (define (covers-how-many? c)
             (length (s:lset-intersection type-equal? (cdr c) to-cover)))
           (define-values (next _)
             (for/fold ([next      (car candidates)]
                        [max-cover (covers-how-many? (car candidates))])
                 ([c (in-list candidates)])
               (let ([how-many? (covers-how-many? c)])
                 (if (> how-many? max-cover)
                     (values c how-many?)
                     (values next max-cover)))))
           (loop (s:lset-difference type-equal? to-cover (cdr next))
                 (remove next candidates)
                 (cons next coverage))])))

;; format-arr : arr -> String
;; Convert an arr (see type-rep.rkt) to its printable form
(define (format-arr arr)
  (match arr
    [(top-arr:) "Procedure"]
    [(arr: dom rng rest drest kws)
     (define out (open-output-string))
     (define (fp . args) (apply fprintf out args))
     (define (fp/filter fmt ret . rest)
       (if (print-complex-filters?)
           (apply fp fmt ret rest)
           (fp "-> ~a" ret)))
     (fp "(")
     (for-each (lambda (t) (fp "~a " t)) dom)
     (for ([kw (in-list kws)])
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
       [(AnyValues:)
        (fp "-> AnyValues")]
       [(Values: (list (Result: t (FilterSet: (Top:) (Top:)) (Empty:))))
        (fp "-> ~a" t)]
       [(Values: (list (Result: t
                                (FilterSet: (TypeFilter: ft pth id)
                                            (NotTypeFilter: ft pth id))
                                (Empty:))))        
        (if (null? pth)
            (fp "-> ~a : ~a" t ft)
            (begin (fp "-> ~a : ~a @" t ft)
                   (for ([pe (in-list pth)]) (fp " ~a" pe))))]
       [(Values: (list (Result: t fs (Empty:))))
        (fp/filter "-> ~a : ~a" t fs)]
       [(Values: (list (Result: t lf lo)))
        (fp/filter "-> ~a : ~a ~a" t lf lo)]
       [_
        (fp "-> ~a" rng)])
     (fp ")")
     (get-output-string out)]
    [else (format "(Unknown Function Type: ~a)" (struct->vector arr))]))

;; print-case-lambda : Type -> String
;; Convert a case-> type to a string
(define (print-case-lambda type)
  (match type
    [(Function: arities)
     (let ()
       (match arities
         [(list) "(case->)"]
         [(list a) (format-arr a)]
         [(list a b ...)
          (define multi-line? (print-multi-line-case->))
          (format (string-append "(case-> ~a" (if multi-line? "\n        " " ") "~a)")
                  (format-arr a)
                  (string-join (map format-arr b) (if multi-line? "\n        " " ")))]))]))

;; print out a type
;; print-type : Type Port Boolean -> Void
(define (print-type type port write? [ignored-names '()])
  (define (fp . args)
    (parameterize ([current-print-type-fuel
                    (sub1 (current-print-type-fuel))])
      (apply fprintf port args)))
  (define (tuple? t)
    (match t
      [(Pair: a (? tuple?)) #t]
      [(Value: '()) #t]
      [_ #f]))
  (define (tuple-elems t)
    (match t
      [(Pair: a e) (cons a (tuple-elems e))]
      [(Value: '()) null]))
  (match type
    ;; if we know how it was written, print that
    [(? Rep-stx a)
     (fp "~a" (syntax->datum (Rep-stx a)))]
    [(Univ:) (fp "Any")]
    ;; names are just the printed as the original syntax
    [(Name: stx) (fp "~a" (syntax-e stx))]
    ;; If a type has a name, then print it with that name.
    ;; However, we expand the alias in some cases
    ;; (i.e., the fuel is > 0) for the :type form.
    [(app has-name? (? values names))
     (=> fail)
     (when (not (null? ignored-names)) (fail))
     (define fuel (current-print-type-fuel))
     (cond [(> fuel 0)
            (parameterize ([current-print-type-fuel (sub1 fuel)])
              ;; if we still have fuel, print the expanded type and
              ;; add the name to the ignored list so that the union
              ;; printer does not try to print with the name.
              (print-type type port write? (append names ignored-names)))]
           [else
            ;; to allow :type to cue the user on unexpanded aliases
            (when (Union? type) ; only unions can be expanded
              (set-box! (current-print-unexpanded)
                        (cons (car names) (unbox (current-print-unexpanded)))))
            (fp "~a" (car names))])]
    [(StructType: (Struct: nm _ _ _ _ _)) (fp "(StructType ~a)" (syntax-e nm))]
    [(StructTop: (Struct: nm _ _ _ _ _)) (fp "(Struct ~a)" (syntax-e nm))]
    [(BoxTop:) (fp "BoxTop")]
    [(ChannelTop:) (fp "ChannelTop")]
    [(ThreadCellTop:) (fp "ThreadCellTop")]
    [(VectorTop:) (fp "VectorTop")]
    [(HashtableTop:) (fp "HashTableTop")]
    [(MPairTop:) (fp "MPairTop")]
    [(Prompt-TagTop:) (fp "Prompt-TagTop")]
    [(Continuation-Mark-KeyTop:) (fp "Continuation-Mark-KeyTop")]
    [(App: rator rands stx)
     (fp "~a" (list* rator rands))]
    ;; special cases for lists
    [(Mu: var (Union: (list (Value: '()) (Pair: elem-ty (F: var)))))
     (fp "(Listof ~a)" elem-ty)]
    [(Mu: var (Union: (list (Pair: elem-ty (F: var)) (Value: '()))))
     (fp "(Listof ~a)" elem-ty)]
    [(Mu: var (Union: (list (Value: '()) (MPair: elem-ty (F: var)))))
     (fp "(MListof ~a)" elem-ty)]
    [(Mu: var (Union: (list (MPair: elem-ty (F: var)) (Value: '()))))
     (fp "(MListof ~a)" elem-ty)]
    [(Value: v) (cond [(or (symbol? v) (null? v))
                       (fp "'~a" v)]
                      [else (fp "~a" v)])]
    [(? tuple? t)
     (fp "~a" (cons 'List (tuple-elems t)))]
    [(Base: n cnt _ _) (fp "~s" n)]
    [(Opaque: pred) (fp "(Opaque ~a)" (syntax->datum pred))]
    [(Struct: nm       par (list (fld: t _ _) ...)       proc _ _)
     (fp "#(struct:~a ~a" nm t)
     (when proc
       (fp " ~a" proc))
     (fp ")")]
    [(Function: arities)
     (define fun-type
       (parameterize ([current-print-type-fuel
                       (sub1 (current-print-type-fuel))])
         (print-case-lambda type)))
     (fp "~a" fun-type)]
    [(arr: _ _ _ _ _) (fp "(arr ~a)" (format-arr type))]
    [(Vector: e) (fp "(Vectorof ~a)" e)]
    [(HeterogeneousVector: e) (fp "(Vector")
                              (for ([i (in-list e)])
                                (fp " ~a" i))
                              (fp ")")]
    [(Box: e) (fp "(Boxof ~a)" e)]
    [(Future: e) (fp "(Futureof ~a)" e)]
    [(Channel: e) (fp "(Channelof ~a)" e)]
    [(ThreadCell: e) (fp "(ThreadCellof ~a)" e)]
    [(Promise: e) (fp "(Promise ~a)" e)]
    [(Ephemeron: e) (fp "(Ephemeronof ~a)" e)]
    [(CustodianBox: e) (fp "(CustodianBoxof ~a)" e)]
    [(Set: e) (fp "(Setof ~a)" e)]
    [(Evt: r) (fp "(Evtof ~a)" r)]
    [(Union: elems) (fp "~a" (cons 'U (print-union type ignored-names)))]
    [(Pair: l r) (fp "(Pairof ~a ~a)" l r)]
    [(ListDots: dty dbound)
     (fp "(List ~a ...~a~a)" dty (if (special-dots-printing?) "" " ") dbound)]
    [(F: nm) (fp "~a" nm)]
    ;; FIXME (Values are not types and shouldn't need to be considered here
    [(AnyValues:) (fp "AnyValues")]
    [(Values: (list v)) (fp "~a" v)]
    [(Values: (list v ...)) (fp "~s" (cons 'values v))]
    [(ValuesDots: v dty dbound)
     (fp "~s" (cons 'values (append v (list dty '... dbound))))]
    [(Param: in out)
     (if (equal? in out)
         (fp "(Parameterof ~a)" in)
         (fp "(Parameterof ~a ~a)" in out))]
    [(Hashtable: k v) (fp "(HashTable ~a ~a)" k v)]
    [(Continuation-Mark-Keyof: rhs)
     (fp "(Continuation-Mark-Keyof ~a)" rhs)]
    [(Prompt-Tagof: body handler)
     (fp "(Prompt-Tagof ~a ~a)" body handler)]
    #;[(Poly-unsafe: n b) (fp "(unsafe-poly ~a ~a ~a)" (Type-seq c) n b)]
    [(Poly-names: names body)
     #;(eprintf "POLY SEQ: ~a\n" (Type-seq body))
     (fp "(All ~a ~a)" names body)]
    #;
    [(PolyDots-unsafe: n b) (fp "(unsafe-polydots ~a ~a ~a)" (Type-seq c) n b)]
    [(PolyDots-names: (list names ... dotted) body)
     (fp "(All ~a ~a)" (append names (list dotted '...)) body)]
    #;
    [(Mu-unsafe: b) (fp "(unsafe-mu ~a ~a)" (Type-seq c) b)]
    [(Mu: x (Syntax: (Union: (list
                              (Base: 'Number _ _ _)
                              (Base: 'Boolean _ _ _)
                              (Base: 'Symbol _ _ _)
                              (Base: 'String _ _ _)
                              (Mu: var (Union: (list (Value: '())
                                                     (Pair: (F: x) (F: var)))))
                              (Mu: y (Union: (list (F: x)
                                                   (Pair: (F: x) (F: y)))))
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
    [(MPair: s t) (fp "(MPairof ~a ~a)" s t)]
    [(Refinement: parent p?)
     (fp "(Refinement ~a ~a)" parent (syntax-e p?))]
    [(Sequence: ts)
     (fp "(Sequenceof")
     (for ([t ts]) (fp " ~a" t))
     (fp ")")]
    [(Error:) (fp "Error")]
    [(fld: t a m) (fp "(fld ~a)" t)]
    [else (fp "(Unknown Type: ~a)" (struct->vector type))]
    ))



(define-syntax (define-debug-printer stx)
  (syntax-parse stx
    [(_ debug-printer:id)
     #:when (eq? printer-type 'debug)
     #'(begin
         (require racket/pretty)
         (require mzlib/pconvert)

         (define (converter v basic sub)
           (define (gen-constructor sym)
             (string->symbol (string-append "make-" (substring (symbol->string sym) 7))))
           (match v
             [(? Rep? rep)
              `(,(gen-constructor (car (vector->list (struct->vector rep))))
                ,@(map sub (Rep-values rep)))]
             [_ (basic v)]))

         (define (debug-printer v port write?)
           ((if write? pretty-write pretty-print)
            (parameterize ((current-print-convert-hook converter))
              (print-convert v))
            port)))]
    [_ #'(begin)]))

(define-debug-printer debug-printer)

