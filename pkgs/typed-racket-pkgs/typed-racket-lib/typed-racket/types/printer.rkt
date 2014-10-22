#lang racket/base

;; This module provides functions for printing types and related
;; data structures such as filters and objects

(require racket/require racket/match unstable/sequence racket/string racket/promise
         racket/pretty
         racket/list
         (prefix-in s: srfi/1)
         (path-up "rep/type-rep.rkt" "rep/filter-rep.rkt" "rep/object-rep.rkt"
                  "rep/rep-utils.rkt" "types/subtype.rkt"
                  "types/match-expanders.rkt"
                  "types/kw-types.rkt"
                  "types/utils.rkt"
                  "types/resolve.rkt"
                  "utils/utils.rkt"
                  "utils/tc-utils.rkt"
                  "env/type-name-env.rkt")
         (for-syntax racket/base syntax/parse))

;; printer-type: (one-of/c 'custom 'debug)
(define-for-syntax printer-type 'custom)

(define-syntax (provide-printer stx)
  (if (eq? printer-type 'debug)
      #'(provide (rename-out [debug-printer print-type]
                             [debug-printer print-filter]
                             [debug-printer print-object]
                             [debug-printer print-pathelem]
                             [debug-pretty-format-type pretty-format-type]))
      #'(provide print-type print-filter print-object print-pathelem
                 pretty-format-type)))
(provide-printer)

(provide print-complex-filters? type-output-sexpr-tweaker
         current-print-type-fuel current-print-unexpanded)


;; do we attempt to find instantiations of polymorphic types to print?
;; FIXME - currently broken
(define print-poly-types? #t)
;; do we use simple type aliases in printing
(define print-aliases #t)

(define type-output-sexpr-tweaker (make-parameter values))
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
  (define candidates
    (for/list ([(n t*) (in-pairs (in-list (force (current-type-names))))]
               #:when (and print-aliases (Type? t*) (type-equal? t t*)))
      n))
  (and (pair? candidates)
       (sort candidates string>? #:key symbol->string #:cache-keys? #t)))

;; print-<thing> : <thing> Output-Port Boolean -> Void
;; print-type also takes an optional (Listof Symbol)
;;
;; These four functions call the helpers below to print an
;; s-expression representation of the given type/pathelem/filter/object.
(define (print-type type port write? [ignored-names '()])
  (display (type->sexp type ignored-names) port))

(define (print-pathelem pe port write?)
  (display (pathelem->sexp pe) port))

(define (print-filter filter port write?)
  (display (filter->sexp filter) port))

(define (print-object obj port write?)
  (display (object->sexp obj) port))

;; Table for formatting pretty-printed types
(define type-style-table
  (pretty-print-extend-style-table
   #f '(U All -> ->*) '(and lambda and and)))

;; pretty-format-type : Type -> String
;; Formats the type using pretty printing
(define (pretty-format-type type #:indent [indent 0])
  (define out (open-output-string))
  (port-count-lines! out)
  (write-string (make-string indent #\space) out)
  (parameterize ([pretty-print-current-style-table type-style-table])
    (pretty-display ((type-output-sexpr-tweaker) (type->sexp type '()))
                    out))
  (string-trim #:left? #f (substring (get-output-string out) indent)))

;; filter->sexp : Filter -> S-expression
;; Print a Filter (see filter-rep.rkt) to the given port
(define (filter->sexp filt)
  (define (name-ref->sexp name-ref)
    (if (syntax? name-ref)
        (syntax-e name-ref)
        name-ref))
  (define (path->sexps path)
    (if (null? path)
        '()
        (list (map pathelem->sexp path))))
  (match filt
    [(FilterSet: thn els) `(,(filter->sexp thn) \| ,(filter->sexp els))]
    [(NoFilter:) '-]
    [(NotTypeFilter: type (Path: path nm))
     `(! ,(type->sexp type) @ ,@(path->sexps path) ,(name-ref->sexp nm))]
    [(TypeFilter: type (Path: path nm))
     `(,(type->sexp type) @ ,@(path->sexps path) ,(name-ref->sexp nm))]
    [(Bot:) 'Bot]
    [(Top:) 'Top]
    [(ImpFilter: a c)
     `(ImpFilter ,(filter->sexp a) ,(filter->sexp c))]
    [(AndFilter: a) `(AndFilter ,@(map filter->sexp a))]
    [(OrFilter: a) `(OrFilter ,@(map filter->sexp a))]
    [else `(Unknown Filter: ,(struct->vector filt))]))

;; pathelem->sexp : PathElem -> S-expression
;; Print a PathElem (see object-rep.rkt) to the given port
(define (pathelem->sexp pathelem)
  (match pathelem
    [(CarPE:) 'car]
    [(CdrPE:) 'cdr]
    [(ForcePE:) 'force]
    [(StructPE: t i) `(,(pathelem->sexp t) ,i)]
    [else `(Unknown Path Element: ,(struct->vector pathelem))]))

;; object->sexp : Object -> S-expression
;; Print an Object (see object-rep.rkt) to the given port
(define (object->sexp object)
  (match object
    [(NoObject:) '-]
    [(Empty:) '-]
    [(Path: pes i) (append (map pathelem->sexp pes) (list i))]
    [else `(Unknown Object: ,(struct->vector object))]))

;; cover-union : Type LSet<Type> -> Listof<Symbol> Listof<Type>
;; Unions are represented as a flat list of branches. In some cases, it would
;; be nicer to print them using higher-level descriptions instead.
;; We do set coverage, with the elements of the union being what we want to
;; cover, and all the names types we know about being the sets.
(define (cover-union t ignored-names)
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
           (values coverage-names uncoverable)] ; we want the names
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

;; arr->sexp : arr -> s-expression
;; Convert an arr (see type-rep.rkt) to its printable form
(define (arr->sexp arr)
  (match arr
    [(arr: dom rng rest drest kws)
     (append
      (list '->)
      (map type->sexp dom)
      ;; Format keyword types as strings because the square
      ;; brackets are significant for printing. Note that
      ;; as long as the resulting s-expressions are `display`ed
      ;; this is fine, though it may not pretty-print well.
      (for/list ([kw (in-list kws)])
        (match kw
          [(Keyword: k t req?)
           (if req?
               (format "~a ~a" k (type->sexp t))
               (format "[~a ~a]" k (type->sexp t)))]))
      (if rest  `(,(type->sexp rest) *)                       null)
      (if drest `(,(type->sexp (car drest)) ... ,(cdr drest)) null)
      (match rng
        [(AnyValues: (Top:)) '(AnyValues)]
        [(AnyValues: f) `(AnyValues : ,(filter->sexp f))]
        [(Values: (list (Result: t (FilterSet: (Top:) (Top:)) (Empty:))))
         (list (type->sexp t))]
        [(Values: (list (Result: t
                                 (FilterSet: (TypeFilter: ft (Path: pth (list 0 0)))
                                             (NotTypeFilter: ft (Path: pth (list 0 0))))
                                 (Empty:))))
         ;; Only print a simple filter for single argument functions,
         ;; since parse-type only accepts simple latent filters on single
         ;; argument functions.
         #:when (= 1 (length dom))
         (if (null? pth)
             `(,(type->sexp t) : ,(type->sexp ft))
             `(,(type->sexp t) : ,(type->sexp ft) @
               ,@(map pathelem->sexp pth)))]
        ;; Print asymmetric filters with only a positive filter as a
        ;; special case (even when complex printing is off) because it's
        ;; useful to users who use functions like `filter`.
        [(Values: (list (Result: t
                                 (FilterSet: (TypeFilter: ft (Path: '() (list 0 0))) (Top:))
                                 (Empty:))))
         #:when (= 1 (length dom))
         `(,(type->sexp t) : #:+ ,(type->sexp ft))]
        [(Values: (list (Result: t fs (Empty:))))
         (if (print-complex-filters?)
             `(,(type->sexp t) : ,(filter->sexp fs))
             (list (type->sexp t)))]
        [(Values: (list (Result: t lf lo)))
         (if (print-complex-filters?)
             `(,(type->sexp t) : ,(filter->sexp lf) ,(object->sexp lo))
             (list (type->sexp t)))]
        [_ (list (type->sexp rng))]))]
    [else `(Unknown Function Type: ,(struct->vector arr))]))

;; format->* : (Listof arr) -> S-Expression
;; Format arrs that correspond to a ->* type
(define (format->* arrs)
  ;; see type-contract.rkt, which does something similar and this code
  ;; was stolen from/inspired by/etc.
  (match* ((first arrs) (last arrs))
    [((arr: first-dom rng rst _ kws)
      (arr: last-dom _ _ _ _))
     (define-values (mand-kws opt-kws) (partition-kws kws))
     (define opt-doms (drop last-dom (length first-dom)))
     `(->*
       ,(append* (for/list ([dom (in-list first-dom)])
                   (type->sexp dom))
                 (for/list ([mand-kw (in-list mand-kws)])
                   (match-define (Keyword: k t _) mand-kw)
                   (list k (type->sexp t))))
       ,(append* (for/list ([opt-dom (in-list opt-doms)])
                   (type->sexp opt-dom))
                 (for/list ([opt-kw (in-list opt-kws)])
                   (match-define (Keyword: k t _) opt-kw)
                   (list k (type->sexp t))))
       ,@(if rst (list '#:rest (type->sexp rst)) null)
       ,(type->sexp rng))]))

;; cover-case-lambda : (Listof arr) -> (Listof s-expression)
;; Try to cover a case-> type with ->* types
(define (cover-case-lambda arrs)
  ;; sublists : (Listof X) -> (Listof (List (Listof X) (Listof X) (Listof X)))
  ;; produce sublists of a list in decreasing order, also
  ;; returning the rest of the list before and after the
  ;; sublist for each.
  (define (sublists lst)
    (define (sublist-n n lst)
      (for/list ([to-drop (range (- (length lst) (- n 1)))])
        (define-values (pre mid) (split-at lst to-drop))
        (define-values (sub post) (split-at mid n))
        (list pre sub post)))
    (apply append (for/list ([i (range (length lst) 0 -1)])
                    (sublist-n i lst))))
  (let loop ([left-to-cover arrs])
    ;; try to match the largest sublists possible that correspond to
    ;; ->* types and then the remainder are formatted normally
    (define a-match
      (for/first ([sub (in-list (sublists left-to-cover))]
                  #:when (has-optional-args? (second sub)))
        sub))
    (cond [a-match
           (match-define (list pre sub post) a-match)
           (append (loop pre) (list (format->* sub)) (loop post))]
          [else (map arr->sexp left-to-cover)])))

;; case-lambda->sexp : Type -> S-expression
;; Convert a case-> type to an s-expression
(define (case-lambda->sexp type)
  (match type
    [(Function: arities)
     (match arities
       [(list) '(case->)]
       [(list a) (arr->sexp a)]
       [(and arrs (list a b ...))
        (define cover (cover-case-lambda arrs))
        (if (> (length cover) 1)
            `(case-> ,@cover)
            (car cover))])]))

;; class->sexp : Class [#:object? Boolean] -> S-expression
;; Convert a class or object type to an s-expression
(define (class->sexp cls #:object? [object? #f])
  (match-define (Class: row-var inits fields methods augments init-rest) cls)
  (define row-var*
    (if (and row-var (F? row-var)) `(#:row-var ,(F-n row-var)) '()))
  (define inits*
    (if (or object? (null? inits))
        null
        (list
         (cons 'init
               (for/list ([init inits])
                 (match-define (list name type opt?) init)
                 (if opt?
                     (list name (type->sexp type) '#:optional)
                     (list name (type->sexp type))))))))
  (define fields*
    (if (null? fields)
        null
        (list
         (cons 'field
               (for/list ([name+type (in-list fields)])
                 (match-define (list name type) name+type)
                 `(,name ,(type->sexp type)))))))
  (define methods*
    (for/list ([name+type (in-list methods)])
      (match-define (list name type) name+type)
      `(,name ,(type->sexp type))))
  (define augments*
    (cond [(or object? (null? augments)) '()]
          [else (list (cons 'augment augments))]))
  (define init-rest*
    (if (and init-rest (not object?))
        (list `(init-rest ,init-rest))
        null))
  `(,(if object? 'Object 'Class)
    ,@row-var* ,@inits* ,@init-rest* ,@fields* ,@methods* ,@augments*))

;; type->sexp : Type -> S-expression
;; convert a type to an s-expression that can be printed
(define (type->sexp type [ignored-names '()])
  (define (t->s type)
    (parameterize ([current-print-type-fuel
                    (sub1 (current-print-type-fuel))])
      (type->sexp type)))
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
    [(? Rep-stx a) (syntax->datum (Rep-stx a))]
    [(Univ:) 'Any]
    ;; struct names are just printed as the original syntax
    [(Name: id _ _ #t) (syntax-e id)]
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
              (type->sexp (if (Name? type) (resolve type) type)
                          (append names ignored-names)))]
           [else
            ;; to allow :type to cue the user on unexpanded aliases
            (when (Union? type) ; only unions can be expanded
              (set-box! (current-print-unexpanded)
                        (cons (car names) (unbox (current-print-unexpanded)))))
            (car names)])]
    [(StructType: (Struct: nm _ _ _ _ _)) `(StructType ,(syntax-e nm))]
    ;; this case occurs if the contained type is a type variable
    [(StructType: ty) `(Struct-Type ,(t->s ty))]
    [(StructTypeTop:) 'Struct-TypeTop]
    [(StructTop: (Struct: nm _ _ _ _ _)) `(Struct ,(syntax-e nm))]
    [(BoxTop:) 'BoxTop]
    [(ChannelTop:) 'ChannelTop]
    [(Async-ChannelTop:) 'Async-ChannelTop]
    [(ThreadCellTop:) 'ThreadCellTop]
    [(VectorTop:) 'VectorTop]
    [(HashtableTop:) 'HashTableTop]
    [(MPairTop:) 'MPairTop]
    [(Prompt-TagTop:) 'Prompt-TagTop]
    [(Continuation-Mark-KeyTop:) 'Continuation-Mark-KeyTop]
    [(App: rator rands stx)
     (list* (type->sexp rator) (map type->sexp rands))]
    ;; Special cases for lists. Avoid printing with these cases if the
    ;; element type refers to the Mu variable (otherwise it prints the
    ;; type variable with no binding).
    [(Listof: elem-ty var)
     #:when (not (memq var (fv elem-ty)))
     `(Listof ,(t->s elem-ty))]
    [(MListof: elem-ty var)
     #:when (not (memq var (fv elem-ty)))
     `(MListof ,(t->s elem-ty))]
    ;; format as a string to preserve reader abbreviations and primitive
    ;; values like characters (when `display`ed)
    [(Value: v) (format "~v" v)]
    [(? tuple? t)
     `(List ,@(map type->sexp (tuple-elems t)))]
    [(Base: n cnt _ _) n]
    [(Opaque: pred) `(Opaque ,(syntax->datum pred))]
    [(Struct: nm       par (list (fld: t _ _) ...)       proc _ _)
     `#(,(string->symbol (format "struct:~a" nm))
        ,(map t->s t)
        ,@(if proc (list (t->s proc)) null))]
    [(Function: arities)
     (parameterize ([current-print-type-fuel
                     (sub1 (current-print-type-fuel))])
       (case-lambda->sexp type))]
    [(arr: _ _ _ _ _) `(arr ,(arr->sexp type))]
    [(Vector: e) `(Vectorof ,(t->s e))]
    [(HeterogeneousVector: e) `(Vector ,@(map t->s e))]
    [(Box: e) `(Boxof ,(t->s e))]
    [(Future: e) `(Futureof ,(t->s e))]
    [(Channel: e) `(Channelof ,(t->s e))]
    [(Async-Channel: e) `(Async-Channelof ,(t->s e))]
    [(ThreadCell: e) `(ThreadCellof ,(t->s e))]
    [(Promise: e) `(Promise ,(t->s e))]
    [(Ephemeron: e) `(Ephemeronof ,(t->s e))]
    [(CustodianBox: e) `(CustodianBoxof ,(t->s e))]
    [(Set: e) `(Setof ,(t->s e))]
    [(Evt: r) `(Evtof ,(t->s r))]
    [(Union: elems)
     (define-values (covered remaining) (cover-union type ignored-names))
     (cons 'U (append covered (map t->s remaining)))]
    [(Pair: l r) `(Pairof ,(t->s l) ,(t->s r))]
    [(ListDots: dty dbound) `(List ,(t->s dty) ... ,dbound)]
    [(F: nm) nm]
    ;; FIXME (Values are not types and shouldn't need to be considered here
    [(AnyValues: (Top:)) 'AnyValues]
    [(AnyValues: f) `(AnyValues : ,(filter->sexp f))]
    [(Values: (list v)) v]
    [(Values: (list v ...)) (cons 'values (map t->s v))]
    [(ValuesDots: v dty dbound)
     (cons 'values (append (map t->s v) (list (t->s dty) '... dbound)))]
    [(Param: in out)
     (if (equal? in out)
         `(Parameterof ,(t->s in))
         `(Parameterof ,(t->s in) ,(t->s out)))]
    [(Hashtable: k v) `(HashTable ,(t->s k) ,(t->s v))]
    [(Continuation-Mark-Keyof: rhs)
     `(Continuation-Mark-Keyof ,(t->s rhs))]
    [(Prompt-Tagof: body handler)
     `(Prompt-Tagof ,(t->s body) ,(t->s handler))]
    [(Poly-names: names body)
     `(All ,names ,(t->s body))]
    [(PolyDots-names: (list names ... dotted) body)
     `(All ,(append names (list dotted '...)) ,(t->s body))]
    ;; FIXME: should this print constraints too
    [(PolyRow-names: names _ body)
     `(All (,(car names) #:row) ,(t->s body))]
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
     'Syntax]
    [(Mu-name: name body) `(Rec ,name ,(t->s body))]
    [(B: idx) `(B ,idx)]
    [(Syntax: t) `(Syntaxof ,(t->s t))]
    [(Instance: (and (? has-name?) cls)) `(Instance ,(t->s cls))]
    [(Instance: (? Class? cls)) (class->sexp cls #:object? #t)]
    [(ClassTop:) 'ClassTop]
    [(? Class?) (class->sexp type)]
    [(Result: t (or (NoFilter:) (FilterSet: (Top:) (Top:))) (or (NoObject:) (Empty:))) (type->sexp t)]
    [(Result: t fs (Empty:)) `(,(type->sexp t) : ,(filter->sexp fs))]
    [(Result: t fs lo) `(,(type->sexp t) : ,(filter->sexp fs) : ,(object->sexp lo))]
    [(MPair: s t) `(MPairof ,(t->s s) ,(t->s t))]
    [(Refinement: parent p?)
     `(Refinement ,(t->s parent) ,(syntax-e p?))]
    [(Sequence: ts)
     `(Sequenceof ,@(map t->s ts))]
    [(Error:) 'Error]
    [(fld: t a m) `(fld ,(type->sexp t))]
    [else `(Unknown Type: ,(struct->vector type))]
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

;; debug-pretty-format-type : Type -> String
;; Debugging mode for pretty printing types, which just uses
;; the debug printer above. Ignores kw argument. Only defined
;; in debug printing mode.
(define-syntax (define-debug-pretty-format-type stx)
  (syntax-parse stx
    [(_ debug-pretty-format-type:id)
     (if (eq? printer-type 'debug)
       #'(define (debug-pretty-format-type type #:indent [indent 0])
           (define out (open-output-string))
           (debug-printer type out #t)
           (get-output-string out))
       #'(void))]))

(define-debug-pretty-format-type debug-pretty-format-type)

