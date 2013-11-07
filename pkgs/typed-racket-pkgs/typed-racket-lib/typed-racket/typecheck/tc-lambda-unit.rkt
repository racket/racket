#lang racket/unit

(require "../utils/utils.rkt"
         racket/dict racket/list syntax/parse racket/syntax syntax/stx
         racket/match syntax/id-table racket/set
         (contract-req)
         (except-in (rep type-rep) make-arr)
         (rename-in (except-in (types abbrev utils union) -> ->* one-of/c)
                    [make-arr* make-arr])
         (private type-annotation syntax-properties)
         (types type-table)
         (typecheck signatures tc-metafunctions tc-subst)
         (env lexical-env tvar-env index-env scoped-tvar-env)
         (utils tc-utils)
         (for-template racket/base))

(import tc-expr^)
(export tc-lambda^)

(define-struct/cond-contract lam-result ([args (listof (list/c identifier? Type/c))]
                                         [kws (listof (list/c keyword? identifier? Type/c boolean?))]
                                         [rest (or/c #f (list/c identifier? Type/c))]
                                         [drest (or/c #f (list/c identifier? (cons/c Type/c symbol?)))]
                                         [body tc-results/c])
  #:transparent)

(define (lam-result->type lr)
  (match lr
    [(struct lam-result ((list (list arg-ids arg-tys) ...) (list (list kw kw-id kw-ty req?) ...) rest drest body))
     (let ([arg-names (append arg-ids
                              (if rest (list (first rest)) null)
                              (if drest (list (first drest)) null)
                              kw-id)])
       (make-arr
        arg-tys
        (abstract-results body arg-names)
        #:kws (map make-Keyword kw kw-ty req?)
        #:rest (and rest (second rest))
        #:drest (and drest (second drest))))]))

(define-syntax-class cl-rhs
  #:literals (if)
  #:attributes (i cond)
  [pattern i:id #:attr cond #f]
  [pattern (if cond:id i:id e:expr)])

(define-syntax-class rebuild-let*
  #:literals (let-values)
  #:attributes (mapping flag-mapping)
  (pattern (let-values ([(new-id) e:cl-rhs]) body:rebuild-let*)
           #:attr mapping (dict-set (attribute body.mapping) #'e.i #'new-id)
           #:attr flag-mapping (if (attribute e.cond)
                                   (dict-set (attribute body.flag-mapping) #'e.i #'e.cond)
                                   (attribute body.flag-mapping)))
  (pattern body:expr
           #:attr mapping (make-immutable-free-id-table)
           #:attr flag-mapping (make-immutable-free-id-table)))

(define (expected-str tys-len rest-ty drest arg-len rest)
  (format "Expected function with ~a argument~a~a, but got function with ~a argument~a~a"
          tys-len
          (if (= tys-len 1) "" "s")
          (if (or rest-ty
                  drest)
              " and a rest arg"
              "")
          arg-len
          (if (= arg-len 1) "" "s")
          (if rest " and a rest arg" "")))

;; listof[id] option[id] block listof[type] option[type] option[(cons type var)] tc-result -> lam-result
(define/cond-contract (check-clause arg-list rest body arg-tys rest-ty drest ret-ty)
     ((listof identifier?)
      (or/c #f identifier?) syntax? (listof Type/c) (or/c #f Type/c)
      (or/c #f (cons/c Type/c symbol?)) tc-results/c
      . -> .
      lam-result?)
  (let* ([arg-len (length arg-list)]
         [tys-len (length arg-tys)]
         [arg-types (if (andmap type-annotation arg-list)
                        (get-types arg-list #:default Univ)
                        (cond
                          [(= arg-len tys-len) arg-tys]
                          [(< arg-len tys-len) (take arg-tys arg-len)]
                          [(> arg-len tys-len) (append arg-tys
                                                       (map (lambda _ (or rest-ty (Un)))
                                                            (drop arg-list tys-len)))]))])
    (define (check-body [rest-ty rest-ty])
      (with-lexical-env/extend
       arg-list arg-types
       (make-lam-result (for/list ([al (in-list arg-list)]
                                   [at (in-list arg-types)])
                                  (list al at))
                        null
                        (and rest-ty (list (or rest (generate-temporary)) rest-ty))
                        (and drest (list (or rest (generate-temporary)) drest))
                        (tc-body/check body ret-ty))))
    ;; Check that the number of formal arguments is valid for the expected type.
    ;; Thus it must be able to accept the number of arguments that the expected
    ;; type has. So we check for two cases: if the function doesn't accept
    ;; enough arguments, or if it requires too many arguments.
    ;; This allows a form like (lambda args body) to have the type (-> Symbol
    ;; Number) with out a rest arg.
    (when (or (and (< arg-len tys-len) (not rest))
              (and (> arg-len tys-len) (not (or rest-ty drest))))
      (tc-error/delayed (expected-str tys-len rest-ty drest arg-len rest)))
    (cond
      [(not rest)
       (check-body)]
      [drest
       (with-lexical-env/extend
        (list rest) (list (make-ListDots (car drest) (cdr drest)))
        (check-body))]
      [(dotted? rest)
       =>
       (lambda (b)
         (let ([dty (extend-tvars (list b) (get-type rest #:default Univ))])
           (with-lexical-env/extend
            (list rest) (list (make-ListDots dty b))
            (check-body))))]
      [else
       (define base-rest-type
         (cond
          [rest-ty rest-ty]
          [(type-annotation rest) (get-type rest #:default Univ)]
          [else Univ]))
       (define extra-types
         (if (<= arg-len tys-len)
             (drop arg-tys arg-len)
             null))
       (define rest-type (apply Un base-rest-type extra-types))

       (with-lexical-env/extend
        (list rest) (list (-lst rest-type))
        (check-body rest-type))])))

;; typecheck a single lambda, with argument list and body
;; drest-ty and drest-bound are both false or not false
;; tc/lambda-clause/check: formals syntax listof[Type/c] tc-result
;;                         option[Type/c] option[(cons Type/c symbol)] -> lam-result
(define (tc/lambda-clause/check formals body arg-tys ret-ty rest-ty drest)
  (check-clause (formals-positional formals) (formals-rest formals) body arg-tys rest-ty drest ret-ty))

;; typecheck a single opt-lambda clause with argument list and body
;; tc/opt-lambda-clause: listof[identifier] syntax -> listof[lam-result]
(define (tc/opt-lambda-clause arg-list body aux-table flag-table)
  ;; arg-types: Listof[Type/c]
  (define arg-types
    (for/list ([a (in-list arg-list)])
      (get-type a #:default (lambda ()
                              (define id (dict-ref aux-table a #f))
                              (if id
                                  (get-type id #:default Univ)
                                  Univ)))))

  ;; new-arg-types: Listof[Listof[Type/c]]
  (define new-arg-types
    (if (= 0 (dict-count flag-table))
        (list arg-types)
        (apply append
               (for/list ([(k v) (in-dict flag-table)])
                 (list
                  (for/list ([i (in-list arg-list)]
                             [t (in-list arg-types)])
                    (cond [(free-identifier=? i k) t]
                          [(free-identifier=? i v) (-val #t)]
                          [else t]))
                  (for/list ([i (in-list arg-list)]
                             [t (in-list arg-types)])
                    (cond [(free-identifier=? i k) (-val #f)]
                          [(free-identifier=? i v) (-val #f)]
                          [else t])))))))
  (for/list ([arg-types (in-list new-arg-types)])
    (with-lexical-env/extend
     arg-list arg-types
     (lam-result
       (map list arg-list arg-types)
       null
       #f
       #f
       (tc-body body)))))



;; formals syntax -> listof[lam-result]
(define (tc/lambda-clause formals body)
  (define-values (aux-table flag-table)
    (syntax-parse body
      [(b:rebuild-let*) (values (attribute b.mapping) (attribute b.flag-mapping))]
      [_ (values #hash() #hash())]))

  (define arg-list (formals-positional formals))
  (define rest-id (formals-rest formals))
  (cond
    [(and (> (dict-count aux-table) 0) (not rest-id))
     (tc/opt-lambda-clause arg-list body aux-table flag-table)]
    [else
     (define arg-types (get-types arg-list #:default Univ))
     (define combined-args (map list arg-list arg-types))
     (list
       (cond
         ;; Lambda with poly dotted rest argument
         [(and rest-id (dotted? rest-id))
          =>
          (lambda (bound)
            (unless (bound-index? bound)
              (if (bound-tvar? bound)
                  (tc-error "Bound on ... type (~a) is not an appropriate type variable" bound)
                  (tc-error/stx rest-id "Bound on ... type (~a) was not in scope" bound)))
            (let ([rest-type (extend-tvars (list bound)
                               (get-type rest-id #:default Univ))])
              (with-lexical-env/extend
               (cons rest-id arg-list)
               (cons (make-ListDots rest-type bound) arg-types)
               (lam-result
                 combined-args
                 null
                 #f
                 (list rest-id (cons rest-type bound))
                 (tc-body body)))))]
         ;; Lambda with regular rest argument
         [rest-id
          (let ([rest-type (get-type rest-id #:default Univ)])
            (with-lexical-env/extend
             (cons rest-id arg-list)
             (cons (make-Listof rest-type) arg-types)
             (lam-result
               combined-args
               null
               (list rest-id rest-type)
               #f
               (tc-body body))))]
         ;; Lambda with no rest argument
         [else
          (with-lexical-env/extend
           arg-list arg-types
           (lam-result
             combined-args
             null
             #f
             #f
             (tc-body body)))]))]))

;; positional: natural? - the number of positional arguments
;; rest: boolean? - if there is a positional argument
;; syntax: syntax? - the improper syntax list of identifiers
(struct formals (positional rest syntax) #:transparent)

(define (make-formals stx)
  (let loop ([s stx] [acc null])
    (cond
      [(pair? s) (loop (cdr s) (cons (car s) acc))]
      [(null? s) (formals (reverse acc) #f stx)]
      [(pair? (syntax-e s)) (loop (stx-cdr s) (cons (stx-car s) acc))]
      [(null? (syntax-e s)) (formals (reverse acc) #f stx)]
      [else (formals (reverse acc) s stx)])))

(define (formals->list formals)
  (append
    (formals-positional formals)
    (if (formals-rest formals)
        (list (formals-rest formals))
        empty)))


;; An arity is a list (List Natural Boolean), with the number of positional
;; arguments and whether there is a rest argument.
;;
;; An arities-seen is a list (List (Listof Natural) (U Natural Infinity)),
;; with the list of positional only arities seen and the least number of
;; positional arguments on an arity with a rest argument seen.
(define (formals->arity formals)
  (list
    (length (formals-positional formals))
    (and (formals-rest formals) #t)))

(define initial-arities-seen (list empty +inf.0))

;; arities-seen-add : arities-seen? arity? -> arities-seen?
;; Adds the arity to the arities encoded in the arity-seen.
(define (arities-seen-add arities-seen arity)
  (match-define (list positionals min-rest) arities-seen)
  (match-define (list new-positional new-rest) arity)
  (define new-min-rest
    (if new-rest
        (min new-positional min-rest)
        min-rest))
  (list
    (filter (λ (n) (< n new-min-rest)) (cons new-positional positionals))
    new-min-rest))


;; arities-seen-seen-before? : arities-seen? arity? -> boolean?
;; Determines if the arity would have been covered by an existing arity in the arity-seen
(define (arities-seen-seen-before? arities-seen arity)
  (match-define (list positionals min-rest) arities-seen)
  (match-define (list new-positional new-rest) arity)
  (or (>= new-positional min-rest)
      (and (member new-positional positionals) (not new-rest))))


;; tc/mono-lambda : (listof (list formals syntax?)) (or/c #f tc-results) -> (listof lam-result)
;; typecheck a sequence of case-lambda clauses
(define (tc/mono-lambda formals+bodies expected)
  (define expected-type
    (match expected
      [(tc-result1: t)
       (let loop ((t t))
         (match t
           [(Mu: _ _) (loop (unfold t))]
           [(Function: (list (arr: _ _ _ _ '()) ...)) t]
           [_ #f]))]
      [_ #f]))

  ;; find-matching-arities: formals -> Listof[arr?]
  (define (find-matching-arities fml)
    (match expected-type
      [(Function: (and fs (list (arr: argss rets rests drests '()) ...)))
       (for/list ([a (in-list argss)] [f (in-list fs)]  [r (in-list rests)] [dr (in-list drests)]
                  #:when (if (formals-rest fml)
                             (>= (length a) (length (formals-positional fml)))
                             ((if (or r dr) <= =) (length a) (length (formals-positional fml)))))
         f)]
      [_ null]))

  (define-values (used-formals+bodies arities-seen)
    (for/fold ((formals+bodies* empty) (arities-seen initial-arities-seen))
              ((formal+body formals+bodies))
      (match formal+body
        [(list formal body)
         (define arity (formals->arity formal))
         (values
           (cond
             [(or (arities-seen-seen-before? arities-seen arity)
                   (and expected-type (null? (find-matching-arities formal))))
              (warn-unreachable body)
              (add-dead-lambda-branch (formals-syntax formal))
              (if (check-unreachable-code?)
                  (cons formal+body formals+bodies*)
                  formals+bodies*)]
             [else
              (cons formal+body formals+bodies*)])
           (arities-seen-add arities-seen arity))])))

   (if (and
         (empty? used-formals+bodies)
         ;; If the empty function is expected, then don't error out
         (match expected-type
           [(Function: (list)) #f]
           [_ #t]))
       ;; TODO improve error message.
       (tc-error/expr #:return (list (lam-result null null (list (generate-temporary) Univ) #f (ret (Un))))
                      "Expected a function of type ~a, but got a function with the wrong arity"
                      expected-type)
       (apply append
              (for/list ([fb* (in-list used-formals+bodies)])
                (match-define (list f* b*) fb*)
                (match (find-matching-arities f*)
                  [(list) (tc/lambda-clause f* b*)]
                  [(list (arr: argss rets rests drests '()) ...)
                   (for/list ([args (in-list argss)] [ret (in-list rets)] [rest (in-list rests)] [drest (in-list drests)])
                     (tc/lambda-clause/check
                      f* b* args (values->tc-results ret (formals->list f*)) rest drest))])))))

(define (tc/mono-lambda/type formals bodies expected)
  (make-Function (map lam-result->type
                      (tc/mono-lambda
                        (map list
                             (stx-map make-formals formals)
                             (syntax->list bodies))
                        expected))))

(define (plambda-prop stx)
  (define d (plambda-property stx))
  (and d (car (flatten d))))

(define (has-poly-annotation? form)
  (or (plambda-prop form) (cons? (lookup-scoped-tvar-layer form))))

(define (remove-poly-layer tvarss)
  (filter cons? (map rest tvarss)))

(define (get-poly-layer tvarss)
  (map first tvarss))

(define (get-poly-tvarss form)
  (let ([plambda-tvars
          (let ([p (plambda-prop form)])
            (match (and p (map syntax-e (syntax->list p)))
              [#f #f]
              [(list var ... dvar '...)
               (list (list var dvar))]
              [(list id ...)
               (list id)]))]
        [scoped-tvarss
          (for/list ((tvarss (in-list (lookup-scoped-tvar-layer form))))
            (for/list ((tvar (in-list tvarss)))
              (match tvar
                [(list (list v ...) dotted-v)
                 (list (map syntax-e v) (syntax-e dotted-v))]
                [(list v ...) (map syntax-e v)])))])
    (if plambda-tvars
        (cons plambda-tvars scoped-tvarss)
        scoped-tvarss)))


;; tc/plambda syntax tvarss-list syntax-list syntax-list type -> Poly
;; formals and bodies must by syntax-lists
(define/cond-contract (tc/plambda form tvarss-list formals bodies expected)
  (syntax? (listof list?) syntax? syntax? (or/c tc-results/c #f) . -> . Type/c)
  (define/cond-contract (maybe-loop form formals bodies expected)
    (syntax? syntax? syntax? (or/c tc-results/c #f) . -> . Type/c)
    (match expected
      [(tc-result1: (or (Poly: _ _) (PolyDots: _ _)))
       (tc/plambda form (remove-poly-layer tvarss-list) formals bodies expected)]
      [(tc-result1: (and v (Values: _))) (maybe-loop form formals bodies (values->tc-results v #f))]
      [_ 
        (define remaining-layers (remove-poly-layer tvarss-list))
        (if (null? remaining-layers)
            (tc/mono-lambda/type formals bodies expected)
            (tc/plambda form remaining-layers formals bodies expected))]))
  ;; check the bodies appropriately
  ;; and make both annotated and declared type variables point to the
  ;; same actual type variables (the fresh names)
  (define (extend-and-loop form ns formals bodies expected)
    (let loop ((tvarss tvarss))
      (match tvarss
        [(list) (maybe-loop form formals bodies expected)]
        [(cons (list (list tvars ...) dotted) rest-tvarss)
         (extend-indexes dotted
            (extend-tvars/new tvars ns
              (loop rest-tvarss)))]
        [(cons tvars rest-tvarss)
         (extend-tvars/new tvars ns
           (loop rest-tvarss))])))
  (define tvarss (get-poly-layer tvarss-list))

  (match expected
    [(tc-result1: (and t (Poly-fresh: ns fresh-ns expected*)))
     ;; make sure the declared and annotated type variable arities match up
     ;; with the expected type variable arity
     (for ((tvars (in-list tvarss)))
       (when (and (cons? tvars) (list? (first tvars)))
         (tc-error
          "Expected a polymorphic function without ..., but given function/annotation had ..."))
       (unless (= (length tvars) (length fresh-ns))
         (tc-error "Expected ~a type variables, but given ~a"
                   (length fresh-ns) (length tvars))))
     (make-Poly #:original-names ns fresh-ns (extend-and-loop form fresh-ns formals bodies (ret expected*)))]
    [(tc-result1: (and t (PolyDots-names: (list ns ... dvar) expected*)))
     ;; make sure the declared and annotated type variable arities match up
     ;; with the expected type variable arity
     (for ((tvars (in-list tvarss)))
       (match tvars
         [(list (list vars ...) dotted)
          (unless (= (length vars) (length ns))
            (tc-error "Expected ~a non-dotted type variables, but given ~a"
                      (length ns) (length vars)))]
         [else
           (tc-error "Expected a polymorphic function with ..., but function/annotation had no ...")]))
     (make-PolyDots (append ns (list dvar)) (extend-and-loop form ns formals bodies (ret expected*)))]
    [(or (tc-results: _) (tc-any-results:) #f)
     (define lengths
       (for/set ((tvars (in-list tvarss)))
         (match tvars
           [(list (list vars ...) dotted)
            (length vars)]
           [(list vars ...)
            (length vars)])))
     (define dots
       (for/set ((tvars (in-list tvarss)))
         (match tvars
           [(list (list vars ...) dotted) #t]
           [(list vars ...) #f])))
     (unless (= 1 (set-count lengths))
      (tc-error "Expected annotations to have the same number of type variables, but given ~a"
                (set->list lengths)))
     (unless (= 1 (set-count dots))
      (tc-error "Expected annotations to all have ... or none to have ..., but given both"))
     (define dotted (and (set-first dots) (second (first tvarss))))
     (define ns (build-list (set-first lengths) (lambda (_) (gensym))))
     (define results (extend-and-loop form ns formals bodies expected))
     (if dotted
         (make-PolyDots (append ns (list dotted)) results)
         (make-Poly #:original-names (first tvarss) ns results))]))

;; typecheck a sequence of case-lambda clauses, which is possibly polymorphic
;; tc/lambda/internal syntax syntax-list syntax-list option[type] -> tc-result
(define (tc/lambda/internal form formals bodies expected)
  (if (or (has-poly-annotation? form)
          (match expected
            [(tc-result1: t) (or (Poly? t) (PolyDots? t))]
            [_ #f]))
      (ret (tc/plambda form (get-poly-tvarss form) formals bodies expected) -true-filter)
      (ret (tc/mono-lambda/type formals bodies expected) -true-filter)))

;; tc/lambda : syntax syntax-list syntax-list -> tc-result
(define (tc/lambda form formals bodies)
  (tc/lambda/internal form formals bodies #f))

;; tc/lambda/check : syntax syntax-list syntax-list Type -> tc-result
(define (tc/lambda/check form formals bodies expected)
  (tc/lambda/internal form formals bodies expected))

;; formals : the formal arguments to the loop
;; body : a block containing the body of the loop
;; name : the name of the loop
;; args : the types of the actual arguments to the loop
;; ret : the expected return type of the whole expression
(define (tc/rec-lambda/check formals body name args return)
  (with-lexical-env/extend
   (syntax->list formals) args
   (let* ([r (tc-results->values return)]
          [t (make-arr args r)]
          [ft (make-Function (list t))])
     (with-lexical-env/extend
      (list name) (list ft)
      (begin (tc-body/check body return) (ret ft))))))
