#lang racket/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt"
         "tc-metafunctions.rkt"
         "tc-subst.rkt" "check-below.rkt"
         racket/dict
         racket/list syntax/parse "parse-cl.rkt"
         racket/syntax unstable/struct syntax/stx
         (rename-in racket/contract [-> -->] [->* -->*] [one-of/c -one-of/c])
         (except-in (rep type-rep) make-arr)
         (rename-in (types abbrev utils union)
                    [make-arr* make-arr])
         (private type-annotation)
         (env type-env-structs lexical-env tvar-env index-env)
         (utils tc-utils)

         racket/match)
(require (for-template racket/base "internal-forms.rkt"))

(import tc-expr^)
(export tc-lambda^)

(define-struct/cond-contract lam-result ([args (listof (list/c identifier? Type/c))]
                                         [kws (listof (list/c keyword? identifier? Type/c boolean?))]
                                         [rest (or/c #f (list/c identifier? Type/c))]
                                         [drest (or/c #f (cons/c identifier? (cons/c Type/c symbol?)))]
                                         [body tc-results?])
  #:transparent)

(define (lam-result->type lr)
  (match lr
    [(struct lam-result ((list (list arg-ids arg-tys) ...) (list (list kw kw-id kw-ty req?) ...) rest drest body))
     (let ([arg-names (append arg-ids
                              (if rest (list (car rest)) null)
                              (if drest (list (car drest)) null)
                              kw-id)])
       (make-arr
        arg-tys
        (abstract-results body arg-names)
        #:kws (map make-Keyword kw kw-ty req?)
        #:rest (if rest (second rest) #f)
        #:drest (if drest (cdr drest) #f)))]
    [_ (int-err "not a lam-result")]))

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
      (or/c #f identifier?) syntax? (listof Type/c) (or/c #f Type/c) (or/c #f (cons/c Type/c symbol?)) tc-results?
      . --> .
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
    (define (check-body)
      (with-lexical-env/extend
       arg-list arg-types
       (make-lam-result (for/list ([al arg-list] [at arg-types] [a-ty arg-tys]) (list al at)) null
                        (and rest-ty (list (or rest (generate-temporary)) rest-ty))
                        ;; make up a fake name if none exists, this is an error case anyway
                        (and drest (cons (or rest (generate-temporary)) drest))
                        (tc-exprs/check (syntax->list body) ret-ty))))
    (when (or (not (= arg-len tys-len))
              (and (or rest-ty drest) (not rest)))
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
         (let ([dty (get-type rest #:default Univ)])
           (with-lexical-env/extend
            (list rest) (list (make-ListDots dty b))
            (check-body))))]
      [else
       (let ([rest-type (cond
                          [rest-ty rest-ty]
                          [(type-annotation rest) (get-type rest #:default Univ)]
                          [(< arg-len tys-len) (list-ref arg-tys arg-len)]
                          [else (Un)])])
         (with-lexical-env/extend
          (list rest) (list (-lst rest-type))
          (check-body)))])))

;; typecheck a single lambda, with argument list and body
;; drest-ty and drest-bound are both false or not false
;; syntax-list[id] block listof[type] tc-result option[type] option[(cons type var)] -> lam-result
(define (tc/lambda-clause/check args body arg-tys ret-ty rest-ty drest)
    (syntax-case args ()
      [(args* ...)
       (check-clause (syntax->list #'(args* ...)) #f body arg-tys rest-ty drest ret-ty)]
      [(args* ... . rest)
       (check-clause (syntax->list #'(args* ...)) #'rest body arg-tys rest-ty drest ret-ty)]))

;; syntax-list[id] block -> lam-result
(define (tc/lambda-clause args body)
  (define-values (aux-table flag-table)
    (syntax-parse body
      [(b:rebuild-let*) (values (attribute b.mapping) (attribute b.flag-mapping))]
      [_ (values #hash() #hash())]))
  ;(printf "body: ~a\n" body)
  (syntax-case args ()
    [(args ...)
     (let* ([arg-list (syntax->list #'(args ...))]
            [arg-types (for/list ([a arg-list]) 
                         (get-type a #:default (lambda () 
                                                 #;(printf "got to here ~a ~a ~a\n~a ~a\n" 
                                                         (syntax-e a) (syntax-e (dict-ref aux-table a #'no)) (dict-ref aux-table a #'no)
                                                         aux-table (dict-keys aux-table))
                                                 (get-type (dict-ref aux-table a #'no) #:default Univ))))])
       (define new-arg-types
         (if (= 0 (dict-count flag-table))
             (list arg-types)
             (apply append
                    (for/list ([(k v) (in-dict flag-table)])
                      (list
                       (for/list ([i arg-list]
                                  [t arg-types])
                         (cond [(free-identifier=? i k) t]
                               [(free-identifier=? i v) (-val #t)]
                               [else t]))
                       (for/list ([i arg-list]
                                  [t arg-types])
                         (cond [(free-identifier=? i k) (-val #f)]
                               [(free-identifier=? i v) (-val #f)]
                               [else t])))))))
       #;(printf "nat: ~a\n" new-arg-types)
       (for/list ([arg-types (in-list new-arg-types)])
         (with-lexical-env/extend
          arg-list arg-types
          (make lam-result
                (map list arg-list arg-types)
                null
                #f
                #f
                (tc-exprs (syntax->list body))))))]
    [(args ... . rest)
     (let* ([arg-list (syntax->list #'(args ...))]
            [arg-types (get-types arg-list #:default Univ)])
       (cond
         [(dotted? #'rest)
          =>
          (lambda (bound)
            (unless (bound-index? bound)
              (if (bound-tvar? bound)
                  (tc-error "Bound on ... type (~a) is not an appropriate type variable" bound)
                  (tc-error/stx #'rest "Bound on ... type (~a) was not in scope" bound)))
            (let ([rest-type (extend-tvars (list bound)
                               (get-type #'rest #:default Univ))])
              (with-lexical-env/extend
               (cons #'rest arg-list)
               (cons (make-ListDots rest-type bound) arg-types)
               (list (make lam-result
                           (map list arg-list arg-types)
                           null
                           #f
                           (cons #'rest (cons rest-type bound))
                           (tc-exprs (syntax->list body)))))))]
         [else
          (let ([rest-type (get-type #'rest #:default Univ)])
            (with-lexical-env/extend
             (cons #'rest arg-list)
             (cons (make-Listof rest-type) arg-types)
             (list
              (make lam-result
                    (map list arg-list arg-types)
                    null
                    (list #'rest rest-type)
                    #f
                    (tc-exprs (syntax->list body))))))]))]))

(define (formals->list l)
  (let loop ([l (syntax-e l)])
    (cond [(stx-pair? l) (cons (stx-car l) (loop (stx-cdr l)))]
          [(pair? l) (cons (car l) (loop (cdr l)))]
          [else null])))

;; tc/mono-lambda : syntax-list syntax-list (or/c #f tc-results) -> (listof lam-result)
;; typecheck a sequence of case-lambda clauses
(define (tc/mono-lambda formals bodies expected)
  (define (syntax-len s)
    (cond [(syntax->list s) => length]
          [else (let loop ([s s])
                  (cond
                    [(pair? s)
                     (+ 1 (loop (cdr s)))]
                    [(pair? (syntax-e s))
                     (+ 1 (loop (cdr (syntax-e s))))]
                    [else 1]))]))
  (define (formals->list s)
    (let loop ([s s])
      (cond
        [(pair? s)
         (cons (car s) (loop (cdr s)))]
        [(null? s) s]
        [(pair? (syntax-e s))
         (cons (stx-car s) (loop (cdr (syntax-e s))))]
        [(null? (syntax-e s)) null]
        [else (list s)])))
  (define (find-expected tc-r fml)
    (match tc-r
      [(tc-result1: (Function: (and fs (list (arr: argss rets rests drests '()) ...))))       
       (cond [(syntax->list fml)
              (for/list ([a argss] [f fs]  [r rests] [dr drests] 
                                   #:when (and (not r) (not dr) (= (length a) (length (syntax->list fml)))))
                f)]
             [else
              (for/list ([a argss] [f fs]  [r rests] [dr drests]
                                   #:when (and (or r dr) (= (length a) (sub1 (syntax-len fml)))))
                f)])]
       [_ null]))
  (define (go expected formals bodies formals* bodies* nums-seen)
    (cond
      [(null? formals)
       (apply append
              (for/list ([f* formals*] [b* bodies*])                
                (match (find-expected expected f*)
                  ;; very conservative -- only do anything interesting if we get exactly one thing that matches
                  [(list) 
                   (if (and (= 1 (length formals*)) expected)
                       (tc-error/expr #:return (list (lam-result null null (list #'here Univ) #f (ret (Un))))
                                      "Expected a function of type ~a, but got a function with the wrong arity"
                                      (match expected [(tc-result1: t) t]))
                       (tc/lambda-clause f* b*))]
                  [(list (arr: argss rets rests drests '()) ...) 
                   (for/list ([args argss] [ret rets] [rest rests] [drest drests])
                     (tc/lambda-clause/check 
                      f* b* args (values->tc-results ret (formals->list f*)) rest drest))])))]
      [(memv (syntax-len (car formals)) nums-seen)
       ;; we check this clause, but it doesn't contribute to the overall type
       (tc/lambda-clause (car formals) (car bodies))
       ;; FIXME - warn about dead clause here       
       (go expected (cdr formals) (cdr bodies) formals* bodies* nums-seen)]
      [else
       (go expected
           (cdr formals) (cdr bodies)
           (cons (car formals) formals*)
           (cons (car bodies) bodies*)
           (cons (syntax-len (car formals)) nums-seen))]))  
  (let loop ([expected expected])
    (match expected
      [(tc-result1: (and t (Mu: _ _))) (loop (ret (unfold t)))]
      [(tc-result1: (Function: (list (arr: argss rets rests drests '()) ...)))
       (go expected (syntax->list formals) (syntax->list bodies) null null null)]
      [_ (go #f (syntax->list formals) (syntax->list bodies) null null null)])))

(define (tc/mono-lambda/type formals bodies expected)
  (define t (make-Function (map lam-result->type (tc/mono-lambda formals bodies expected))))
  (cond-check-below (ret t true-filter) expected)
  t)

(define (plambda-prop stx)
  (define d (syntax-property stx 'typechecker:plambda))
  (and d (car (flatten d))))

;; tc/plambda syntax syntax-list syntax-list type -> Poly
;; formals and bodies must by syntax-lists
(define/cond-contract (tc/plambda form formals bodies expected)
  (syntax? syntax? syntax? (or/c tc-results? #f) . --> . Type/c)
  (define/cond-contract (maybe-loop form formals bodies expected)
    (syntax? syntax? syntax? tc-results? . --> . Type/c)
    (match expected
      [(tc-result1: (Function: _)) (tc/mono-lambda/type formals bodies expected)]
      [(tc-result1: (or (Poly: _ _) (PolyDots: _ _)))
       (tc/plambda form formals bodies expected)]
      [(tc-result1: (Error:)) (tc/mono-lambda/type formals bodies #f)]
      [(tc-result1: (and v (Values: _))) (maybe-loop form formals bodies (values->tc-results v #f))]
      [_ (int-err "expected not an appropriate tc-result: ~a" expected)]))
  (match expected
    [(tc-result1: (and t (Poly-fresh: ns fresh-ns expected*)))
     (let* ([tvars (let ([p (plambda-prop form)])
                     (when (and (pair? p) (eq? '... (car (last p))))
                       (tc-error
                        "Expected a polymorphic function without ..., but given function had ..."))
                     (and p (map syntax-e (syntax->list p))))])
       ;; make sure the declared type variable arity matches up with the
       ;; annotated type variable arity
       (when tvars
        (unless (= (length tvars) (length ns))
          (tc-error "Expected ~a type variables, but given ~a"
                    (length ns) (length tvars))))
       ;; check the bodies appropriately
       (if tvars
           ;; make both annotated and given type variables point to the
           ;; same actual type variables (the fresh names)
           (extend-tvars/new ns fresh-ns
            (extend-tvars/new tvars fresh-ns
             (maybe-loop form formals bodies (ret expected*))))
           ;; no plambda: type variables given
           (extend-tvars/new ns fresh-ns
            (maybe-loop form formals bodies (ret expected*))))
       t)]
    [(tc-result1: (and t (PolyDots-names: (list ns ... dvar) expected*)))
     (let-values
         ([(tvars dotted)
           (let ([p (plambda-prop form)])
             (if p
                 (match (map syntax-e (syntax->list p))
                   [(list var ... dvar '...)
                    (values var dvar)]
                   [_ (tc-error "Expected a polymorphic function with ..., but given function had no ...")])
                 (values ns dvar)))])
       ;; check the body for side effect
       (extend-indexes dotted
         (extend-tvars tvars
           (maybe-loop form formals bodies (ret expected*))))
       t)]
    [#f
     (match (map syntax-e (syntax->list (plambda-prop form)))
       [(list tvars ... dotted-var '...)
        (let* ([ty (extend-indexes dotted-var
                     (extend-tvars tvars
                       (tc/mono-lambda/type formals bodies #f)))])
          (make-PolyDots (append tvars (list dotted-var)) ty))]
       [tvars
        (let* (;; manually make some fresh names since
               ;; we don't use a match expander
               [fresh-tvars (map gensym tvars)]
               [ty (extend-tvars/new tvars fresh-tvars
                     (tc/mono-lambda/type formals bodies #f))])
          ;(printf "plambda: ~a ~a ~a \n" literal-tvars new-tvars ty)
          (make-Poly fresh-tvars ty #:original-names tvars))])]
    [(tc-result1: t)
     (check-below (tc/plambda form formals bodies #f) t)]
    [_ (int-err "not a good expected value: ~a" expected)]))

;; typecheck a sequence of case-lambda clauses, which is possibly polymorphic
;; tc/lambda/internal syntax syntax-list syntax-list option[type] -> tc-result
(define (tc/lambda/internal form formals bodies expected)
  (if (or (plambda-prop form)
          (match expected
            [(tc-result1: t) (or (Poly? t) (PolyDots? t))]
            [_ #f]))
      (ret (tc/plambda form formals bodies expected) true-filter)
      (ret (tc/mono-lambda/type formals bodies expected) true-filter)))

;; tc/lambda : syntax syntax-list syntax-list -> tc-result
(define (tc/lambda form formals bodies)
  (tc/lambda/internal form formals bodies #f))

;; tc/lambda/check : syntax syntax-list syntax-list Type -> tc-result
(define (tc/lambda/check form formals bodies expected)
  (tc/lambda/internal form formals bodies expected))

;; form : a syntax object for error reporting
;; formals : the formal arguments to the loop
;; body : a block containing the body of the loop
;; name : the name of the loop
;; args : the types of the actual arguments to the loop
;; ret : the expected return type of the whole expression
(define (tc/rec-lambda/check form formals body name args return)
  (with-lexical-env/extend
   (syntax->list formals) args
   (let* ([r (tc-results->values return)]
          [t (make-arr args r)]
          [ft (make-Function (list t))])
     (with-lexical-env/extend
      (list name) (list ft)
      (begin (tc-exprs/check (syntax->list body) return) (ret ft))))))

;(trace tc/mono-lambda)


