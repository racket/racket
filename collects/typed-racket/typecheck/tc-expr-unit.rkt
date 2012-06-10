#lang scheme/unit


(require (rename-in "../utils/utils.rkt" [private private-in])
         racket/match (prefix-in - scheme/contract)
         "signatures.rkt" "tc-envops.rkt" "tc-metafunctions.rkt" "tc-subst.rkt"
         "check-below.rkt" "tc-funapp.rkt" "tc-app-helper.rkt" "../types/kw-types.rkt"
         (types utils convenience union subtype remove-intersect type-table filter-ops)
         (private-in parse-type type-annotation)
         (rep type-rep)
         (only-in (infer infer) restrict)
         (except-in (utils tc-utils stxclass-util))
         (env lexical-env type-env-structs tvar-env index-env)
         racket/private/class-internal
         (except-in syntax/parse id)
         unstable/function #;unstable/debug
         (only-in srfi/1 split-at)
         (for-template "internal-forms.rkt"))

(require (for-template scheme/base racket/private/class-internal))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ check-subforms^)
(export tc-expr^)

;; return the type of a literal value
;; scheme-value [type] -> type
(define (tc-literal v-stx [expected #f])
  (define-syntax-class exp
    (pattern (~and i (~or :number :str :bytes))
             #:fail-unless expected #f
             #:fail-unless (subtype (-val (syntax-e #'i)) expected) #f))
  (define r
    (syntax-parse v-stx
      [i:exp expected]
      [i:boolean (-val (syntax-e #'i))]
      [i:identifier (-val (syntax-e #'i))]
      
      ;; Numbers
      [0 -Zero]
      [1 -One]
      [(~var i (3d (conjoin byte? positive?))) -PosByte]
      [(~var i (3d byte?)) -Byte]
      [(~var i (3d (conjoin portable-index? positive?))) -PosIndex]
      [(~var i (3d (conjoin portable-fixnum? positive?))) -PosFixnum]
      [(~var i (3d (conjoin portable-fixnum? negative?))) -NegFixnum]
      [(~var i (3d exact-positive-integer?)) -PosInt]
      [(~var i (3d (conjoin exact-integer? negative?))) -NegInt]
      [(~var i (3d (conjoin number? exact? rational? positive?))) -PosRat]
      [(~var i (3d (conjoin number? exact? rational? negative?))) -NegRat]
      [(~var i (3d (lambda (x) (eqv? x 0.0)))) -FlonumPosZero]
      [(~var i (3d (lambda (x) (eqv? x -0.0)))) -FlonumNegZero]
      [(~var i (3d (lambda (x) (eqv? x +nan.0)))) -FlonumNan]
      [(~var i (3d (conjoin flonum? positive?))) -PosFlonum]
      [(~var i (3d (conjoin flonum? negative?))) -NegFlonum]
      [(~var i (3d flonum?)) -Flonum] ; for nan
      [(~var i (3d (lambda (x) (eqv? x 0.0f0)))) -SingleFlonumPosZero]
      [(~var i (3d (lambda (x) (eqv? x -0.0f0)))) -SingleFlonumNegZero]
      [(~var i (3d (lambda (x) (eqv? x +nan.f)))) -SingleFlonumNan]
      [(~var i (3d (conjoin single-flonum? positive?))) -PosSingleFlonum]
      [(~var i (3d (conjoin single-flonum? negative?))) -NegSingleFlonum]
      [(~var i (3d single-flonum?)) -SingleFlonum] ; for nan
      [(~var i (3d inexact-real?)) -InexactReal] ; catch-all, just in case
      [(~var i (3d real?)) -Real] ; catch-all, just in case
      ;; a complex number can't have a float imaginary part and an exact real part
      [(~var i (3d (conjoin number? exact?)))
       -ExactNumber]
      [(~var i (3d (conjoin number? (lambda (x) (and (flonum? (imag-part x))
                                                     (flonum? (real-part x)))))))
       -FloatComplex]
      [(~var i (3d (conjoin number? (lambda (x) (and (single-flonum? (imag-part x))
                                                     (single-flonum? (real-part x)))))))
       -SingleFlonumComplex]
      ;; can't have real and imaginary parts that are both inexact, but not the same precision
      [(~var i (3d number?)) -Number] ; otherwise, Number
      
      [i:str -String]
      [i:char -Char]
      [i:keyword (-val (syntax-e #'i))]
      [i:bytes -Bytes]
      [i:byte-pregexp -Byte-PRegexp]
      [i:byte-regexp -Byte-Regexp]
      [i:pregexp -PRegexp]
      [i:regexp  -Regexp]
      [(~and i ()) (-val '())]
      [(i . r)
       (match (and expected (restrict expected (-pair Univ Univ) 'orig))
         [(Pair: a-ty d-ty)
          (-pair
           (tc-literal #'i a-ty)
           (tc-literal #'r d-ty))]
         [(Union: '())
          (tc-error/expr "expected ~a, but got" expected #:return expected)]
         ;; errors are handled elsewhere
         [t 
          (-pair (tc-literal #'i) (tc-literal #'r))])]
      [(~var i (3d vector?))
       (match (and expected (restrict expected (-vec Univ) 'orig))
         [(Vector: t)
          (make-Vector (apply Un
                              t ;; so that this isn't (Un) when we get no elems
                              (for/list ([l (in-vector (syntax-e #'i))])
                                (tc-literal l t))))]
         [(HeterogenousVector: ts)
          (make-HeterogenousVector
           (for/list ([l (in-vector (syntax-e #'i))]
                      [t (in-list ts)])
             (tc-literal l t)))]
         ;; errors are handled elsewhere
         [_ (make-HeterogenousVector (for/list ([l (syntax-e #'i)])
                                       (generalize (tc-literal l #f))))])]
      [(~var i (3d hash?))
       (match expected
         [(Hashtable: k v)
          (let* ([h (syntax-e #'i)]
                 [ks (hash-map h (lambda (x y) (tc-literal x k)))]
                 [vs (hash-map h (lambda (x y) (tc-literal y v)))])
            (make-Hashtable (generalize (check-below (apply Un ks) k)) (generalize (check-below (apply Un vs) v))))]
         [_ (let* ([h (syntax-e #'i)]
                   [ks (hash-map h (lambda (x y) (tc-literal x)))]
                   [vs (hash-map h (lambda (x y) (tc-literal y)))])
              (make-Hashtable (generalize (apply Un ks)) (generalize (apply Un vs))))])]
      [_ Univ]))
  
  (if expected
      (check-below r expected)
      r))


;; do-inst : syntax type -> type
(define (do-inst stx ty)
  (define inst (syntax-property stx 'type-inst))
  (define (split-last l)
    (let-values ([(all-but last-list) (split-at l (sub1 (length l)))])
      (values all-but (car last-list))))
  (define (in-improper-stx stx)
    (let loop ([l stx])
      (match l
        [#f null]
        [(cons a b) (cons a (loop b))]
        [e (list e)])))
  (match ty
    [(list ty)
     (list
      (for/fold ([ty ty])
        ([inst (in-improper-stx inst)])
        (cond [(not inst) ty]
              [(not (or (Poly? ty) (PolyDots? ty)))
               (tc-error/expr #:return (Un) "Cannot instantiate non-polymorphic type ~a"
                              (cleanup-type ty))]
              [(and (Poly? ty)
                    (not (= (length (syntax->list inst)) (Poly-n ty))))
               (tc-error/expr #:return (Un)
                              "Wrong number of type arguments to polymorphic type ~a:\nexpected: ~a\ngot: ~a"
                              (cleanup-type ty) (Poly-n ty) (length (syntax->list inst)))]
              [(and (PolyDots? ty) (not (>= (length (syntax->list inst)) (sub1 (PolyDots-n ty)))))
               ;; we can provide 0 arguments for the ... var
               (tc-error/expr #:return (Un)
                              "Wrong number of type arguments to polymorphic type ~a:\nexpected at least: ~a\ngot: ~a"
                              (cleanup-type ty) (sub1 (PolyDots-n ty)) (length (syntax->list inst)))]
              [(PolyDots? ty)
               ;; In this case, we need to check the last thing.  If it's a dotted var, then we need to
               ;; use instantiate-poly-dotted, otherwise we do the normal thing.
               ;; In the case that the list is empty we also do the normal thing
               (let ((stx-list (syntax->list inst)))
                 (if (null? stx-list)
                     (instantiate-poly ty (map parse-type stx-list))
                     (let-values ([(all-but-last last-stx) (split-last stx-list)])
                       (match (syntax-e last-stx)
                         [(cons last-ty-stx (? identifier? last-id-stx))
                          (unless (bound-index? (syntax-e last-id-stx))
                            (tc-error/stx last-id-stx "~a is not a type variable bound with ..." (syntax-e last-id-stx)))
                          (if (= (length all-but-last) (sub1 (PolyDots-n ty)))
                              (let* ([last-id (syntax-e last-id-stx)]
                                     [last-ty (extend-tvars (list last-id) (parse-type last-ty-stx))])
                                (instantiate-poly-dotted ty (map parse-type all-but-last) last-ty last-id))
                              (tc-error/expr #:return (Un) "Wrong number of fixed type arguments to polymorphic type ~a:\nexpected: ~a\ngot: ~a"
                                             ty (sub1 (PolyDots-n ty)) (length all-but-last)))]
                         [_
                          (instantiate-poly ty (map parse-type stx-list))]))))]
              [else
               (instantiate-poly ty (map parse-type (syntax->list inst)))])))]
    [_ (if inst
           (tc-error/expr #:return (Un)
                          "Cannot instantiate expression that produces ~a values"
                          (if (null? ty) 0 "multiple"))
           ty)]))

;; typecheck an identifier
;; the identifier has variable effect
;; tc-id : identifier -> tc-results
(define/cond-contract (tc-id id)
  (--> identifier? tc-results?)
  (let* ([ty (lookup-type/lexical id)])
    (ret ty
         (make-FilterSet (-not-filter (-val #f) id)
                         (-filter (-val #f) id))
         (make-Path null id))))

;; typecheck an expression, but throw away the effect
;; tc-expr/t : Expr -> Type
(define (tc-expr/t e) (match (single-value e)
                        [(tc-result1: t _ _) t]
                        [t (int-err "tc-expr returned ~a, not a single tc-result, for ~a" t (syntax->datum e))]))

(define (tc-expr/check/t e t)
  (match (tc-expr/check e t)
    [(tc-result1: t) t]))

(define (tc-expr/check/type form expected)
  #;(syntax? Type/c . -> . tc-results?)
  (tc-expr/check form (ret expected)))

(define (tc-expr/check form expected)
  (parameterize ([current-orig-stx form])
    ;; the argument must be syntax
    (unless (syntax? form)
      (int-err "bad form input to tc-expr: ~a" form))
    ;; typecheck form
    (let loop ([form* form] [expected expected] [checked? #f])
      (cond [(type-ascription form*)
             =>
             (lambda (ann)
               (let* ([r (tc-expr/check/internal form* ann)]
                      [r* (check-below r expected)])
                 ;; add this to the *original* form, since the newer forms aren't really in the program
                 (add-typeof-expr form ann)
                 ;; around again in case there is an instantiation
                 ;; remove the ascription so we don't loop infinitely
                 (loop (remove-ascription form*) r* #t)))]
            [(syntax-property form* 'type-inst)
             ;; check without property first
             ;; to get the appropriate type to instantiate
             (match (tc-expr (syntax-property form* 'type-inst #f))
               [(tc-results: ts fs os)
                ;; do the instantiation on the old type
                (let* ([ts* (do-inst form* ts)]
                       [ts** (ret ts* fs os)])
                  (add-typeof-expr form ts**)
                  ;; make sure the new type is ok
                  (check-below ts** expected))]
               ;; no annotations possible on dotted results
               [ty (add-typeof-expr form ty) ty])]
            ;; nothing to see here
            [checked? expected]
            [else (let ([t (tc-expr/check/internal form* expected)])
                    (add-typeof-expr form t)
                    t)]))))

(define (explicit-fail stx msg var)
  (cond [(and (identifier? var) (lookup-type/lexical var #:fail (λ _ #f)))
         =>
         (λ (t)
           (tc-error/expr #:return (ret (Un)) #:stx stx
                          (string-append (syntax-e msg) "; missing coverage of ~a")
                          t))]
         [else (tc-error/expr #:return (ret (Un)) #:stx stx (syntax-e msg))]))

;; tc-expr/check : syntax tc-results -> tc-results
(define/cond-contract (tc-expr/check/internal form expected)
  (--> syntax? tc-results? tc-results?)
  (parameterize ([current-orig-stx form])
    ;(printf "form: ~a\n" (syntax-object->datum form))
    ;; the argument must be syntax
    (unless (syntax? form)
      (int-err "bad form input to tc-expr: ~a" form))
    (let ([old-ret ret]
          ;; a local version of ret that does the checking
          [ret
           (lambda args
             (define te (apply ret args))
             (check-below te expected))])
      (syntax-parse form
        #:literal-sets (kernel-literals)
        #:literals (find-method/who)
        [stx
         #:when (syntax-property form 'typechecker:with-handlers)
         (check-subforms/with-handlers/check form expected)]
        [stx
         #:when (syntax-property form 'typechecker:ignore-some)
         (let ([ty (check-subforms/ignore form)])
           (unless ty
             (int-err "internal error: ignore-some"))
           (check-below ty expected))]
        ;; explicit failure
        [(quote-syntax ((~literal typecheck-fail-internal) stx msg:str var))
         (explicit-fail #'stx #'msg #'var)]
        ;; data
        [(quote #f) (ret (-val #f) false-filter)]
        [(quote #t) (ret (-val #t) true-filter)]
        [(quote val)  (match expected
                        [(tc-result1: t)
                         (ret (tc-literal #'val t) true-filter)])]
        ;; syntax
        [(quote-syntax datum) (ret (-Syntax (tc-literal #'datum)) true-filter)]
        ;; mutation!
        [(set! id val)
         (match-let* ([(tc-result1: id-t) (single-value #'id)]
                      [(tc-result1: val-t) (single-value #'val)])
           (unless (subtype val-t id-t)
             (tc-error/expr "Mutation only allowed with compatible types:\n~a is not a subtype of ~a" val-t id-t))
           (ret -Void))]
        ;; top-level variable reference - occurs at top level
        [(#%top . id) (check-below (tc-id #'id) expected)]

        [(#%variable-reference . _)
         (ret -Variable-Reference)]
        ;; identifiers
        [x:identifier
           (check-below (tc-id #'x) expected)]
        ;; w-c-m
        [(with-continuation-mark e1 e2 e3)
         (begin (tc-expr/check/type #'e1 Univ)
                (tc-expr/check/type #'e2 Univ)
                (tc-expr/check #'e3 expected))]
        ;; application
        [(#%plain-app . _) (tc/app/check form expected)]
        ;; #%expression
        [(#%expression e) (tc-expr/check #'e expected)]
        ;; syntax
        ;; for now, we ignore the rhs of macros
        [(letrec-syntaxes+values stxs vals . body)
         (tc-expr/check (syntax/loc form (letrec-values vals . body)) expected)]
        ;; begin
        [(begin e . es) (tc-exprs/check (syntax->list #'(e . es)) expected)]
        [(begin0 e . es)
         (begin (tc-exprs/check (syntax->list #'es) (old-ret Univ))
                (tc-expr/check #'e expected))]
        ;; if
        [(if tst thn els) (tc/if-twoarm #'tst #'thn #'els expected)]
        ;; lambda
        [(#%plain-lambda formals . body)
         (tc/lambda/check form #'(formals) #'(body) expected)]
        [(case-lambda [formals . body] ...)
         (tc/lambda/check form #'(formals ...) #'(body ...) expected)]
        ;; send
        [(let-values (((_) meth))
           (let-values (((_) rcvr))
             (let-values (((_) (~and find-app (#%plain-app find-method/who _ _ _))))
               (#%plain-app _ _ args ...))))
         (tc/send #'find-app #'rcvr #'meth #'(args ...) expected)]
        ;; kw function def
        [(let-values ([(_) fun])
           . body)
         #:when (syntax-property form 'kw-lambda)
         (match expected
           [(tc-result1: (and f (Function: _))) 
            ;(printf ">>> ~a\n" f)
            ;(printf ">>>\t ~a\n" (kw-convert f #:split #t))
            (tc-expr/check/type #'fun (kw-convert f #:split #t))]
           [(tc-result1: (Poly-names: names (and f (Function: _))))
            (tc-expr/check/type #'fun (make-Poly names (kw-convert f #:split #t)))]
           [(tc-result1: _) (tc-error/expr "Keyword functions must have function type, given ~a" expected)])
         expected]
        ;; let
        [(let-values ([(name ...) expr] ...) . body)
         (tc/let-values #'((name ...) ...) #'(expr ...) #'body form expected)]
        [(letrec-values ([(name) expr]) name*)
         #:when (and (identifier? #'name*) (free-identifier=? #'name #'name*))
         (match expected
           [(tc-result1: t)
            (with-lexical-env/extend (list #'name) (list t) (tc-expr/check #'expr expected))]
           [(tc-results: ts)
            (tc-error/expr #:return (ret (Un)) "Expected ~a values, but got only 1" (length ts))])]
        [(letrec-values ([(name ...) expr] ...) . body)
         (tc/letrec-values #'((name ...) ...) #'(expr ...) #'body form expected)]
        ;; other
        [_ (tc-error/expr #:return (ret expected) "cannot typecheck unknown form : ~a\n" (syntax->datum form))]
        ))))

;; type check form in the current type environment
;; if there is a type error in form, or if it has the wrong annotation, error
;; otherwise, produce the type of form
;; syntax[expr] -> type
(define (tc-expr form)
  ;; do the actual typechecking of form
  ;; internal-tc-expr : syntax -> Type
  (define (internal-tc-expr form)
    (syntax-parse form
      #:literal-sets (kernel-literals)
      #:literals (#%app lambda find-method/who)
      ;;
      [stx
       #:when (syntax-property form 'typechecker:with-handlers)
       (let ([ty (check-subforms/with-handlers form)])
         (unless ty
           (int-err "internal error: with-handlers"))
         ty)]
      [stx
       #:when (syntax-property form 'typechecker:ignore-some)
       (let ([ty (check-subforms/ignore form)])
         (unless ty
           (int-err "internal error: ignore-some"))
         ty)]
      ;; explicit failure
      [(quote-syntax ((~literal typecheck-fail-internal) stx msg var))
       (explicit-fail #'stx #'msg #'var)]
      ;; data
      [(quote #f) (ret (-val #f) false-filter)]
      [(quote #t) (ret (-val #t) true-filter)]

      [(quote val)  (ret (tc-literal #'val) true-filter)]
      ;; syntax
      [(quote-syntax datum) (ret (-Syntax (tc-literal #'datum)) true-filter)]
      ;; w-c-m
      [(with-continuation-mark e1 e2 e3)
       (begin (tc-expr/check/type #'e1 Univ)
              (tc-expr/check/type #'e2 Univ)
              (tc-expr #'e3))]
      ;; lambda
      [(#%plain-lambda formals . body)
       (tc/lambda form #'(formals) #'(body))]
      [(case-lambda [formals . body] ...)
       (tc/lambda form #'(formals ...) #'(body ...))]
      ;; send
      [(let-values (((_) meth))
         (let-values (((_) rcvr))
           (let-values (((_) (~and find-app (#%plain-app find-method/who _ _ _))))
             (#%plain-app _ _ args ...))))
       (tc/send #'find-app #'rcvr #'meth #'(args ...))]
      ;; let
      [(let-values ([(name ...) expr] ...) . body)
       (tc/let-values #'((name ...) ...) #'(expr ...) #'body form)]
      [(letrec-values ([(name ...) expr] ...) . body)
       (tc/letrec-values #'((name ...) ...) #'(expr ...) #'body form)]
      ;; mutation!
      [(set! id val)
       (match-let* ([(tc-result1: id-t) (tc-expr #'id)]
                    [(tc-result1: val-t) (tc-expr #'val)])
         (unless (subtype val-t id-t)
           (tc-error/expr "Mutation only allowed with compatible types:\n~a is not a subtype of ~a" val-t id-t))
         (ret -Void))]
      ;; top-level variable reference - occurs at top level
      [(#%top . id) (tc-id #'id)]
      ;; #%expression
      [(#%expression e) (tc-expr #'e)]
      ;; #%variable-reference
      [(#%variable-reference . _)
       (ret -Variable-Reference)]
      ;; identifiers
      [x:identifier (tc-id #'x)]
      ;; application
      [(#%plain-app . _) (tc/app form)]
      ;; if
      [(if tst thn els) (tc/if-twoarm #'tst #'thn #'els)]



      ;; syntax
      ;; for now, we ignore the rhs of macros
      [(letrec-syntaxes+values stxs vals . body)
       (tc-expr (syntax/loc form (letrec-values vals . body)))]

      ;; begin
      [(begin e . es) (tc-exprs (syntax->list #'(e . es)))]
      [(begin0 e . es)
       (begin (tc-exprs (syntax->list #'es))
              (tc-expr #'e))]
      ;; other
      [_
       (printf "~s\n" (continuation-mark-set->context (current-continuation-marks)))
       (tc-error/expr #:return (ret (Un)) "cannot typecheck unknown form : ~a\n" (syntax->datum form))]))

  (parameterize ([current-orig-stx form])
    ;(printf "form: ~a\n" (syntax->datum form))
    ;; the argument must be syntax
    (unless (syntax? form)
      (int-err "bad form input to tc-expr: ~a" form))
    ;; typecheck form
    (let ([ty (cond [(type-ascription form) => (lambda (ann)
                                                 (tc-expr/check form ann))]
                    [else (internal-tc-expr form)])])
      (match ty
        [(tc-results: ts fs os)
         (let* ([ts* (do-inst form ts)]
                [r (ret ts* fs os)])
           (add-typeof-expr form r)
           r)]))))

(define/cond-contract (tc/send form rcvr method args [expected #f])
  (-->* (syntax? syntax? syntax? syntax?) ((-or/c tc-results? #f)) tc-results?)
  (match (tc-expr rcvr)
    [(tc-result1: (Instance: (and c (Class: _ _ methods))))
     (match (tc-expr method)
       [(tc-result1: (Value: (? symbol? s)))
        (let* ([ftype (cond [(assq s methods) => cadr]
                            [else (tc-error/expr "send: method ~a not understood by class ~a" s c)])]
               [ret-ty (tc/funapp rcvr args (ret ftype) (map tc-expr (syntax->list args)) expected)]
               [retval (if expected
                           (begin (check-below ret-ty expected) expected)
                           ret-ty)])
          (add-typeof-expr form retval)
          retval)]
       [(tc-result1: t) (int-err "non-symbol methods not supported by Typed Racket: ~a" t)])]
    [(tc-result1: t) (tc-error/expr #:return (or expected (ret (Un))) "send: expected a class instance, got ~a" t)]))

(define (single-value form [expected #f])
  (define t (if expected (tc-expr/check form expected) (tc-expr form)))
  (match t
    [(tc-result1: _ _ _) t]
    [_ (tc-error/expr
          #:stx form
          #:return (or expected (ret (Un)))
     "expected single value, got multiple (or zero) values")]))

;; type-check a list of exprs, producing the type of the last one.
;; if the list is empty, the type is Void.
;; list[syntax[expr]] -> tc-result
(define (tc-exprs exprs)
  (cond [(null? exprs) (ret -Void)]
        [(null? (cdr exprs)) (tc-expr (car exprs))]
        [else (tc-expr/check/type (car exprs) Univ)
              (tc-exprs (cdr exprs))]))

(define (tc-exprs/check exprs expected)
  (cond [(null? exprs) (check-below (ret -Void) expected)]
        [(null? (cdr exprs)) (tc-expr/check (car exprs) expected)]
        [else (tc-expr/check/type (car exprs) Univ)
              (tc-exprs/check (cdr exprs) expected)]))
