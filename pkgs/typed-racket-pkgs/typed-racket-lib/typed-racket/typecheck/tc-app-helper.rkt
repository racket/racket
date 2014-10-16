#lang racket/base

(require "../utils/utils.rkt"
         racket/match unstable/list unstable/sequence racket/set racket/list
         (only-in srfi/1 unzip4) (only-in racket/list make-list)
         (contract-req)
         (typecheck check-below tc-subst)
         (utils tc-utils)
         (rep type-rep filter-rep)
         (except-in (types utils union abbrev subtype)
                    -> ->* one-of/c))
(require-for-cond-contract
  syntax/stx)

(provide/cond-contract
  [tc/funapp1
    ((syntax? stx-list? arr? (listof tc-results/c) (or/c #f tc-results/c))
     (#:check boolean?)
     . ->* . full-tc-results/c)])
(define (tc/funapp1 f-stx args-stx ftype0 argtys expected #:check [check? #t])
  (match* (ftype0 argtys)
    ;; we check that all kw args are optional
    [((arr: dom rng rest #f (and kws (list (Keyword: _ _ #f) ...)))
      (list (tc-result1: t-a phi-a o-a) ...))

     (when check?
       (cond [(and (not rest) (not (= (length dom) (length t-a))))
              (tc-error/fields "could not apply function"
                               #:more "wrong number of arguments provided"
                               "expected" (length dom)
                               "given" (length t-a)
                               #:delayed? #t)]
             [(and rest (< (length t-a) (length dom)))
              (tc-error/fields "could not apply function"
                               #:more "wrong number of arguments provided"
                               "expected at least" (length dom)
                               "given" (length t-a)
                               #:delayed? #t)])
       (for ([dom-t (if rest (in-sequence-forever dom rest) (in-list dom))]
             [a (in-syntax args-stx)]
             [arg-t (in-list t-a)])
         (parameterize ([current-orig-stx a]) (check-below arg-t dom-t))))
     (let* ([dom-count (length dom)])
       ;; Currently do nothing with rest args and keyword args as there are no support for them in
       ;; objects yet.
       (let-values
           ([(o-a t-a) (for/lists (os ts)
                         ([_ (in-range dom-count)]
                          [oa (in-sequence-forever (in-list o-a) -empty-obj)]
                          [ta (in-sequence-forever (in-list t-a) Univ)])
                         (values oa ta))])
           (values->tc-results rng o-a t-a)))]
    ;; this case should only match if the function type has mandatory keywords
    ;; but no keywords were provided in the application
    [((arr: _ _ _ _
            ;; at least one mandatory keyword
            (app (λ (kws)
                   (for/or ([keyword (in-list kws)])
                     (match keyword
                       [(Keyword: kw _ #t) kw]
                       [_ #f])))
                 (? values req-kw))) _)
     (when check?
       (tc-error/fields "could not apply function"
                        #:more "a required keyword was not supplied"
                        "missing keyword" req-kw))]
    [((arr: _ _ _ drest '()) _)
     (int-err "funapp with drest args ~a ~a NYI" drest argtys)]
    [((arr: _ _ _ _ kws) _)
     (int-err "funapp with keyword args ~a NYI" kws)]))


(define (make-printable t)
  (match t
    [(tc-result1: t) t]
    [(tc-results: ts) (-values ts)]
    [(tc-any-results: f) (-AnyValues -top)]
    [_ t]))

(define (stringify-domain dom rst drst [rng #f])
  (let ([doms-string (if (null? dom) "" (string-append (stringify (map make-printable dom)) " "))]
        [rng-string (if rng (format " -> ~a" rng) "")])
    (cond [drst
           (format "~a~a ... ~a~a" doms-string (car drst) (cdr drst) rng-string)]
          [rst
           (format "~a~a *~a" doms-string rst rng-string)]
          [else (string-append (stringify (map make-printable dom)) rng-string)])))

;; Generates error messages when operand types don't match operator domains.
(provide/cond-contract
  [domain-mismatches
   ((syntax? syntax? Type/c (listof (listof Type/c)) (listof (or/c #f Type/c))
     (listof (or/c #f (cons/c Type/c (or/c natural-number/c symbol?))))
     (listof SomeValues/c) (listof tc-results?) (or/c #f Type/c) any/c)
    (#:expected (or/c #f tc-results/c)
     #:return tc-results?
     #:msg-thunk (-> string? string?))
    . ->* . tc-results/c)])
(define (domain-mismatches f-stx args-stx ty doms rests drests rngs arg-tys tail-ty tail-bound
                           #:expected [expected #f] #:return [return (ret -Bottom)]
                           #:msg-thunk [msg-thunk (lambda (dom) dom)])
  (define arguments-str
    (stringify-domain arg-tys
                      (if (not tail-bound) tail-ty #f)
                      (if tail-bound (cons tail-ty tail-bound) #f)))
  (cond
    [(null? doms)
     (tc-error/expr/fields
      "cannot apply a function with unknown arity"
      #:more (format "~a has type Procedure which cannot be applied"
                     (name->function-str (and (identifier? f-stx) f-stx)))
      #:return return)]
    [(and (= 1 (length doms)) (not (car rests)) (not (car drests)) (not tail-ty) (not tail-bound))
     (tc-error/expr
      #:return return
      (msg-thunk
       (apply string-append
              (if (not (= (length (car doms)) (length arg-tys)))
                  (format "Wrong number of arguments - Expected ~a, but got ~a\n\n" (length (car doms)) (length arg-tys))
                  "")
              (append
               (for/list ([dom-t (in-list (extend arg-tys (car doms) #f))]
                          [arg-t (in-list (extend (car doms) arg-tys #f))]
                          [i (in-naturals 1)])
                         (let ([dom-t (or dom-t "-none-")]
                               [arg-t (or arg-t "-none-")])
                           (format "Argument ~a:\n  Expected: ~a\n  Given:    ~a\n" i (make-printable dom-t) (make-printable arg-t))))
               (list
                (if expected
                    (format "\nResult type:     ~a\nExpected result: ~a\n"
                            (car rngs) (make-printable expected))
                    ""))))))]
    [(= 1 (length doms))
     (tc-error/expr
      #:return return
      (msg-thunk
       (string-append
        "Domain: "
        (stringify-domain (car doms) (car rests) (car drests))
        "\nArguments: "
        arguments-str
        "\n"
        (if expected
            (format "Result type: ~a\nExpected result: ~a\n"
                    (car rngs) (make-printable expected))
            ""))))]
    [else
     (define label  (if expected   "Types: "   "Domains: "))
     (define nl+spc (if expected "\n       " "\n         "))
     ;; we restrict the domains shown in the error messages to those that
     ;; are useful
     (match-let ([(list pdoms prngs prests pdrests) (possible-domains doms rests drests rngs expected)])
       ;; if we somehow eliminate all the cases (bogus expected type) fall back to showing the
       ;; extra cases
       (let-values ([(pdoms rngs rests drests)
                     (if (null? pdoms)
                         (values doms rngs rests drests)
                         (values pdoms prngs prests pdrests))])
         ;; only use `tc/funapp1` if `tail-ty` was *not* provided
         ;; since it either won't error correctly or produces a poor error
         (cond [(and (not tail-ty) (= (length pdoms) 1))
                ;; if we narrowed down the possible cases to a single one, have
                ;; tc/funapp1 generate a better error message
                (tc/funapp1 f-stx args-stx
                            (make-arr (car pdoms) (car rngs)
                                      (car rests) (car drests) null)
                            arg-tys expected)
                return]
               [else
                ;; if not, print the message as usual
                (define pdoms* (map make-printable pdoms))
                (define err-doms
                  (string-append
                   label
                   (stringify (if expected
                                  (map stringify-domain pdoms* rests drests rngs)
                                  (map stringify-domain pdoms* rests drests))
                              nl+spc)
                   "\nArguments: "
                   arguments-str
                   "\n"
                   (if expected
                       (format "Expected result: ~a\n" (make-printable expected))
                       "")))
                (tc-error/expr
                 #:return return
                 (msg-thunk err-doms))])))])) ; generate message


;; to avoid long and confusing error messages, in the case of functions with
;; multiple similar domains (<, >, +, -, etc.), we show only the domains that
;; are relevant to this specific error
;; this is done in several ways:
;; - if a case-lambda case is subsumed by another, we don't need to show it
;;   (subsumed cases may be useful for their filter information, but this is
;;   unrelated to error reporting)
;; - if we have an expected type, we don't need to show the domains for which
;;   the result type is not a subtype of the expected type
;; - we can disregard domains that are more restricted than required to get
;;   the expected type (or all but the most liberal domain when no type is
;;   expected)
;;   ex: if we have the 2 following possible domains for an operator:
;;       Fixnum -> Fixnum
;;       Integer -> Integer
;;     and an expected type of Integer for the result of the application,
;;     we can disregard the Fixnum domain since it imposes a restriction that
;;     is not necessary to get the expected type
;; This function can be used in permissive or restrictive mode.
;; in permissive mode, domains that are not consistent with the expected type
;; may still be considered possible. This is useful for error messages, where
;; we want to collapse domains always, regardless of expected type. In
;; restrictive mode, only domains that are consistent with the expected type can
;; be considered possible. This is useful when computing the possibly empty set
;; of domains that would *satisfy* the expected type, e.g. for the :query-type
;; forms.
;; TODO separating pruning and collapsing into separate functions may be nicer
(define (possible-domains doms rests drests rngs expected [permissive? #t])

  ;; is fun-ty subsumed by a function type in others?
  (define (is-subsumed-in? fun-ty others)
    ;; a case subsumes another if the first one is a subtype of the other
    (ormap (lambda (x) (subtype x fun-ty))
           others))

  ;; currently does not take advantage of multi-valued or arbitrary-valued expected types,
  (define expected-ty
    (and expected
         (match expected
           [(tc-result1: t) t]
           [(tc-any-results: (or (Top:) (NoFilter:))) #t] ; anything is a subtype of expected
           [_ #f]))) ; don't know what it is, don't do any pruning
  (define (returns-subtype-of-expected? fun-ty)
    (or (not expected) ; no expected type, anything is fine
        (eq? expected-ty #t) ; expected is tc-anyresults, anything is fine
        (and expected-ty ; not some unknown expected tc-result
             (match fun-ty
               [(Function: (list (arr: _ rng _ _ _)))
                (let ([rng (match rng
                             [(Values: (list (Result: t _ _)))
                              t]
                             [(ValuesDots: (list (Result: t _ _)) _ _)
                              t]
                             [_ #f])])
                  (and rng (subtype rng expected-ty)))]))))

  (define orig (map list doms rngs rests drests))

  (define cases
    (map (compose make-Function list make-arr)
         doms
         (map (match-lambda ; strip filters
               [(AnyValues: f) (-AnyValues -top)]
               [(Values: (list (Result: t _ _) ...))
                (-values t)]
               [(ValuesDots: (list (Result: t _ _) ...) _ _)
                (-values t)])
              rngs)
         rests drests (make-list (length doms) null)))

  ;; iterate in lock step over the function types we analyze and the parts
  ;; that we will need to print the error message, to make sure we throw
  ;; away cases consistently
  (define-values (candidates* parts-acc*)
    (for/fold ([candidates '()] ; from cases
               [parts-acc '()]) ; from orig
        ([c (in-list cases)]
         ;; the parts we'll need to print the error message
         [p (in-list orig)])
      (if (returns-subtype-of-expected? c)
          (values (cons c candidates) ; we keep this one
                  (cons p parts-acc))
          ;; we discard this one
          (values candidates parts-acc))))

  ;; if none of the cases return a subtype of the expected type, still do some
  ;; merging, but do it on the entire type
  ;; only do this if we're in permissive mode
  (define-values (candidates parts-acc)
    (if (and permissive? (null? candidates*))
        (values cases orig)
        (values candidates* parts-acc*)))

  ;; among the domains that fit with the expected type, we only need to
  ;; keep the most liberal
  ;; since we only care about permissiveness of domains, we reconstruct
  ;; function types with a return type of any then test for subtyping
  (define fun-tys-ret-any
    (map (match-lambda
          [(Function: (list (arr: dom _ rest drest _)))
           (make-Function (list (make-arr dom
                                          (-values (list Univ))
                                          rest drest null)))])
         candidates))

  ;; Heuristic: often, the last case in the definition (first at this
  ;; point, we've reversed the list) is the most general of all, subsuming
  ;; all the others. If that's the case, just go with it. Otherwise, go
  ;; the slow way.
  (cond [(and (not (null? fun-tys-ret-any))
              (andmap (lambda (c) (subtype (car fun-tys-ret-any) c))
                      fun-tys-ret-any))
         ;; Yep. Return early.
         (map list (car parts-acc))]
        
        [else
         ;; No luck, do it the slow way
         (define parts-res
           ;; final pass, we only need the parts to print the error message
           (for/fold ([parts-res '()])
               ([c (in-list fun-tys-ret-any)]
                [p (in-list parts-acc)]
                ;; if a case is a supertype of another, we discard it
                #:unless (is-subsumed-in? c (remove c fun-tys-ret-any)))

             (cons p parts-res)))

         (call-with-values
             (lambda () (unzip4 (reverse parts-res)))
           list)]))

;; Wrapper over possible-domains that works on types.
(provide/cond-contract
  [cleanup-type ((Type/c) ((or/c #f Type/c) any/c) . ->* . Type/c)])
(define (cleanup-type t [expected #f] [permissive? #t])
  (match t
    ;; function type, prune if possible.
    [(Function/arrs: doms rngs rests drests kws)
     (match-let ([(list pdoms rngs rests drests)
                  (possible-domains doms rests drests rngs
                                    (and expected (ret expected))
                                    permissive?)])
       (if (= (length pdoms) (length doms))
           ;; pruning didn't improve things, return the original
           ;; (Note: pruning may have reordered clauses, so may not be `equal?' to
           ;;  the original, which may confuse `:print-type''s pruning detection)
           t
           ;; pruning helped, return pruned type
           (make-Function (map make-arr
                               pdoms rngs rests drests (make-list (length pdoms) null)))))]
    ;; not a function type. keep as is.
    [_ t]))

(provide/cond-contract
  [poly-fail ((syntax? syntax? Type/c (listof tc-results?))
              (#:name (or/c #f syntax?)
               #:expected (or/c #f tc-results/c))
              . ->* . tc-results/c)])
(define (poly-fail f-stx args-stx t argtypes #:name [name #f] #:expected [expected #f])
  (match t
    [(or (Poly-names:
          msg-vars
          (Function/arrs: msg-doms msg-rngs msg-rests msg-drests (list (Keyword: _ _ #f) ...)))
         (PolyDots-names:
          msg-vars
          (Function/arrs: msg-doms msg-rngs msg-rests msg-drests (list (Keyword: _ _ #f) ...)))
         (PolyRow-names:
          msg-vars _
          (Function/arrs: msg-doms msg-rngs msg-rests msg-drests (list (Keyword: _ _ #f) ...))))
     (let ([fcn-string (name->function-str name)])
       (if (and (andmap null? msg-doms)
                (null? argtypes))
           (tc-error/expr (string-append
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (domain-mismatches f-stx args-stx t msg-doms msg-rests msg-drests
                              msg-rngs argtypes #f #f #:expected expected
                              #:msg-thunk (lambda (dom)
                                            (string-append
                                             "Polymorphic " fcn-string " could not be applied to arguments:\n"
                                             dom
                                             (if (not (subset? (apply set-union (seteq) (map fv/list msg-doms))
                                                               (list->seteq msg-vars)))
                                                 (string-append "Type Variables: " (stringify msg-vars) "\n")
                                                 ""))))))]
    [(or (Poly-names: msg-vars (Function/arrs: msg-doms msg-rngs msg-rests msg-drests kws))
         (PolyDots-names: msg-vars (Function/arrs: msg-doms msg-rngs msg-rests msg-drests kws))
         (PolyRow-names: msg-vars _ (Function/arrs: msg-doms msg-rngs msg-rests msg-drests kws)))
     (let ([fcn-string (if name
                           (format "function with keywords ~a" (syntax->datum name))
                           "function with keywords")])
       (if (and (andmap null? msg-doms)
                (null? argtypes))
           (tc-error/expr (string-append
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (domain-mismatches f-stx args-stx t msg-doms msg-rests msg-drests
                              msg-rngs argtypes #f #f #:expected expected
                              #:msg-thunk (lambda (dom)
                                            (string-append
                                             "Polymorphic " fcn-string " could not be applied to arguments:\n"
                                             dom
                                             (if (not (subset? (apply set-union (seteq) (map fv/list msg-doms))
                                                               (list->seteq msg-vars)))
                                                 (string-append "Type Variables: " (stringify msg-vars) "\n")
                                                 ""))))))]))

;; name->function-str : (Option Identifier) -> String
;; Produce a function name string for error messages
(define (name->function-str name)
  (if name
      (format "function `~a'" (syntax->datum name))
      "function"))
