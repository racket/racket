#lang scheme/base

(require "../utils/utils.rkt" racket/match unstable/list unstable/sequence
         (only-in srfi/1 unzip4) (only-in racket/list make-list)
         (prefix-in c: racket/contract)
         "check-below.rkt" "tc-subst.rkt"
         (utils tc-utils)
         (rep type-rep object-rep)
         (types utils union abbrev subtype))

(provide (all-defined-out))


;; syntax? syntax? arr? (listof tc-results?) (or/c #f tc-results) [boolean?] -> tc-results?
(define/cond-contract (tc/funapp1 f-stx args-stx ftype0 argtys expected #:check [check? #t])
  ((syntax? (c:and/c syntax? syntax->list) arr? (c:listof tc-results?) (c:or/c #f tc-results?)) (#:check boolean?) . c:->* . tc-results?)
  (match* (ftype0 argtys)
    ;; we check that all kw args are optional
    [((arr: dom (Values: (and results (list (Result: t-r f-r o-r) ...))) rest #f (and kws (list (Keyword: _ _ #f) ...)))
      (list (tc-result1: t-a phi-a o-a) ...))
     (when check?
       (cond [(and (not rest) (not (= (length dom) (length t-a))))
              (tc-error/expr #:return (ret t-r)
                             "Wrong number of arguments, expected ~a and got ~a" (length dom) (length t-a))]
             [(and rest (< (length t-a) (length dom)))
              (tc-error/expr #:return (ret t-r)
                             "Wrong number of arguments, expected at least ~a and got ~a" (length dom) (length t-a))])
       (for ([dom-t (if rest (in-sequence-forever dom rest) (in-list dom))]
             [a (in-list (syntax->list args-stx))]
             [arg-t (in-list t-a)])
         (parameterize ([current-orig-stx a]) (check-below arg-t dom-t))))
     (let* ([dom-count (length dom)]
            [arg-count (+ dom-count (if rest 1 0) (length kws))])
       (let-values
           ([(o-a t-a) (for/lists (os ts)
                         ([nm (in-range arg-count)]
                          [oa (in-sequence-forever (in-list o-a) (make-Empty))]
                          [ta (in-sequence-forever (in-list t-a) (Un))])
                         (values (if (>= nm dom-count) (make-Empty) oa)
                                 ta))])
         (define-values (t-r f-r o-r)
           (for/lists (t-r f-r o-r)
             ([r (in-list results)])             
             (open-Result r o-a t-a)))
         (ret t-r f-r o-r)))]
    [((arr: _ _ _ drest '()) _)
     (int-err "funapp with drest args ~a ~a NYI" drest argtys)]
    [((arr: _ _ _ _ kws) _)
     (int-err "funapp with keyword args ~a NYI" kws)]))


(define (make-printable t)
  (match t
    [(tc-result1: t) t]
    [(tc-results: ts) (-values ts)]
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
(define/cond-contract (domain-mismatches f-stx args-stx ty doms rests drests rngs arg-tys tail-ty tail-bound
                                         #:expected [expected #f] #:return [return (make-Union null)]
                                         #:msg-thunk [msg-thunk (lambda (dom) dom)])
   ((syntax? syntax? Type/c (c:listof (c:listof Type/c)) (c:listof (c:or/c #f Type/c))
     (c:listof (c:or/c #f (c:cons/c Type/c (c:or/c c:natural-number/c symbol?))))
     (c:listof (c:or/c Values? ValuesDots?)) (c:listof tc-results?) (c:or/c #f Type/c) c:any/c)
    (#:expected (c:or/c #f tc-results?) #:return tc-results?
     #:msg-thunk (c:-> string? string?))
    . c:->* . tc-results?)

  (define arguments-str
    (stringify-domain arg-tys
                      (if (not tail-bound) tail-ty #f)
                      (if tail-bound (cons tail-ty tail-bound) #f)))
  (cond
    [(null? doms)
     (int-err "How could doms be null: ~a ~a" ty)]
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
     (let ([label  (if expected   "Types: "   "Domains: ")]
           [nl+spc (if expected "\n       " "\n         ")])
       ;; we restrict the domains shown in the error messages to those that
       ;; are useful
       (match-let ([(list pdoms prngs prests pdrests) (possible-domains doms rests drests rngs expected)])
         ;; if we somehow eliminate all the cases (bogus expected type) fall back to showing the
         ;; extra cases
         (let-values ([(pdoms rngs rests drests)
                       (if (null? pdoms)
                           (values doms rngs rests drests)
                           (values pdoms prngs prests pdrests))])
           (if (= (length pdoms) 1)
               ;; if we narrowed down the possible cases to a single one, have
               ;; tc/funapp1 generate a better error message
               (begin (tc/funapp1 f-stx args-stx
                                  (make-arr (car pdoms) (car rngs)
                                            (car rests) (car drests) null)
                                  arg-tys expected)
                      return)
               ;; if not, print the message as usual
               (let* ([pdoms (map make-printable pdoms)]
                      [err-doms
                       (string-append
                        label
                        (stringify (if expected
                                       (map stringify-domain pdoms rests drests rngs)
                                       (map stringify-domain pdoms rests drests))
                                   nl+spc)
                        "\nArguments: "
                        arguments-str
                        "\n"
                        (if expected
                            (format "Expected result: ~a\n" (make-printable expected))
                            ""))])
                 (tc-error/expr
                  #:return return
                  (msg-thunk err-doms)))))))])) ; generate message


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
(define (possible-domains doms rests drests rngs expected)

  ;; If we fail, no big deal. We just don't prune the type.
  (with-handlers ([exn:fail? (lambda (e) (list doms rngs rests drests))])

    ;; is fun-ty subsumed by a function type in others?
    (define (is-subsumed-in? fun-ty others)
      ;; a case subsumes another if the first one is a subtype of the other
      (ormap (lambda (x) (subtype x fun-ty))
             others))

    ;; currently does not take advantage of multi-valued expected types
    (define expected-ty (and expected (match expected [(tc-result1: t) t] [_ #f])))
    (define (returns-subtype-of-expected? fun-ty)
      (or (not expected)
          (match fun-ty
            [(Function: (list (arr: _ rng _ _ _)))
             (let ([rng (match rng
                          [(Values: (list (Result: t _ _)))
                           t]
                          [(ValuesDots: (list (Result: t _ _)) _ _)
                           t])])
               (subtype rng expected-ty))])))

    (define orig (map list doms rngs rests drests))

    (define cases
      (map (compose make-Function list make-arr)
           doms
           (map (match-lambda ; strip filters
                 [(Values: (list (Result: t _ _) ...))
                  (-values t)]
                 [(ValuesDots: (list (Result: t _ _) ...) _ _)
                  (-values t)])
                rngs)
           rests drests (make-list (length doms) null)))

    ;; iterate in lock step over the function types we analyze and the parts
    ;; that we will need to print the error message, to make sure we throw
    ;; away cases consistently
    (let loop ([cases cases]
               ;; the parts we'll need to print the error message
               [parts orig]
               ;; accumulators
               [candidates '()] ; from cases
               [parts-acc '()]) ; from parts

      ;; keep only the domains for which the associated function type
      ;; is consistent with the expected type
      (if (not (null? cases))
          (if (returns-subtype-of-expected? (car cases))
              (loop (cdr cases) (cdr parts)
                    (cons (car cases) candidates) ; we keep this one
                    (cons (car parts) parts-acc))
              (loop (cdr cases) (cdr parts)
                    candidates parts-acc)) ; we discard this one

          ;; among the domains that fit with the expected type, we only
          ;; need to keep the most liberal
          ;; since we only care about permissiveness of domains, we
          ;; reconstruct function types with a return type of any then test
          ;; for subtyping
          (let ([fun-tys-ret-any
                 (map (match-lambda
                       [(Function: (list (arr: dom _ rest drest _)))
                        (make-Function (list (make-arr dom
                                                       (-values (list Univ))
                                                       rest drest null)))])
                      candidates)])

            ;; Heuristic: often, the last case in the definition (first at
            ;; this point, we've reversed the list) is the most general of
            ;; all, subsuming all the others. If that's the case, just go
            ;; with it. Otherwise, go the slow way.
            (if (and (not (null? fun-tys-ret-any))
                     (andmap (lambda (c) (subtype (car fun-tys-ret-any) c))
                             fun-tys-ret-any))
                ;; Yep. Return early.
                (map list (car parts-acc))

                ;; No luck, do it the slow way
                (let loop ([cases fun-tys-ret-any]
                           [parts parts-acc]
                           ;; accumulators
                           ;; final pass, we only need the parts to print the
                           ;; error message
                           [parts-acc '()])
                  (if (not (null? cases))
                      ;; if a case is a supertype of another, we discard it
                      (let ([head (car cases)])
                        (if (is-subsumed-in? head (remove head fun-tys-ret-any))
                            (loop (cdr cases) (cdr parts)
                                  parts-acc) ; we discard this one
                            (loop (cdr cases) (cdr parts)
                                  (cons (car parts) parts-acc)))) ; we keep this one

                      (call-with-values
                          (lambda () (unzip4 (reverse parts-acc)))
                        list)))))))))

;; Wrapper over possible-domains that works on types.
(define (cleanup-type t [expected #f])
  (match t
    ;; function type, prune if possible.
    [(Function: (list (arr: doms rngs rests drests kws) ...))
     (match-let ([(list pdoms rngs rests drests) (possible-domains doms rests drests rngs (and expected (ret expected)))])
       (let ([res (make-Function (map make-arr
                                      pdoms rngs rests drests (make-list (length pdoms) null)))])
         res))]
    ;; not a function type. keep as is.
    [_ t]))

(define (poly-fail f-stx args-stx t argtypes #:name [name #f] #:expected [expected #f])
  (match t
    [(or (Poly-names:
          msg-vars
          (Function: (list (arr: msg-doms msg-rngs msg-rests msg-drests (list (Keyword: _ _ #f) ...)) ...)))
         (PolyDots-names:
          msg-vars
          (Function: (list (arr: msg-doms msg-rngs msg-rests msg-drests (list (Keyword: _ _ #f) ...)) ...))))
     (let ([fcn-string (if name
                           (format "function ~a" (syntax->datum name))
                           "function")])
       (if (and (andmap null? msg-doms)
                (null? argtypes))
           (tc-error/expr #:return (ret (Un))
                          (string-append
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (domain-mismatches f-stx args-stx t msg-doms msg-rests msg-drests
                              msg-rngs argtypes #f #f #:expected expected
                              #:return (ret (Un))
                              #:msg-thunk (lambda (dom)
                                            (string-append
                                             "Polymorphic " fcn-string " could not be applied to arguments:\n"
                                             dom
                                             (if (not (for/and ([t (apply append (map fv/list msg-doms))]) (memq t msg-vars)))
                                                 (string-append "Type Variables: " (stringify msg-vars) "\n")
                                                 ""))))))]
    [(or (Poly-names: msg-vars (Function: (list (arr: msg-doms msg-rngs msg-rests msg-drests kws) ...)))
         (PolyDots-names: msg-vars (Function: (list (arr: msg-doms msg-rngs msg-rests msg-drests kws) ...))))
     (let ([fcn-string (if name
                           (format "function with keywords ~a" (syntax->datum name))
                           "function with keywords")])
       (if (and (andmap null? msg-doms)
                (null? argtypes))
           (tc-error/expr #:return (ret (Un))
                          (string-append
                           "Could not infer types for applying polymorphic "
                           fcn-string
                           "\n"))
           (domain-mismatches f-stx args-stx t msg-doms msg-rests msg-drests
                              msg-rngs argtypes #f #f #:expected expected
                              #:return (ret (Un))
                              #:msg-thunk (lambda (dom)
                                            (string-append
                                             "Polymorphic " fcn-string " could not be applied to arguments:\n"
                                             dom
                                             (if (not (for/and ([t (apply append (map fv/list msg-doms))]) (memq t msg-vars)))
                                                 (string-append "Type Variables: " (stringify msg-vars) "\n")
                                                 ""))))))]))
