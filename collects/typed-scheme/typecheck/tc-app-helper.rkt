#lang scheme/base

(require "../utils/utils.rkt" racket/match unstable/list
         (only-in srfi/1 unzip4) (only-in racket/list make-list)
         (utils tc-utils) (rep type-rep) (types utils union abbrev subtype))

(provide (all-defined-out))

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

(define (domain-mismatches ty doms rests drests rngs arg-tys tail-ty tail-bound
                           #:expected [expected #f])
  (define arguments-str
    (stringify-domain arg-tys
                      (if (not tail-bound) tail-ty #f)
                      (if tail-bound (cons tail-ty tail-bound) #f)))
  (cond
    [(null? doms)
     (int-err "How could doms be null: ~a ~a" ty)]
    [(and (= 1 (length doms)) (not (car rests)) (not (car drests)) (not tail-ty) (not tail-bound))
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
                  ""))))]
    [(= 1 (length doms))
     (string-append
      "Domain: "
      (stringify-domain (car doms) (car rests) (car drests))
      "\nArguments: "
      arguments-str
      "\n"
      (if expected
        (format "Result type: ~a\nExpected result: ~a\n"
                (car rngs) (make-printable expected))
        ""))]
    [else
     (let ([label  (if expected   "Types: "   "Domains: ")]
           [nl+spc (if expected "\n       " "\n         ")])
       ;; we restrict the domains shown in the error messages to those that
       ;; are useful
       (let-values ([(pdoms rngs rests drests) (possible-domains doms rests drests rngs expected)])
         (let ([pdoms (map make-printable pdoms)])
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
                "")))))]))


;; to avoid long and confusing error messages, in the case of functions with
;; multiple similar domains (<, >, +, -, etc.), we show only the domains that
;; are relevant to this specific error
;; this is done in several ways:
;; - if a case-lambda case is subsumed by another, we don't need to show it
;;   (subsumed cases may be useful for their filter information, but this is
;;   unrelated to error reporting)
;; - if we have an expected type, we don't need to show the domains for which
;;   the result type is not a subtype of the expected type
(define (possible-domains doms rests drests rngs expected)

  ;; is fun-ty subsumed by a function type in others?
  (define (is-subsumed-in? fun-ty others)
    ;; assumption: domains go from more specific to less specific
    ;;  thus, a domain can only be subsumed by another that is further down
    ;;  the list.
    ;; this is reasonable because a more specific domain coming after a more
    ;; general domain would never be matched
    ;; a case subsumes another if the first one is a subtype of the other
    (ormap (lambda (x) (subtype x fun-ty))
           others))

  (define expected-ty (and expected (match expected [(tc-result1: t) t])))
  (define (returns-subtype-of-expected? fun-ty)
    (and fun-ty ; was not skipped by a previous check
         (or (not expected)
             (match fun-ty
                    [(Function: (list (arr: _ rng _ _ _)))
                     (let ([rng (match rng
                                       [(Values: (list (Result: t _ _)))
                                        t]
                                       [(ValuesDots: (list (Result: t _ _)) _ _)
                                        t])])
                       (subtype rng expected-ty))]))))
  
  (let loop ([cases (map (compose make-Function list make-arr)
                         doms
                         (map (lambda (rng) ; strip filters
                                (match rng
                                  [(Values: (list (Result: t _ _) ...))
                                   (-values t)]
                                  [(ValuesDots: (list (Result: t _ _) ...) _ _)
                                   (-values t)]))
                              rngs)
                         rests drests (make-list (length doms) null))]
             [candidates '()])
    (if (not (null? cases))
        ;; discard subsumed cases
        (let ([head (car cases)] [tail (cdr cases)])
          (if (is-subsumed-in? head tail)
              (loop tail (cons #f   candidates)) ; will be skipped later
              (loop tail (cons head candidates))))
        ;; keep only the domains for which the associated function type
        ;; fits our criteria
        (unzip4 (map cdr ; doms, rests drests
                     (let* ([orig (map list
                                       (reverse candidates)
                                       doms
                                       rngs
                                       rests
                                       drests)]
                            [after (filter (compose returns-subtype-of-expected? car)
                                           orig)])
                       ;; if we somehow eliminate all the cases (bogus expected type)
                       ;; fall back to the showing extra cases
                       (if (null? after) orig after)))))))

(define (poly-fail t argtypes #:name [name #f] #:expected [expected #f])
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
           (tc-error/expr #:return (ret (Un))
                          (string-append
                           "Polymorphic " fcn-string " could not be applied to arguments:\n"
                           (domain-mismatches t msg-doms msg-rests msg-drests msg-rngs argtypes #f #f #:expected expected)
                           (if (not (for/and ([t (apply append (map fv/list msg-doms))]) (memq t msg-vars)))
                               (string-append "Type Variables: " (stringify msg-vars) "\n")
                               "")))))]
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
           (tc-error/expr #:return (ret (Un))
                          (string-append
                           "Polymorphic " fcn-string " could not be applied to arguments:\n"
                           (domain-mismatches t msg-doms msg-rests msg-drests msg-rngs argtypes #f #f #:expected expected)
                           (if (not (for/and ([t (apply append (map fv/list msg-doms))]) (memq t msg-vars)))
                               (string-append "Type Variables: " (stringify msg-vars) "\n")
                               "")))))]))
