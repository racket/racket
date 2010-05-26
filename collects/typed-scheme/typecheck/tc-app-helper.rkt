#lang scheme/base

(require "../utils/utils.rkt" scheme/match unstable/list
         (utils tc-utils) (rep type-rep) (types utils union abbrev))

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
           [nl+spc (if expected "\n       " "\n         ")]
           [pdoms  (map make-printable doms)])
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
          "")))]))

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
                           "Polymorphic " fcn-string " could not be applied to arguments:~n"
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
                           "Polymorphic " fcn-string " could not be applied to arguments:~n"
                           (domain-mismatches t msg-doms msg-rests msg-drests msg-rngs argtypes #f #f #:expected expected)
                           (if (not (for/and ([t (apply append (map fv/list msg-doms))]) (memq t msg-vars)))
                               (string-append "Type Variables: " (stringify msg-vars) "\n")
                               "")))))]))
