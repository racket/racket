#lang racket

(require "SL-syntax.rkt" "SL-semantics.rkt"
         "TL-syntax.rkt" "TL-semantics.rkt"
         "CMT.rkt"
         "common.rkt"
         redex)
(provide main same-result?)

(define (main . args)
  (define seed (add1 (random (sub1 (expt 2 31)))))
  (define unique-decomp-tests #f)
  (define CMT-range-all-tests #f)
  (define CMT-range-rules-tests #f)
  (define same-result-all-tests #f)
  (define same-result-rules-tests #f)
  
  (command-line
   #:argv args
   #:once-each
   ["--seed"
    n
    "Seed PRG with n"
    (set! seed (string->number n))]
   ["--unique-decomposition"
    n
    "Test unique decomposition of n expressions"
    (set! unique-decomp-tests (string->number n))]
   ["--CMT-range"
    n
    ("Test that CMT produces a TL expression for n expressions,"
     "chosen from SL's e non-terminal")
    (set! CMT-range-all-tests (string->number n))]
   ["--CMT-range-rules"
    n
    ("Test that CMT produces a TL expression for n expressions,"
     "chosen from the left-hand sides of -->SL")
    (set! CMT-range-rules-tests (string->number n))]
   ["--same-result"
    n
    ("Test that translation preserves meaning for n expression,"
     "chosen from SL's e non-terminal")
    (set! same-result-all-tests (string->number n))]
   ["--same-result-rules"
    n
    ("Test that translation preserves meaning for n expression,"
     "chosen from SL's e non-terminal")
    (set! same-result-rules-tests (string->number n))])
  
  (printf "Random seed: ~s\n" seed)
  
  (when unique-decomp-tests
    (test-unique-decomposition unique-decomp-tests))
  (when CMT-range-all-tests
    (test-CMT-range CMT-range-all-tests))
  (when CMT-range-rules-tests
    (test-CMT-range/rules CMT-range-rules-tests))
  (when same-result-all-tests
    (test-same-result same-result-all-tests))
  (when same-result-rules-tests
    (test-same-result/rules same-result-rules-tests)))

;; Unique decomposition
(define (test-unique-decomposition attempts)
  (redex-check SL e (uniquely-decomposes? (term e)) #:attempts attempts))

(define uniquely-decomposes?
  (let ([a? (redex-match SL a)]
        [decompositions (redex-match SL (in-hole T r))])
    (λ (e)
      (or (a? e)
          (match (decompositions e)
            [(list _) #t]
            [_ #f])))))

;; CMT produces TL expressions
(define (test-CMT-range attempts)
  (test-translation CMT-produces-TL?
                    #:attempts attempts))

(define (test-CMT-range/rules attempts)
  (test-translation CMT-produces-TL? 
                    #:attempts attempts 
                    #:source -->SL/no-side-conds))

(define CMT-produces-TL?
  (let ([TL-e? (redex-match TL e)])
    (λ (e) 
      (TL-e? (term (CMT ,e))))))

;; Translated programs have the same result
(define (test-same-result attempts)
  (test-translation same-result? 
                    #:attempts attempts))

(define (test-same-result/rules attempts)
  (test-translation same-result?
                    #:attempts attempts
                    #:source -->SL/no-side-conds))

(define (same-result? expr)
  (let/ec return
    (define SL-result
      (with-handlers ([normalization-timeout? return]
                      [eval-undefined? return])
        (parameterize ([max-normalization-steps 1000])
          (SL-eval (term (∅ / ,expr))))))
    (define TL-result
      (dynamic-wind
       (λ () (set-cache-size! 100))
       (λ () (TL-eval (term (∅ / (translate ,expr)))))
       (λ () (set-cache-size! 350))))
    (or (equal? SL-result TL-result)
        (compares-incomparable-keys? expr))))

(define SL-eval
  (make-eval -->SL (redex-match SL v)))
(define TL-eval
  (make-eval -->TL (redex-match TL v)))

;; Utilities
(define-syntax-rule (test-translation predicate . kw-args)
  (redex-check SL-gen (Σ / e) 
               (predicate (term e))
               #:prepare close-program . kw-args))

(define-extended-language SL-gen SL
  (σ (ref (variable-except resume restore-marks c-w-i-c-m map-set kont/ms equal? last)))
  (K (side-condition string_1 (not (member (term string_1) '("square" "diamond")))))
  
  (K-tree (K K-tree ...)))

(define -->SL/no-side-conds
  ; Disables side-conditions on some rules.
  (extend-reduction-relation
   -->SL SL
   (--> (Σ / (in-hole E (match (K v ...) l ...))) any "2")
   (--> (Σ / (in-hole E (σ v ...))) any "4")
   (--> (Σ / (in-hole E_1 (σ v))) any "4’")))

(define close-program
  (match-lambda
    [`(,Σ / ,e)
     `(,Σ / ,(close-expr e))]))

(define close-expr
  (let ([var? (redex-match SL x)]
        [val? (redex-match SL v)])
    (λ (expr)
      ; 1. Considers refs to be bound only within the binding letrec.
      ; 2. Avoids adding duplicate mark to an existing frame, since 
      ; doing so (a) can make a continuation value syntactically invalid
      ; and (b) can change the reduction rule that applies
      (let recur ([expr expr] [vars (set)] [refs (set)] [keys (set)])
        (match expr
          [(? var?)
           (if (set-empty? vars)
               (if (set-empty? refs)
                   (random-constant)
                   `(ref ,(random-element refs)))
               (if (set-member? vars expr)
                   expr
                   (random-element vars)))]
          [`(ref ,σ)
           (if (set-empty? refs)
               (random-constant)
               (if (set-member? refs σ)
                   expr
                   `(ref ,(random-element refs))))]
          [`(w-c-m ,k ,v ,e)
           (define k’
             (if (val? k)
                 k
                 (let retry ([candidate (recur k vars refs keys)]
                             [retries 0])
                   (when (> retries 3)
                     (error 'close "too many"))
                   (if (set-member? keys candidate)
                       (retry (list (first (random-constant)) candidate)
                              (add1 retries))
                       candidate))))
           (define ext-keys
             (if (val? k’)
                 (set-add keys k’)
                 keys))
           `(w-c-m ,k’ ,(recur v vars refs (set))
                   ,(recur e vars refs ext-keys))]
          [`(letrec ([(ref ,σs) ,es] ...) ,e)
           (define ext-refs (extend refs σs))
           `(letrec ,(for/list ([e es] [σ σs])
                               `[(ref ,σ) ,(recur e vars ext-refs (set))])
              ,(recur e vars ext-refs (set)))]
          [`(match ,e [(,ks ,xss ...) ,es] ...)
           (define ext-vars (extend vars (apply append xss)))
           `(match ,(recur e vars refs (set))
              ,@(for/list ([xs xss] [e es] [k ks])
                  `[(,k ,@xs) ,(recur e ext-vars refs (set))]))]
          [`(λ ,xs ,e)
           (define ext-vars (extend vars xs))
           `(λ ,xs ,(recur e ext-vars refs (set)))]
          [(? list?)
           (map (λ (e) (recur e vars refs (set))) expr)]
          [_ expr])))))

(define (extend s xs)
  (for/fold ([s s]) ([x xs])
            (set-add s x)))

(define random-constant
  (let ([generate-K-tree (generate-term SL-gen K-tree)])
    (λ () (generate-K-tree (random 3)))))

(define (random-element xs)
  (list-ref (set-map xs values) (random (set-count xs))))

(define (compares-incomparable-keys? expr)
  (define procedure? (redex-match SL (λ (x ...) e)))
  (define continuation? (redex-match SL (κ E)))
  (let/ec return
    (apply-reduction-relation*
     (extend-reduction-relation 
      -->SL SL
      (--> (Σ / (in-hole E (c-c-m [v ...])))
           ,(if (andmap comparable? (term (v ...)))
                (term (Σ / (in-hole E (χ (v ...) E ("nil")))))
                (return #t))
           "8"))
     `(∅ / ,expr))
    #f))

(define comparable?
  (let ()
    (define-extended-language SL’ SL
      (c σ (K c ...)))
    (redex-match SL’ c)))
