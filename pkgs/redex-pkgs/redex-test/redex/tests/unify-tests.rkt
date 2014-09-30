#lang racket

(require (for-syntax redex/private/rewrite-side-conditions)
         redex/private/pat-unify
         redex/reduction-semantics
         redex/private/jdg-gen
         rackunit)

(provide env-equal?)

(define unify*-untested (make-hash))
(for* ([p1 (in-list pat*-clause-p?s)]
       [p2 (in-list pat*-clause-p?s)])
  (hash-set! unify*-untested
             (cons (object-name p1) 
                   (object-name p2))
             #t))

(define (log-test p1 p2)
  (define (pat->pred-name p)
    (ormap (λ (pred?) (and (pred? p) (object-name pred?)))
           pat*-clause-p?s))
  (hash-remove! unify*-untested
                (cons (pat->pred-name p1)
                      (pat->pred-name p2))))

(define (contains-name-pat? p)
  (cond
    [(and (pair? p) (eq? (car p) 'name)) #t]
    [(list? p) (ormap contains-name-pat? p)]
    [else #f]))

(define (unify/lt p1 p2 e l)
  (unless (or (contains-name-pat? p1)
              (contains-name-pat? p2))
    (log-test p1 p2))
  (unify p1 p2 e l))

(define (unify*/lt p1 p2 e l)
  (log-test p1 p2)
  (unify* p1 p2 e l))

(define-syntax m-hash
  (λ (stx)
    (syntax-case stx ()
      [(m-hash kvs ...)
       #'(hash kvs ...)])))

(define (p*e-equal? pe-left pe-right)
  (and (equal? (p*e-p pe-left)
               (p*e-p pe-right))
       (env-equal? (p*e-e pe-left)
                   (p*e-e pe-right))))

(define (env-equal? e-left e-right)
  (and (equal? (env-eqs e-left)
               (env-eqs e-right))
       (dqs-equal? (env-dqs e-left)
                   (env-dqs e-right))))

(define (dqs->dqset dqs)
  (for/set ([the-dq (in-list dqs)])
           (match the-dq
             [(dq ps dq-e)
              (for/fold ([dq-pairs (set)])
                ([ls (first dq-e)]
                 [rs (second dq-e)])
                (set-add dq-pairs (cons ls rs)))])))

(check-equal? (dqs->dqset (list (dq '() `((list (name a ,(bound))
                                                (name b ,(bound)))
                                          (list a-cant-be
                                                b-cant-be)))))
              (set (set (cons 'list 'list)
                        (cons `(name a ,(bound)) 'a-cant-be)
                        (cons `(name b ,(bound)) 'b-cant-be))))

(check-equal? (dqs->dqset `(,(dq '() `((list (name a ,(bound))
                                             (name b ,(bound)))
                                       (list a-cant-be
                                             b-cant-be)))
                            ,(dq '() `((list (name c ,(bound))
                                             (name d ,(bound)))
                                       (list c-cant-be
                                             d-cant-be)))))
              (set (set (cons 'list 'list)
                        (cons `(name c ,(bound)) 'c-cant-be)
                        (cons `(name d ,(bound)) 'd-cant-be))
                   (set (cons 'list 'list)
                        (cons `(name b ,(bound)) 'b-cant-be)
                        (cons `(name a ,(bound)) 'a-cant-be))))

(define (dqs-equal? dqs-left dqs-right)
  (equal? (dqs->dqset dqs-left)
          (dqs->dqset dqs-right)))

(define (p*e-equivalent? pe-left pe-right initial-eqs)
  (define (new-vars env)
    (set-subtract (for/set ([(k v) (in-hash (env-eqs env))]) k)
                  (for/set ([(k v) (in-hash initial-eqs)]) k)))
  (define all-left (all-resolutions pe-left))
  (define all-right (all-resolutions pe-right))
  (and (unless (for/and
                   ([l (for/list ([l all-left] 
                                  #:when (no-lvars-pat? l)) 
                         l)]
                    [r (for/list ([r all-right] 
                                  #:when (no-lvars-pat? r)) 
                         r)])
                 (no-lvars-pat-equal? l r))
           (not (set-empty? (set-intersect all-left all-right))))
       (equal? (new-vars (p*e-e pe-left))
               (new-vars (p*e-e pe-right)))))

(define (no-lvars-pat? p)
  (match p
    [`(list ,ps ...)
     (for/and ([p ps])
       (no-lvars-pat? p))]
    [`(cstr (,nts ...) ,p)
     (no-lvars-pat? p)]
    [`(name ,id ,(bound))
     #f]
    [else 
     #t]))

(define (no-lvars-pat-equal? l r)
  (match* (l r)
    [(`(list ,ls ...) `(list ,rs ...))
     (and (equal? (length ls) (length rs))
          (for/and ([l ls] [r rs])
            (no-lvars-pat-equal? l r)))]
    [(`(cstr (,l-nts ...) (nt ,l-nt)) `(cstr (,r-nts ...) (nt ,r-nt)))
     (equal? (for/set ([l `(,@l-nts ,l-nt)]) l)
             (for/set ([r `(,@r-nts ,r-nt)]) r))]
    [(`(cstr (,l-nts ...) ,l-p) `(cstr (,r-nts ...) ,r-p))
     (and (equal? (for/set ([l-nt `(,@l-nts)]) l-nt)
                  (for/set ([r-nt `(,@r-nts)]) r-nt))
          (no-lvars-pat-equal? l-p r-p))]
    [(_ _)
     (equal? l r)]))

(define (all-resolutions pe)
  (define p0 (p*e-p pe))
  (define env (p*e-e pe))
  (define eqs (env-eqs env))
  (let loop ([p p0]
             [n 0])
    (when (> n 100)
      (error 'all-resolutions "size bound exceeded: ~s" p))
    (match p
      [`(name ,id ,(bound))
       (define res (hash-ref eqs (lvar id)))
       (match res
         [(lvar id2)
          (set-union (set p)
                     (loop `(name ,id2 ,(bound)) (add1 n)))]
         [`(name ,id2 ,(bound))
          (set-union (set p)
                     (loop `(name ,id2 ,(bound)) (add1 n)))]
         [else
          (set p res)])]
      [`(list ,ps ...)
        (define p-lists
          (let loop2 ([pats ps])
              (match pats
                [(cons p pts)
                 (for*/set ([all-p (loop p (add1 n))]
                            [all-rest (loop2 pts)])
                       `(,all-p ,@all-rest))]
                ['() (set '())])))
        (for/set ([p-l (in-set p-lists)])
            (match p-l
              [`(,ps ...)
               `(list ,@ps)]))]
      [`(cstr (,nts ...) ,c-p)
       (for/set ([all-p (in-set (loop c-p (add1 n)))])
          `(cstr (,@nts) ,all-p))]
      [`(mismatch-name ,id ,m-p)
       (for/set ([all-p (in-set (loop m-p (add1 n)))])
          `(mismatch-name ,id ,all-p))]
      [else
       (set p)])))

(check-equal? (all-resolutions (p*e 'number (env (hash) '())))
              (set 'number))
(check-equal? (all-resolutions (p*e `(name a ,(bound)) 
                                    (env (hash (lvar 'a) 5) '())))
              (set 5 `(name a ,(bound))))
(check-equal? (all-resolutions (p*e `(name a ,(bound)) 
                                    (env (hash (lvar 'a) (lvar 'b)
                                               (lvar 'b) 7) '())))
              (set 7 `(name a ,(bound)) `(name b ,(bound))))
(check-equal? (all-resolutions (p*e `(list 1 2 3) (env (hash) '())))
              (set '(list 1 2 3)))
(check-equal? (all-resolutions (p*e `(list 1 (name q ,(bound)) 3)
                                    (env (hash (lvar 'q) 2) '())))
              (set '(list 1 2 3) `(list 1 (name q ,(bound)) 3)))
(check-equal? (all-resolutions (p*e `(list (name a ,(bound)) (name b ,(bound)))
                                    (env (hash (lvar 'a) 1 (lvar 'b) 2) '())))
              (set
               '(list 1 2)
               `(list 1 (name b ,(bound)))
               `(list (name a ,(bound)) 2)
               `(list (name a ,(bound)) (name b ,(bound)))))
(check-equal? (all-resolutions (p*e `(list (name a ,(bound)) (name b ,(bound)))
                                    (env (hash (lvar 'a) 1 
                                               (lvar 'b) `(name c ,(bound))
                                               (lvar 'c) 2) '())))
              (set
               '(list 1 2)
               `(list 1 (name b ,(bound)))
               `(list 1 (name c ,(bound)))
               `(list (name a ,(bound)) 2)
               `(list (name a ,(bound)) (name c ,(bound)))
               `(list (name a ,(bound)) (name b ,(bound)))))
(check-equal? (all-resolutions (p*e `(cstr (e) (nt q)) (env (hash) '())))
              (set '(cstr (e) (nt q))))

;; test unify but only look at the eqs (half the env)
(define (unify/format p1 p2 eqs L)
  (define res-pe (unify/lt p1 p2 (env eqs '()) L))
  (define res-pe-bkwd (unify/lt p2 p1 (env eqs '()) L))
  (cond
    [(and (p*e? res-pe)
          (p*e? res-pe-bkwd)
          (p*e-equivalent? res-pe res-pe-bkwd eqs))
     (p*e (p*e-p res-pe) (env-eqs (p*e-e res-pe)))]
    [(and (unif-fail? res-pe)
          (unif-fail? res-pe-bkwd))
     #f]
    [else 
     (list 'different-orders=>different-results
           res-pe
           res-pe-bkwd)]))


(define (unify*/format p1 p2 eqs L)
  (define e (env eqs '()))
  (define res (unify*/lt p1 p2 e L))
  (define res-bkwd (unify*/lt p2 p1 e L))
  (cond
    [(and (not-failed? res)
          (not-failed? res-bkwd)
          (p*e-equivalent? res res-bkwd eqs))
     res]
    [(and (unif-fail? res)
          (unif-fail? res-bkwd))
     #f]
    [else 
     (list 'different-orders=>different-results
           res
           res-bkwd)]))

;; This looks really strange but it is for backwards compatability
;; with tests that didn't take nonterminal productions into account.
;; There nts will all (except n) accept anything, but since they
;; have two productions they don't get collapsed (and so will create
;; cstr's).
(define-language L0
  (e any any)
  (q any any)
  (v any any)
  (b any any)
  (x any any)
  (Γ any any)
  (s any any)
  (a any any)
  (Q any any)
  (n number any))


(check-equal? (unify/format `number `number (hash) L0)
              (p*e `number (hash)))
(check-equal? (unify/format `number `variable (hash) L0)
              #f)
(check-equal? (unify/format `number `integer (hash) L0)
              (p*e `integer (hash)))
(check-equal? (unify/format `(list integer natural) `(list natural integer) (hash) L0)
              (p*e `(list natural natural) (hash)))
(check-equal? (unify/format `(list integer natural) `(list natural variable) (hash) L0)
              #f)
(check-equal? (unify/format `variable `variable-not-otherwise-mentioned (hash) L0)
              (p*e `variable-not-otherwise-mentioned (hash)))
(check-equal? (unify/format `(nt e) `(list (nt e) variable) (hash) L0)
              (p*e `(cstr (e) (list (nt e) variable)) (hash)))
(check-equal? (unify/format `(list (nt e) variable) `(nt e) (hash) L0)
              (p*e `(cstr (e) (list (nt e) variable)) (hash)))
(check-equal? (unify/format `(nt e) `string (hash) L0)
              (p*e `(cstr (e) string) (hash)))
(check-equal? (unify/format `string `(nt e) (hash) L0)
              (p*e `(cstr (e) string) (hash)))
(check-equal? (unify/format `number `(nt e) (hash) L0)
              (p*e `(cstr (e) number) (hash)))


;; tests specific to #f
;; (which don't work in the above format)
(check-equal? (unify #f '(name x any) (env (hash) '()) #f)
              (p*e `(name x ,(bound))
                   (env (hash (lvar 'x) #f) '())))
(check-equal? (unify #t '(name x any) (env (hash) '()) #f)
              (p*e `(name x ,(bound))
                   (env (hash (lvar 'x) #t) '())))
(check-equal? (unify* #t 'any (env (hash) '()) #f)
              (p*e #t (env (hash) '())))
(check-equal? (unify* #f 'any (env (hash) '()) #f)
              (p*e #f (env (hash) '())))
(check-equal? (unify* #t 'any (env (hash) '()) #f)
              (p*e #t (env (hash) '())))
(check-equal? (unify* #f 'number (env (hash) '()) #f)
              (unif-fail))
(check-equal? (unify* '(list 1) 1 (env (hash) '()) #f)
              (unif-fail))
(check-equal? (unify* 'boolean #t (env (hash) '()) #f)
              (p*e #t (env (hash) '())))
(check-equal? (unify* 'boolean #f (env (hash) '()) #f)
              (p*e #f (env (hash) '())))
(check-equal? (unify* 'number #f (env (hash) '()) #f)
              (unif-fail))
(check-equal? (unify* 'integer #f (env (hash) '()) #f)
              (unif-fail))
(check-equal? (unify* 'natural #f (env (hash) '()) #f)
              (unif-fail))
(check-equal? (unify* 'real #f (env (hash) '()) #f)
              (unif-fail))
(check-equal? (unify* 'string #f (env (hash) '()) #f)
              (unif-fail))
(check-equal? (unify* 'variable #f (env (hash) '()) #f)
              (unif-fail))
(check-equal? (unify* 'variable-not-otherwise-mentioned #f (env (hash) '()) #f)
              (unif-fail))

(define (p-eqs-equal? a b)
  (check-equal?
   (cons (p*e-p a)
         (p*e-eqs a))
   (cons (p*e-p b)
         (p*e-eqs b))))

(define-syntax (unify-all/results stx)
  (syntax-case stx ()
    [(_ lhs eqs ([rhs res res-eqs] rest ...))
     (not (syntax->datum #'res))
     #'(begin
         (check-equal? (unify/format lhs rhs eqs L0)
                       #f) 
         (unify-all/results lhs eqs (rest ...)))]
    [(_ lhs eqs ([rhs res res-eqs] rest ...))
     #'(begin
         (check-equal? (unify/format lhs rhs eqs L0)
                       (p*e res res-eqs))
         (unify-all/results lhs eqs (rest ...)))]
    [(_ lhs eqs ([rhs res] rest ...))
     #'(begin
         (p-eqs-equal? (unify/format lhs rhs eqs L0)
                       res) 
         (unify-all/results lhs eqs (rest ...)))]
    [(_ lhs eqs ())
     #'(void)]))

(define-syntax (unify-all/results/no-bindings stx)
  (syntax-case stx ()
    [(_ lhs eqs ([rhs res] ...))
     #'(unify-all/results lhs eqs
                          ([rhs res eqs] ...))]))

(unify-all/results/no-bindings
 'any (hash)
 (['any 'any]
  ['integer 'integer]
  ['natural 'natural]
  ['number 'number]
  ['real 'real]
  ['string 'string]
  ['boolean 'boolean]
  ['variable 'variable]
  ['variable-not-otherwise-mentioned 'variable-not-otherwise-mentioned]
  [7 7]
  ["a string" "a string"]
  ['(nt e) '(cstr (e) any)]
  ['(list 1 2 3) '(list 1 2 3)]
  ['(mismatch-name x any) 'any]))

(unify-all/results/no-bindings
 'number (hash)
 ([7 7]
  [-7 -7]
  [7.5 7.5]
  ["a string" #f]
  ['string #f]
  ['boolean #f]
  ['variable #f]
  ['variable-not-otherwise-mentioned #f]
  ['(list 1 2 3) #f]
  [`(nt e) `(cstr (e) number)]))

(unify-all/results/no-bindings
 'integer (hash)
 ([7 7]
  [-7 -7]
  [7.5 #f]
  ["a string" #f]
  ['string #f]
  ['boolean #f]
  ['variable #f]
  ['variable-not-otherwise-mentioned #f]
  ['(list 1 2 3) #f]
  [`(nt e) `(cstr (e) integer)]))

(unify-all/results/no-bindings
 'natural (hash)
 ([7 7]
  [-7 #f]
  [7.5 #f]
  ["a string" #f]
  ['string #f]
  ['boolean #f]
  ['variable #f]
  ['variable-not-otherwise-mentioned #f]
  ['(list 1 2 3) #f]
  [`(nt e) `(cstr (e) natural)]))

(unify-all/results/no-bindings
 'real (hash)
 ([7 7]
  [-7 -7]
  [7.5 7.5]
  ["a string" #f]
  ['string #f]
  ['boolean #f]
  ['variable #f]
  ['variable-not-otherwise-mentioned #f]
  ['(list 1 2 3) #f]
  [`(nt e) `(cstr (e) real)]))

(unify-all/results/no-bindings
 'string (hash)
 (['string 'string]
  ['variable #f]
  ['variable-not-otherwise-mentioned #f]
  ['boolean #f]
  [7 #f]
  ["a string" "a string"]
  ['(list a b c) #f]
  ['(nt e) '(cstr (e) string)]))

(unify-all/results/no-bindings
 'variable (hash)
 (['variable 'variable]
  ['x 'x]
  ['boolean #f]
  ["a string" #f]
  ['(nt e) '(cstr (e) variable)]
  ['(list 1 2 3) #f]
  [5 #f]))

(unify-all/results/no-bindings
 'variable-not-otherwise-mentioned (hash)
 (['variable-not-otherwise-mentioned 'variable-not-otherwise-mentioned]
  ['x 'x]
  ['boolean #f]
  ["a string" #f]
  ['(nt e) '(cstr (e) variable-not-otherwise-mentioned)]
  ['(list 1 2 3) #f]
  [5 #f]))

(unify-all/results/no-bindings
 '(nt e) (hash)
 ([5 '(cstr (e) 5)]
  ["a string" '(cstr (e) "a string")]
  ['boolean '(cstr (e) boolean)]
  ['(nt q) '(cstr (e) (nt q))]))

(unify-all/results/no-bindings
 'boolean (hash)
 (['boolean 'boolean]
  [`(list 1 2 3) #f]
  ["abc" #f]))

(unify-all/results
 '(name x any) (hash)
 ([5 `(name x ,(bound)) (hash (lvar 'x) 5)]
  [`(list 1 2 3) `(name x ,(bound)) (hash (lvar 'x) `(list 1 2 3))]
  ['boolean `(name x ,(bound)) (hash (lvar 'x) `boolean)]
  ["a string" `(name x ,(bound)) (hash (lvar 'x) "a string")]))

(unify-all/results/no-bindings
 '(mismatch-name x any) (hash)
 ([5 5]
  ["a string" "a string"]
  ['any 'any]
  ['number 'number]
  ['integer 'integer]
  ['natural 'natural]
  ['real 'real]
  ['string 'string]
  ['boolean 'boolean]
  ['(list 1 2 3) '(list 1 2 3)]
  ['(mismatch-name y 5) 5]
  ['(nt e) '(cstr (e) any)]
  ['variable 'variable]
  ['variable-not-otherwise-mentioned 'variable-not-otherwise-mentioned]))

(unify-all/results/no-bindings
 '(variable-except a) (hash)
 ([5 #f]
  ['any '(variable-except a)]
  ['number #f]
  ['integer #f]
  ['natural #f]
  ['real #f]
  ['string #f]
  ['boolean #f]
  ['(list 1 2 3) #f]
  ['b 'b]
  ['a #f]
  ['(mismatch-name y b) 'b]
  ['(nt e) '(cstr (e) (variable-except a))]
  ['variable '(variable-except a)]
  ['variable-not-otherwise-mentioned '(variable-except a)]
  ['(variable-except b) '(variable-except a b)]
  ['(variable-prefix a) #f]
  ['(variable-prefix b) '(variable-prefix b)]))

(unify-all/results/no-bindings
 '(variable-prefix a) (hash)
 ([5 #f]
  ['any '(variable-prefix a)]
  ['number #f]
  ['integer #f]
  ['natural #f]
  ['real #f]
  ['string #f]
  ['boolean #f]
  ['(list 1 2 3) #f]
  ['b #f]
  ['a 'a]
  ['aa 'aa]
  ['(mismatch-name y b) #f]
  ['(nt e) '(cstr (e) (variable-prefix a))]
  ['variable '(variable-prefix a)]
  ['variable-not-otherwise-mentioned '(variable-prefix a)]
  ['(variable-except b bb c) '(variable-prefix a)]
  ['(variable-except b ab c) #f]
  ['(variable-prefix a) '(variable-prefix a)]
  ['(variable-prefix b) #f]))
                  

(define-syntax-rule
  (check-pat-equal? a b)
  (check-equal? (let ([ans a])
                  (match ans
                    [(p*e p _) p]
                    [_ ans]))
                b))
  
(check-pat-equal? (unify*/lt `(nt v) `(cstr (e) (list (nt e) (nt v))) empty-env L0)
              `(cstr (e v) (list (nt e) (nt v))))
(check-pat-equal? (unify*/lt `(cstr (e) (list (nt e) (nt v))) `(nt v) empty-env L0)
              `(cstr (e v) (list (nt e) (nt v))))
(check-pat-equal? (unify*/lt `(cstr (e) (list (nt e) (nt v))) 5 empty-env L0)
              (unif-fail))
(check-pat-equal? (unify*/lt `(cstr (e) (list (nt e) (nt v))) `(list (nt e) (nt v)) empty-env L0)
              `(cstr (e) (list (nt e) (nt v))))
(check-pat-equal? (unify*/lt `(cstr (e) number) `(cstr (v) natural) empty-env L0)
              `(cstr (e v) natural))
(check-pat-equal? (unify*/lt `(cstr (e) (list number variable)) `(cstr (e) number) empty-env L0)
              (unif-fail))
(check-pat-equal? (unify*/lt `(cstr (e) (list number variable-not-otherwise-mentioned)) 
                         `(cstr (e) (list integer variable)) empty-env L0)
              `(cstr (e) (list integer variable-not-otherwise-mentioned)))


(define-syntax (unify*-all/results stx)
  (syntax-case stx ()
    [(_ lhs eqs ([rhs res res-eqs] ...))
     #'(begin
         (let ([pe (unify*/format lhs rhs eqs L0)])
           (check-equal? (p*e-p pe) res)
           (check-equal? (env-eqs (p*e-e pe)) res-eqs)) ...)]))

(unify*-all/results
 `(cstr (e) (nt q)) (hash)
 (['any `(cstr (e q) any) (hash)]
  ['integer `(cstr (e q) integer) (hash)]
  ['natural `(cstr (e q) natural) (hash)]
  ['number `(cstr (e q) number) (hash)]
  ['real `(cstr (e q) real) (hash)]
  ['string `(cstr (e q) string) (hash)]
  [`boolean `(cstr (e q) boolean) (hash)]
  ['variable `(cstr (e q) variable) (hash)]
  ['variable-not-otherwise-mentioned 
   `(cstr (e q) variable-not-otherwise-mentioned) (hash)]
  ['(variable-except a)
   `(cstr (e q) (variable-except a)) (hash)]
  ['(variable-prefix a) 
   `(cstr (e q) (variable-prefix a)) (hash)]
  ['(list 1 2 3)
   '(cstr (e q) (list 1 2 3)) (hash)]
  [5 '(cstr (e q) 5) (hash)]
  ["a string" '(cstr (e q) "a string") (hash)]
  ['(mismatch-name x 5) '(cstr (e q) 5) (hash)]))

(unify*-all/results
 `(name x ,(bound)) (hash (lvar 'x) 'any)
 (['any `(name x ,(bound)) (hash (lvar 'x) 'any)]
  [5 `(name x ,(bound)) (hash (lvar 'x) 5)]
  ['number `(name x ,(bound)) (hash (lvar 'x) 'number)]
  ['integer `(name x ,(bound)) (hash (lvar 'x) 'integer)]
  ['natural `(name x ,(bound)) (hash (lvar 'x) 'natural)]
  ['real `(name x ,(bound)) (hash (lvar 'x) 'real)]
  ['string `(name x ,(bound)) (hash (lvar 'x) 'string)]
  ['boolean `(name x ,(bound)) (hash (lvar 'x) 'boolean)]
  ['variable `(name x ,(bound)) (hash (lvar 'x) 'variable)]
  ['variable-not-otherwise-mentioned `(name x ,(bound)) (hash (lvar 'x) 'variable-not-otherwise-mentioned)]
  ['(variable-except a) `(name x ,(bound)) (hash (lvar 'x) '(variable-except a))]
  ['(variable-prefix a) `(name x ,(bound)) (hash (lvar 'x) '(variable-prefix a))]
  ['(cstr (n) any) `(name x ,(bound)) (hash (lvar 'x) '(cstr (n) any))]
  ['(list 1 2) `(name x ,(bound)) (hash (lvar 'x) '(list 1 2))]
  ['(mismatch-name z any) `(name x ,(bound)) (hash (lvar 'x) 'any)]
  ['(nt q) `(name x ,(bound)) (hash (lvar 'x) '(cstr (q) any))]))

(unify*-all/results
 `(name x ,(bound)) (hash (lvar 'x) 'any (lvar 'y) 'variable)
 ([`(name x ,(bound)) `(name x ,(bound)) (hash (lvar 'x) 'any (lvar 'y) 'variable)]
  [`(name y ,(bound)) `(name x ,(bound)) (hash (lvar 'y) (lvar 'x) (lvar 'x) 'variable)]))


(let ()
  
  (define-language ntl
    (n number)
    ((x y) variable)
    (Q any any))
  
  (check-equal? (unify/format `(nt n) `any (hash) ntl)
                (p*e `number (hash)))
  (check-equal? (unify/format `(nt n) 5 (hash) ntl)
                (p*e 5 (hash)))
  (check-equal? (unify/format `(nt Q) `any (hash) ntl)
                (p*e `(cstr (Q) any) (hash)))
  (check-equal? (unify/format `(nt x) 'number (hash) ntl)
                #f)
  (check-equal? (unify/format `(nt x) 'a-var (hash) ntl)
                (p*e 'a-var (hash)))
  (check-equal? (unify/format `(nt x) 'any (hash) ntl)
                (p*e 'variable (hash)))
  (check-equal? (unify/format `(nt y) 'any (hash) ntl)
                (p*e 'variable (hash)))
  ;; asymmetry ok here - if the nt doesn't get unified against,
  ;; it doesn't get collapsed
  (check-equal? (unify/format `(nt y) '(nt Q) (hash) ntl)
                `(different-orders=>different-results 
                 ,(p*e '(cstr (Q) variable) (env (hash) '()))
                 ,(p*e '(cstr (Q) (nt y)) (env (hash) '()))))
  )


;; numeric
(check-equal? (unify/format `number `number (hash) L0)
              (p*e `number (hash)))
(check-equal? (unify/format `number `real (hash) L0)
              (p*e `real (hash)))
(check-equal? (unify/format `number `integer (hash) L0)
              (p*e `integer (hash)))
(check-equal? (unify/format `number `natural (hash) L0)
              (p*e `natural (hash)))
(check-equal? (unify/format `real `real (hash) L0)
              (p*e `real (hash)))
(check-equal? (unify/format `real `integer (hash) L0)
              (p*e `integer (hash)))
(check-equal? (unify/format `real `natural (hash) L0)
              (p*e `natural (hash)))
(check-equal? (unify/format `integer `integer (hash) L0)
              (p*e `integer (hash)))
(check-equal? (unify/format `integer `natural (hash) L0)
              (p*e `natural (hash)))
(check-equal? (unify/format `natural `natural (hash) L0)
              (p*e `natural (hash)))


;; bindings
(check-equal? (unify/format `(name e_1 number) `number (hash) L0)
              (p*e `(name e_1 ,(bound)) (m-hash (lvar `e_1) `number)))
(check-equal? (unify/format `number `(name e_1 number) (hash) L0)
              (p*e `(name e_1 ,(bound)) (m-hash (lvar `e_1) `number)))
(check-equal? (unify/format `(name e_1 number) `(name e_2 integer) (hash) L0)
              (p*e `(name e_1 ,(bound)) (m-hash (lvar `e_2) (lvar `e_1) (lvar `e_1) `integer)))
(check-equal? (unify/format `(name e_2 integer) `(name e_1 number) (hash) L0)
              (p*e `(name e_2 ,(bound)) (m-hash (lvar `e_1) (lvar `e_2) (lvar `e_2) `integer)))
(check-equal? (unify/format `(name e_1 number) `(name e_2 integer) (m-hash (lvar 'e_2) `integer) L0)
              (p*e `(name e_1 ,(bound)) (m-hash (lvar `e_1) `integer (lvar `e_2) (lvar 'e_1))))
(check-equal? (unify/format `(name e_2 integer) `(name e_1 number) (m-hash (lvar 'e_2) `integer) L0)
              (p*e `(name e_2 ,(bound)) (m-hash (lvar `e_1) (lvar `e_2) (lvar `e_2) `integer)))
(check-equal? (unify/format `(name e_2 number) `(name e_1 number) 
                            (m-hash (lvar 'e_2) `integer (lvar 'e_1) `number) L0)
              (p*e `(name e_2 ,(bound)) (m-hash (lvar `e_1) (lvar `e_2) (lvar `e_2) `integer)))
(check-equal? (unify/format `(name e1 (nt e)) `(name e2 5) 
                            (m-hash (lvar 'e1) `(nt e)
                                    (lvar 'e2) 5)
                            L0)
              (p*e `(name e1 ,(bound))
                   (m-hash (lvar 'e2) (lvar 'e1)
                           (lvar 'e1)  `(cstr (e) 5))))
(check-equal? (unify/format `(name e1 (nt e)) `(name e2 (nt e)) 
                            (m-hash (lvar 'e1) `(nt e)
                                    (lvar 'e2) (lvar 'e1))
                            L0)
              (p*e `(name e1 ,(bound))
                   (m-hash (lvar 'e2) (lvar 'e1)
                           (lvar 'e1) `(nt e))))

;; occurs check
(check-false (unify/format `(name e_1 (nt e)) 
                           `(list 5 (list 6 (name e_1 (nt e))) 7)
                           (hash)
                           L0))
(check-equal? (unify/format `(name e_2 5)
                            `(nt a)
                            (m-hash (lvar 'e_2) (lvar 'e_1)
                                    (lvar 'e_1) `(cstr (b) 5))
                            L0)
              (p*e `(name e_1 ,(bound))
                   (m-hash (lvar 'e_2) (lvar 'e_1)
                           (lvar 'e_1) `(cstr (a b) 5))))
;; named vars in constraints don't get lost (their pats get added to the env)
(check-equal? (unify/format `(nt e) `(list a b (name this (nt b)) c)
                            (hash) L0)
              (p*e `(cstr (e) (list a b (name this ,(bound)) c))
                   (m-hash (lvar 'this) `(nt b))))
(check-equal? (unify/format `(nt e) `(list a b (name this (nt b)) c)
                            (m-hash (lvar 'this) 6) L0)
              (p*e `(cstr (e) (list a b (name this ,(bound)) c))
                   (m-hash (lvar 'this) `(cstr (b) 6))))
;; resolve maintains variable dependency on update
;; TODO: is there a more minimal test for this (change id in resolve to name-t to reproduce)
(check-equal? (unify/format `(list (list (name x7 (nt x)) (name t_1 (nt t))) (name Γ1 (nt Γ)))
                            `(name Γ2 (nt Γ))
                            (m-hash (lvar 'x7) (lvar 'x1)
                                    (lvar 'x1) `(nt x))
                            L0)
              (p*e '(name Γ2 #s(bound))
                   (hash (lvar 'x1) '(nt x)
                         (lvar 't_1) '(nt t)
                         (lvar 'Γ1) '(nt Γ)
                         (lvar 'x7) (lvar 'x1)
                         (lvar 'Γ2)
                          `(cstr
                            (Γ)
                            (list
                             (list (name x1 ,(bound)) (name t_1 ,(bound)))
                             (name Γ1 ,(bound)))))))
(check-false (unify/format `(name x (list x x))
                           `(name x (list x))
                           (m-hash (lvar 'x)
                                   `(list x x))
                           L0))
(check-false (unify/format `(name x (list x))
                           `(name x (list x))
                           (m-hash (lvar 'x)
                                   `(list x x))
                           L0))
;; sometimes things aren't path compressed apparently...
;; TODO, figure out why
#;(check-not-false (unify/lt `(name x1 ,(bound)) `(nt x)
                             (m-hash (lvar 'x1) (lvar 'x2)
                                     (lvar 'x2) `(list x x))
                             L0))
(check-equal? (unify/format '(list (name n_2 (nt n)) (name n_3 (nt n)))
                            '(list (name n_1 (nt n)) (name n_1 (nt n)))
                            (hash)
                            L0)
              (p*e `(list (name n_2 ,(bound)) (name n_3 ,(bound))) 
                   (m-hash (lvar 'n_3) '(nt n) 
                           (lvar 'n_2) (lvar 'n_3)
                           (lvar 'n_1) (lvar 'n_2))))


(check-not-false (bind-names 'any empty-env L0))
(check-equal? (bind-names `(name x any) empty-env L0)
              (p*e `(name x ,(bound))
                   (env (hash (lvar 'x) 'any) '())))
(check-equal? (let ([h (hash (lvar 'x) (lvar 'y)
                             (lvar 'y) 'any)])
                (bind-names `(list (name x 5) (name y 6)) (env h '()) L0))
              (unif-fail))

(define-syntax do-unify
  (λ (stx)
    (syntax-case stx ()
      [(f-name lang t-1 t-2 env)
       (with-syntax ([(ignore-1 pat-1 (names-1 ...) (names/ellipses-1 ...))
                      (rewrite-side-conditions/check-errs
                       (language-id-nts #'lang 'f-name) 'f-name stx #'t-1)]
                     [(ignore-2 pat-2 (names-2 ...) (names/ellipses-2 ...))
                      (rewrite-side-conditions/check-errs
                       (language-id-nts #'lang 'f-name) 'f-name stx #'t-2)])
         #'(unify/format 'pat-1 'pat-2 env lang))])))

(define-syntax u-test
  (λ (stx)
    (syntax-case stx ()
      [(_ lang t-1 t-2 env res)
       (with-syntax ([u-stx #'(do-unify lang t-1 t-2 env)])
         #'(check-equal? u-stx res))])))

(define-syntax u-fails
  (λ (stx)
    (syntax-case stx ()
      [(_ lang t-1 t-2 env)
       (with-syntax ([u-stx #'(do-unify lang t-1 t-2 env)])
         #'(check-false u-stx))])))

(define-syntax u-succeeds
  (λ (stx)
    (syntax-case stx ()
      [(_ lang t-1 t-2 env)
       (with-syntax ([u-stx #'(do-unify lang t-1 t-2 env)])
         #'(check-not-false u-stx))])))


(define-language U-nums
  (n O
     (S n)))

(define (m-env . eqs)
  (env (apply hash eqs) '()))

(check-pat-equal? (unify*/lt '(cstr (n) (list S O)) '(list S O) empty-env U-nums)
                '(cstr (n) (list S O)))
(u-succeeds U-nums (S O) (S O) (hash))
(u-succeeds U-nums (S (S O)) (S n) (hash))
(u-fails U-nums (S (S O)) (S n) (m-hash (lvar 'n) '(list O)))
(u-succeeds U-nums (S (S O)) (S n) (m-hash (lvar 'n) '(list S O)))
(u-fails U-nums (S O) (S n) (m-hash (lvar 'n) '(list S O)))
(u-fails U-nums (S (S O)) O (hash))
(u-test U-nums O O (hash) (p*e 'O (hash)))
(u-test U-nums (S (S O)) (S (S O)) 
        (hash) (p*e '(list S (list S O)) (hash)))
(u-fails U-nums O (S O) (hash))
(u-fails U-nums (S O) O (hash))
(u-test U-nums O n 
        (hash) (p*e `(name n ,(bound)) (m-hash (lvar 'n) '(cstr (n) O))))
(u-test U-nums n O
        (hash) (p*e `(name n ,(bound)) (m-hash (lvar 'n) '(cstr (n) O))))
(u-test U-nums (S n) (S (S O)) 
        (hash) (p*e `(list S (name n ,(bound))) (m-hash (lvar 'n) '(cstr (n) (list S O)))))
;; occurs check
(u-fails U-nums (S n) (S (S n)) (hash))
(u-test U-nums (S n_1) (S (S n_2)) 
        (hash) 
        (p*e `(list S (name n_1 ,(bound)))
             (m-hash (lvar 'n_1) `(cstr (n) (list S (name n_2 ,(bound))))
                     (lvar 'n_2) `(nt n))))
(u-fails U-nums (S n) (S O) (m-hash (lvar 'n) `(list S (name n_2 ,(bound)))
                                    (lvar 'n_2) '(nt n)))
(u-test U-nums (S n) (S O) (m-hash (lvar 'n) 'O) 
        (p*e `(list S (name n ,(bound)))
             (m-hash (lvar 'n) '(cstr (n) O))))
#|
(u-test (S n) (S O) (m-hash '(name n (nt n)) '(name n_1 (nt n)) '(name n_1 (nt n)) 'O)
        U-nums (m-hash '(name n (nt n)) 'O '(name n_1 (nt n)) 'O))
(u-test (S n) (S O) (m-hash '(name n (nt n)) '(name n_1 (nt n)) '(name n_1 (nt n)) '(S O))
        U-nums #f)
|#


;; λn tests

(define-language λn
  (e (e e)
     (λ x e)
     (if0 e e e)
     number
     x)
  (x variable-not-otherwise-mentioned))

(u-fails λn 1 2 (hash))
(u-test λn (λ x 3) e_1 (hash) 
        (p*e `(name e_1 ,(bound)) 
             (m-hash (lvar 'e_1) `(cstr (e) (list λ (name x ,(bound)) 3))
                     (lvar 'x) `(nt x))))
(u-test λn (λ y 3) (λ y e_2) (hash)
        (p*e `(list λ y (name e_2 ,(bound))) (m-hash (lvar 'e_2) `(cstr (e) 3))))
(u-test λn (λ x 3) (λ x e_2) (hash) 
        (p*e `(list λ (name x ,(bound)) (name e_2 ,(bound)))
             (m-hash (lvar 'e_2) `(cstr (e) 3) (lvar 'x) `(nt x))))
(u-fails λn (λ x 3) (e_1 e_2) (hash))
(u-test λn (e_1 e_2) ((λ x x) 3) (hash) 
        (p*e `(list (name e_1 ,(bound)) (name e_2 ,(bound)))
             (m-hash (lvar 'e_2)
                     `(cstr (e) 3)
                     (lvar 'e_1)
                     `(cstr (e) (list λ (name x ,(bound)) (name x ,(bound))))
                     (lvar 'x)
                     `(nt x))))

(define-language p-types
  (t (t -> t)
     (∀ x t)
     x)
  (x variable-not-otherwise-mentioned))

(define-language n-lang
  (n number))

(check-equal?
 (pat->term n-lang 'hi (env (hash (lvar 'it) '(cstr (n) hi)) '()))
 (unif-fail))


(check-not-false (redex-match λn e_1 (pat->term λn `(nt e) (env (hash) '()))))
(check-not-false
 (redex-match λn (e_1 e_2) (pat->term λn `(list (name e1 ,(bound))
                                                (name e2 ,(bound)))
                                      (env (m-hash (lvar 'e1) `(nt e)
                                                   (lvar 'e2) `(nt e)) '()))))
(check-not-false 
 (redex-match λn (e_1 e_1) (pat->term λn `(list (name e1 ,(bound)) (name e1 ,(bound)))
                                      (env (m-hash (lvar 'e1)
                                                   `(list (name e2 ,(bound)) (name e3 ,(bound)))
                                                   (lvar 'e2)
                                                   `(list (nt e) (name e3 ,(bound)))
                                                   (lvar 'e3)
                                                   `(nt e)) '()))))
(check-not-false
 (redex-match λn (e_1 e_1) (pat->term λn `(list (name e1 ,(bound))
                                                (name e1 ,(bound)))
                                      (env (m-hash (lvar 'e1) `(nt e)
                                                   (lvar 'e2) `(nt e)) '()))))
(check-not-false
 (redex-match λn (e_1 e_2) (pat->term λn `(list (name e1 ,(bound))
                                                (name e2 ,(bound)))
                                      (env (m-hash (lvar 'e1) `(cstr (e) (nt e))
                                                   (lvar 'e2) `(nt e)) '()))))
(check-not-false 
 (redex-match λn (e_1 e_2) (pat->term λn `(cstr (e) (list (name e1 ,(bound))
                                                          (name e2 ,(bound))))
                                      (env (m-hash (lvar 'e1) `(cstr (e) (nt e))
                                                   (lvar 'e2) `(nt e)) '()))))
(check-equal? (pat->term λn `(cstr (x) (list (nt e) (nt e))) (env (hash) '()))
              (unif-fail))
(check-not-false (pat->term λn `(cstr (e) (list (nt e) (nt e))) (env (hash) '())))
(check-equal? (pat->term λn `(cstr (x) (list (name e1 ,(bound))
                                             (name e2 ,(bound))))
                         (env (m-hash (lvar 'e1) `(cstr (e) (nt e))
                                      (lvar 'e2) `(nt e)) '()))
              (unif-fail))
(check-not-false
 (redex-match λn (e_1 e_1) (pat->term λn `(list (name x1 ,(bound))
                                                (name x2 ,(bound)))
                                      (env (m-hash (lvar 'x1) (lvar 'x2)
                                                   (lvar 'x2) `(nt e)) '()))))



;; result contains un unsatisfiable constraint:
;; (cstr (x) ->)
(check-false (do-unify p-types (t_1 -> t_2) (∀ x_3 t_3) (hash)))


;; disunification/unification with disequality tests
;; TODO tests on the dqs here are currently order-dependent

(check-false
 (disunify* '()
            `(list (name x_1 ,(bound))) 
            `(list (name x_2 ,(bound))) 
            (hash (lvar 'x_1) 'a
                   (lvar 'x_2) 'a)
            L0))
(check-not-false
 (disunify* '()
            `(list (name x_1 ,(bound))) 
            `(list (name x_2 ,(bound))) 
            (hash (lvar 'x_1) '(nt x)
                   (lvar 'x_2) 'a)
            L0))
(check-not-false
 (disunify* '()
            `(list (name x_1 ,(bound))) 
            `(list (name x_2 ,(bound))) 
            (hash (lvar 'x_1) '(nt x)
                   (lvar 'x_2) '(nt x))
            L0))
(check-false 
 (disunify* '() 'a '(cstr (s) a) (hash) L0))
(check-false
 (disunify* '() `(name a2 ,(bound)) '(cstr (s) a) (hash (lvar 'a2) 'a) L0))
(check-false
 (disunify* '() `(name a2 ,(bound)) `(name s6 ,(bound))
            (hash (lvar 'a2) 'a
                   (lvar 's6) '(cstr (s) a))
            L0))

(define (make-eqs eqs)
  (for/hash ([eq eqs])
    (values (car eq) (cdr eq))))

(define (make-dqs dqs)
  (for/list ([the-dq dqs])
    (dq '() (list (car the-dq) (cdr the-dq)))))

(define-syntax (test-disunify/no-params stx)
  (syntax-case stx ()
    [(_ t u eqs dqs eqs′ dqs′)
     (quasisyntax/loc stx
       (let* ([eqs-in (make-eqs eqs)]
              [eqs-out (make-eqs eqs′)]
              [dqs-in (check-and-resimplify eqs-in (make-dqs dqs) L0)]
              [dqs-out (make-dqs dqs′)]
              [res (disunify '() t u (env eqs-in dqs-in) L0)])
         #,(syntax/loc stx
             (check-not-false
              (env-equal?
               res
               (env eqs-out dqs-out))))))]
    [(_ t u eqs dqs true/false)
     (quasisyntax/loc stx
       (let* ([eqs-in (make-eqs eqs)]
              [dqs-in (check-and-resimplify eqs (make-dqs dqs) L0)]
              [res (disunify '() t u (env eqs-in dqs-in) L0)])
         (if true/false
             #,(syntax/loc stx (check-not-false res))
             #,(syntax/loc stx (check-false res)))))]))

(define-syntax (test-unify stx)
  (syntax-case stx ()
    [(_ t u v eqs dqs eqs′ dqs′)
     (quasisyntax/loc stx
       (let* ([eqs-in (make-eqs eqs)]
              [eqs-out (make-eqs eqs′)]
              [dqs-in (make-dqs dqs)]
              [dqs-out (make-dqs dqs′)]
              [res (unify/lt t u (env eqs-in dqs-in) L0)])
         (check-not-false
          (p*e-equal?
           res
           (p*e v (env eqs-out dqs-out))))))]
    [(_ t u eqs dqs true/false)
     (quasisyntax/loc stx
       (let* ([eqs-in (make-eqs eqs)]
              [dqs-in (make-dqs dqs)]
              [res (unify/lt t u (env eqs-in dqs-in) L0)])
         (if true/false
             (check-not-false (not-failed? res))
             (check-equal? res (unif-fail)))))]))

(test-disunify/no-params 1 2 '() '() '() '())
(test-disunify/no-params '(list 1 2) '(list 1 3) '() '() '() '())
(test-disunify/no-params `(list (name x any) (name y any)) `(list 1)
                 `((,(lvar 'x) . any)
                   (,(lvar 'y) . any))
                 '() 
                 `((,(lvar 'x) . any)
                   (,(lvar 'y) . any))
                 '())
(test-disunify/no-params `(name x any) 4
               `((,(lvar 'x) . ,(lvar 'y))
                 (,(lvar 'y) . 3))
               '()
               `((,(lvar 'x) . ,(lvar 'y))
                 (,(lvar 'y) . 3))
               '())
(test-disunify/no-params `(name x any) 1 
               `((,(lvar 'x) . 1)) '() #f)
(test-disunify/no-params 1 `(name x any)
               '() '()  `((,(lvar 'x) . any))
               `(((list (name x ,(bound))) . (list 1))))
(test-disunify/no-params `(name x any) 3
               `((,(lvar 'x) . ,(lvar 'y))
                 (,(lvar 'y) . 3))
               '() #f)
;; does violate the occurs check (stolen from Colmerauer '84)
(test-disunify/no-params 1 `(name z any)
               `((,(lvar 'x) . ,(lvar 'y))
                 (,(lvar 'y) . (list (name y ,(bound)) (name z ,(bound))))
                 (,(lvar 'z) . any))
               `(((list (name y ,(bound)) (name z ,(bound))) . (list 2 2)))
               `((,(lvar 'x) . ,(lvar 'y))
                 (,(lvar 'y) . (list (name y ,(bound)) (name z ,(bound))))
                 (,(lvar 'z) . any))
               `(((list (name z ,(bound))) . (list 1))))
(test-unify 1 `(name x any)
            '()
            `(((list (name x ,(bound))) . (list 1)))
            #f)
(test-unify `(name x any) 3
            `((,(lvar 'x) . ,(lvar 'y))
              (,(lvar 'y) . 3))
            '()
            #t)
(test-unify `(name x any) 3
            `((,(lvar 'x) . ,(lvar 'y))
              (,(lvar 'y) . any))
            `(((list (name y ,(bound))) . (list 3)))
            #f)
(test-unify `(name x any) 3
            `((,(lvar 'x) . ,(lvar 'y))
              (,(lvar 'y) . any))
            `(((list (name x ,(bound)) (name y ,(bound))) . (list 2 3)))
            #t)
(test-unify `(name x any) 3
            `((,(lvar 'x) . ,(lvar 'y))
              (,(lvar 'y) . any))
            `(((list (name x ,(bound)) (name y ,(bound))) . (list 3 2)))
            #t)
(test-unify `(name x any) 5
              `((,(lvar 'y) . 4)
                (,(lvar 'z) . 2))
              `(((list (name z ,(bound))) . (list 3))
                ((list (name z ,(bound)) (name y ,(bound)))
                 . (list 2 4)))
              #f)
(test-unify `(name x any) 5
              `((,(lvar 'y) . 4)
                (,(lvar 'z) . 17))
              `(((list (name z ,(bound))) . (list 3))
                ((list (name z ,(bound)) (name y ,(bound)))
                 . (list 2 4)))
              #t)
(test-unify `(name x any) 5
              `((,(lvar 'y) . 4)
                (,(lvar 'z) . 17)
                (,(lvar 'x) . any))
              `(((list (name x ,(bound))) . (list 3))
                ((list (name x ,(bound)) (name y ,(bound)))
                 . (list 5 4)))
              #f)
(test-unify `(name x any) 5
              `((,(lvar 'y) . 4)
                (,(lvar 'z) . 17)
                (,(lvar 'x) . any))
              `(((list (name x ,(bound))) . (list 3))
                ((list (name x ,(bound)) (name y ,(bound)))
                 . (list 6 4)))
              #t)
(test-unify `(name x any) 5
              `((,(lvar 'y) . 4)
                (,(lvar 'z) . 17)
                (,(lvar 'x) . any))
              `(((list (name x ,(bound))) . (list 3))
                ((list (name z ,(bound)) (name y ,(bound)))
                 . (list 5 4)))
              #t)

;; mismatch-name
(test-unify `(list (name a any) foo)
            `(list (mismatch-name a_!_1 any) (mismatch-name a_!_1 any))
            `((,(lvar 'a) . foo)) '()
            #f)
(test-unify `(list (name a any) foo)
            `(list (mismatch-name a_!_1 any) (mismatch-name a_!_1 any))
            `(list (name a ,(bound)) foo)
            `((,(lvar 'a) . bar)) '()
            `((,(lvar 'a) . bar)) '())
(test-unify `(list (name a (nt x)) (name b (nt x)) D) 
            `(list (mismatch-name x_!_1 (nt x)) (mismatch-name x_!_1 (nt x))
                   (mismatch-name x_!_1 (nt x)))
            `(list (name a ,(bound)) (name b ,(bound)) (cstr (x) D))
            '() '()
            `((,(lvar 'a) . (nt x))
              (,(lvar 'b) . (nt x)))
            `(((list (name b ,(bound))) . (list (name a ,(bound))))
              ((list (name a ,(bound))) . (list (cstr (x) D)))
              ((list (name b ,(bound))) . (list (cstr (x) D)))))

;; occurs check on bind (in instantiate*)
(test-unify `(name a any)
            `(name b any)
            `((,(lvar 'a) . (list 5 (name b ,(bound))))
              (,(lvar 'b) . (nt Q))) 
            '() #f)
(test-unify `(name a any)
            `(name b any)
            `((,(lvar 'a) . (list 5 (name c ,(bound))))
              (,(lvar 'c) . ,(lvar 'b))
              (,(lvar 'b) . ,(lvar 'd))
              (,(lvar 'd) . (nt Q))) 
            '() #f)

;; resolve fully
;; (necessary for parameter elimination to be correct)
(check-not-false
 (env-equal?
  (disunify
   '(a)
   '(list (name a any) (name a any))
   '(list (name b any) 2)
   (env '#hash() '())
   #f)
  (env (hash (lvar 'a) 'any (lvar 'b) 'any)
       (list (dq '() `((list (name b ,(bound))) (list 2)))))))

;; unfold non-terminals correctly
(let ()
  (define-language lnt
    (a (a a) 
       b)
    (b (n n))
    (c (n n n))
    (d (b b b))
    (n number))
  
  (check-equal?
   (unify '(list 1 2 3) '(nt a) empty-env lnt)
   (unif-fail))
  (check-equal?
   (unify '(nt c) '(nt a) empty-env lnt)
   (unif-fail))
  (check-equal?
   (unify '(nt d) '(nt a) empty-env lnt)
   (unif-fail))
  (check-not-equal?
   (unify '(list 1 2 3) '(nt c) empty-env lnt)
   (unif-fail))
  (check-not-equal?
   (unify '(list 1 2) '(nt a) empty-env lnt)
   (unif-fail))
  )



(let ([untested
       (sort (hash-map unify*-untested (λ (k v) k))
             string<=?
             #:key (λ (x) (format "~s" x)))])
  (unless (null? untested)
    (eprintf "untested pattern pairs:\n"))
  (for ([untested (in-list untested)])
    (eprintf "  ~a vs ~a\n" (car untested) (cdr untested))))
