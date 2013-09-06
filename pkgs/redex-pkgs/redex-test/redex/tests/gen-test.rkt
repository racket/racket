#lang racket

(require redex/reduction-semantics
         redex/private/jdg-gen
         redex/private/generate-term
         "test-util.rkt"
         (only-in redex/private/pat-unify
                  bound
                  lvar))


(define-syntax-rule (is-not-false e)
  (test (not e) #f))

(define-syntax-rule (is-false e)
  (test e #f))

(define-syntax-rule (no-counterexample e)
  (test
   (counterexample? e)
   #f))

(define-language L0)

(let ()
  
  (test (check-dq (dq '() (list `a `a)) (make-hash) L0 (hash))
              #f)
  (test (check-dq (dq '() (list `a `b)) (make-hash) L0 (hash))
              #t)
  (test (check-dq (dq '() (list `(list a) `(list a))) (make-hash) L0 (hash))
              #f)
  (test (check-dq (dq '() (list `(list a) `(list b))) (make-hash) L0 (hash))
              #t)
  (test (check-dq (dq '() (list `(list number) `(list variable))) (make-hash) L0 (hash))
              #t)
  (test (check-dq (dq '() (list `(list a) `(list number))) (make-hash) L0 (hash))
              #t)
  (test (check-dq (dq '() (list `(list 2) `(list variable-not-otherwise-mentioned)))
                  (make-hash) L0 (hash))
              #t)
  (test (check-dq (dq '() (list `(list a b) `(list a number))) (make-hash) L0 (hash))
              #t)
  (test (check-dq (dq '() (list `(list a b) `(list a b))) (make-hash) L0 (hash))
              #f)
  (test (check-dq (dq '()
                            (list `(list (name a ,(bound)))
                                  `(list (name a ,(bound)))))
                        (make-hash)
                        L0
                        (hash (lvar 'a) 'number))
              #f)
  (test (check-dq (dq '()
                            (list `(name a ,(bound))
                                  `(name b ,(bound))))
                        (make-hash (list (cons (lvar 'a) '(1 2 3))
                                         (cons (lvar 'b) '(1 2 3))))
                        L0
                        (hash (lvar 'a) 'any (lvar 'b) 'any))
              #f)
  )

(let ()
  
  (define-language nats
    (n z (s n)))
  
  (define-judgment-form nats
    #:mode (sum I I O)
    #:contract (sum n n n)
    [(sum z n n)]
    [(sum (s n_1) n_2 (s n_3))
     (sum n_1 n_2 n_3)])
  
  (define-relation nats
    [(r-sum z n n)]
    [(r-sum (s n_1) n_2 (s n_3))
     (r-sum n_1 n_2 n_3)])
  
  (test (judgment-holds (sum z (s z) (s z)))
              #t)
  
  (test (judgment-holds (sum (s z) (s z) (s (s z))))
              #t)
  
  (test (generate-term nats #:satisfying (sum z (s z) n) +inf.0)
              '(sum z (s z) (s z)))
  
  (test (generate-term nats #:satisfying (sum (s z) (s z) n) +inf.0)
              '(sum (s z) (s z) (s (s z))))
  
  (test (generate-term nats #:satisfying (sum z z (s z)) 5)
              #f)
  
  (for ([_ 50])
    (match (generate-term nats #:satisfying (sum n_1 n_2 n_3) 5)
      [`(sum ,l ,r ,res)
       (test (judgment-holds (sum ,l ,r n) n)
                   `(,res))])
    (match (generate-term nats #:satisfying (r-sum n_1 n_2 n_3) 5)
      [`(r-sum ,l ,r ,res)
       (test (term (r-sum ,l ,r ,res))
                   #t)]))
  (no-counterexample
   (redex-check nats
                #:satisfying
                (sum n_1 n_2 n_3)
                (equal? (car (judgment-holds
                              (sum n_1 n_2 n_4) n_4))
                        (term n_3))
                #:attempts 50
                #:print? #f))
  
  (no-counterexample
   (redex-check nats
                #:satisfying
                (r-sum n_1 n_2 n_3)
                (judgment-holds
                 (sum n_1 n_2 n_3))
                #:attempts 50
                #:print? #f))
  )

(let ()
  
  (define-language lists
    (l (x l)
       ())
    (x variable-not-otherwise-mentioned))
  
  (define-judgment-form lists
    #:mode (not-in I I)
    [(not-in x_0 ())]
    [(not-in x_0 (x_1 l_0))
     (different x_0 x_1) ;; this line has to go first or this won't terminate
     (not-in x_0 l_0)])
  
  (define-judgment-form lists
    #:mode (different I I)
    [(different x_!_1 x_!_1)])
  
  (test
   (judgment-holds (not-in a (b (c (d (e (f ())))))))
   #t)
  
  (test
   (judgment-holds (not-in a (b (c (d (a (f ())))))))
   #f)
  
  (test
   (generate-term lists #:satisfying (not-in a (b (a (c l)))) 5)
   #f)
  
  (test
   (not (generate-term lists #:satisfying (not-in a (b (c (d (e (f ())))))) +inf.0))
   #f)
  
  (for/and ([_ 50])
    (match (generate-term lists #:satisfying (not-in a l) 5)
      [`(not-in a ,l)
       (unless (judgment-holds (not-in a ,l)) (printf "l: ~s\n" l))
       (test (judgment-holds (not-in a ,l))
                   #t)]
      [#f
       (void)]))
  
  (no-counterexample
   (redex-check lists
                #:satisfying
                (not-in x l)
                (judgment-holds (not-in x l))
                #:attempts 50
                #:print? #f))
                
  )

(let ()
  
  (define-language TL
    (T (N T T)
       (L number)))
  
  (define-relation TL
    [(tree (N T_1 T_2))
     (tree T_1)
     (tree T_2)]
    [(tree (L number))])
  
  (test
   (not
    (empty?
     (filter
      values
      (for/list ([_ 100])
        (generate-term TL #:satisfying (tree T) 2)))))
   #t)
  )

(let ()
  
  (define-language simple
    (e (+ e e)
       (+ e e e)
       number))
  
  (define-judgment-form simple
    #:mode (double I I)
    [(double (+ e_1 e_2) e_3)
     (where e_3 (+ (+ e_1 e_1) (+ e_2 e_2)))])
  
  (for ([_ 50])
    (define t (generate-term simple #:satisfying (double e_1 e_2) +inf.0))
    (match t
      [`(double ,e1 ,e2)
       (test (judgment-holds (double ,e1 ,e2))
                   #t)]
      [#f
       (void)]))
  
  (no-counterexample
   (redex-check simple
                #:satisfying
                (double e_1 e_2)
                (judgment-holds
                 (double e_1 e_2))
                #:attempts 50
                #:print? #f))
  
  (define-metafunction simple
    [(duplicate e_1 e_1)
     (+ e_1 e_1 e_1)]
    [(duplicate e_1 e_2)
     (+ e_3 e_4)
     (where e_31 e_1)
     (where e_3 (+ e_31 e_31))
     (where e_42 e_2)
     (where e_4 (+ e_42 e_42))]
    [(duplicate e_1)
     (duplicate e_1 e_1)])
  
  (define-judgment-form simple
    #:mode (double2 I I)
    [(double2 (+ e_1 e_2) e_3)
     (where e_3 (duplicate e_1 e_2))]
    [(double2 e_1 e_2)
     (where e_2 (duplicate e_1))])
  
  (test (term (duplicate 1 2))
              '(+ (+ 1 1) (+ 2 2)))
  
  (test (term (duplicate 1))
              '(+ 1 1 1))
  
  (test (term (duplicate 2 2))
              '(+ 2 2 2))
  
  (test (judgment-holds (double2 (+ 1 2) (+ (+ 1 1) (+ 2 2))))
              #t)
  
  (test (judgment-holds (double2 1 (+ 1 1 1)))
              #t)
  
  (test (judgment-holds (double2 (+ 2 2) (+ 2 2 2)))
              #t)
  
  (for ([_ 50])
    (define t (generate-term simple #:satisfying (double2 e_1 e_2) +inf.0))
    (match t
      [`(double2 ,e1 ,e2)
       (test (judgment-holds (double2 ,e1 ,e2))
                   #t)]
      [#f
       (void)]))
  
  (no-counterexample
   (redex-check simple
                #:satisfying
                (double2 e_1 e_2)
                (judgment-holds 
                 (double2 e_1 e_2))
                #:attempts 50
                #:print? #f))
  
  )

(let ()
  (define-language STLC
    (e (λ (x τ) e)
       (e e)
       x
       i
       add1)
    (τ int
       (τ → τ))
    (Γ ([x τ] Γ)
       •)
    (i integer)
    (x variable-not-otherwise-mentioned))
  
  (define-judgment-form STLC
    #:mode (typeof I I O)
    #:contract (typeof Γ e τ)
    [(typeof Γ i int)]
    [(typeof Γ x (lookup x Γ))]
    [(typeof Γ add1 (int → int))]
    [(typeof Γ (λ (x τ_1) e) (τ_1 → τ_2))
     (typeof ([x τ_1] Γ) e τ_2)]
    [(typeof Γ (e_1 e_2) τ)
     (typeof Γ e_1 (τ_2 → τ))
     (typeof Γ e_2 τ_2)])
  
  (define-metafunction STLC
    [(lookup x ([x τ] Γ))
     τ]
    [(lookup x ([x_1 τ] Γ))
     (lookup x Γ)])
  
  (define-extended-language if-l STLC
    (e (if0 e e e)
       ....))
  
  (define-extended-judgment-form if-l typeof
    #:mode (typ-if I I O)
    [(typ-if Γ (if0 e_1 e_2 e_3) τ)
     (typ-if Γ e_1 int)
     (typ-if Γ e_2 τ)
     (typ-if Γ e_3 τ)])
  
  (test (generate-term STLC
                             #:satisfying
                             (lookup x ([x int] ([x (int → int)] •))) = (int → int)
                             6)
              #f)
  
  (test (judgment-holds (typeof ([x_1 int] ([x_1 (int → int)] •)) (x_1 5) int))
              #f)
  (test (judgment-holds (typeof ([x_2 int] ([x_1 (int → int)] •)) (x_1 5) int))
              #t)
  
  (for ([_ 50])
    (define term (generate-term STLC #:satisfying (typeof Γ e τ) 6))
    (match term
      [`(typeof ,g ,e ,t)
       (define tp (judgment-holds (typeof ,g ,e τ) τ))
       (test tp `(,t))]
      [#f
       (void)]))
  
  (no-counterexample
   (redex-check STLC
                #:satisfying
                (typeof Γ e τ)
                (equal? (car (judgment-holds
                              (typeof Γ e τ_2) τ_2))
                        (term τ))
                #:attempts 50
                #:print? #f))
  
  (for ([_ 50])
    (define term (generate-term if-l #:satisfying (typ-if Γ e τ) 5))
    (match term
      [`(typ-if ,g ,e ,t)
       (define tp (judgment-holds (typ-if ,g ,e τ) τ))
       (test tp `(,t))]
      [#f
       (void)]))
  
  (no-counterexample
   (redex-check STLC
                #:satisfying
                (typ-if Γ e τ)
                (equal? (car (judgment-holds
                              (typ-if Γ e τ_2) τ_2))
                        (term τ))
                #:attempts 50
                #:print? #f))
  
  (define g (redex-generator STLC (typeof • e τ) 5))
  (define terms (filter values (for/list ([_ 100]) (g))))
  (test (length terms)
              (length (remove-duplicates terms)))
  (map (match-lambda
         [`(typeof ,g ,e ,t)
          (define tp (judgment-holds (typeof ,g ,e τ) τ))
          (test tp `(,t))])
       terms)
  (void)
  )

(let ()
  (define-language l
    (e (n e)
       •)
    (n number))
  
  (define-metafunction l
    fltr : n e -> e
    [(fltr n •)
     •]
    [(fltr n (n e))
     (fltr n e)]
    [(fltr n (n_0 e))
     (n_0 (fltr n e))])
  
  (define-judgment-form l
    #:mode (filtered I I O)
    [(filtered e n (fltr n e))])
  
  (test (generate-term l #:satisfying (filtered (1 (2 (3 (4 •)))) 3 (1 (2 (4 •)))) +inf.0)
              '(filtered (1 (2 (3 (4 •)))) 3 (1 (2 (4 •)))))
  (test (generate-term l #:satisfying (filtered (1 (2 (3 (4 •)))) 5 (1 (2 (4 •)))) +inf.0)
              #f)
  
  (for ([_ 25])
    (define term (generate-term l #:satisfying (filtered e_1 n e_2) 5))
    (match term
      [`(filtered ,e1 ,n ,e2)
       (define tp (judgment-holds (filtered ,e1 ,n e_2) e_2))
       (test tp `(,e2))]
      [#f
       (void)]))
  
  (no-counterexample
   (redex-check l
                #:satisfying
                (filtered e_1 n e_2)
                (equal? (car (judgment-holds
                              (filtered e_1 n e_3) e_3))
                        (term e_2))
                #:attempts 25
                #:print? #f))
  
  (for ([_ 25])
    (define t (generate-term l #:satisfying (fltr n e) = e_1 5))
    (match t
      [`((fltr ,n ,e) = ,e1)
       (test (term (fltr ,n ,e)) e1)]
      [#f
       (void)]))
  
  (no-counterexample
   (redex-check l 
                #:satisfying 
                (fltr n e) = e_1
                (equal? (term (fltr n e)) (term e_1))
                #:attempts 25
                #:print? #f))
  
  (define g (redex-generator l (fltr n e_1) = e_2 5))
  (define terms (filter values (for/list ([_ 50]) (g))))
  (test (length terms)
              (length (remove-duplicates terms)))
  (map (match-lambda
         [`((fltr ,n ,e) = ,e1)
          (test (term (fltr ,n ,e)) e1)])
       terms)
  (void))

(let ()
  
  (define-language L
    (v a
       b
       c
       d
       e
       f)
    (bool T F))
  
  (define-metafunction L
    [(is-a? a) T]
    [(is-a? v) F])
  
  (define-metafunction/extension is-a? L
    [(is-a/b? b) T])
  
  (define-metafunction/extension is-a/b? L
    [(is-a/b/c/d/e? c) T]
    [(is-a/b/c/d/e? d) T]
    [(is-a/b/c/d/e? e) T])
  
  
  (test (generate-term L #:satisfying (is-a? a) = any +inf.0)
              '((is-a? a) = T))
  (test (generate-term L #:satisfying (is-a? b) = any +inf.0)
              '((is-a? b) = F))
  (test (generate-term L #:satisfying (is-a? c) = any +inf.0)
              '((is-a? c) = F))
  (test (generate-term L #:satisfying (is-a/b? a) = any +inf.0)
              '((is-a/b? a) = T))
  (test (generate-term L #:satisfying (is-a/b? b) = any +inf.0)
              '((is-a/b? b) = T))
  (test (generate-term L #:satisfying (is-a/b? c) = any +inf.0)
              '((is-a/b? c) = F))
  
  (test (generate-term L #:satisfying (is-a? a) = F +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a? b) = T +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a? c) = T +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a/b? a) = F +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a/b? b) = F +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a/b? c) = T +inf.0)
              #f)
  
  (test (generate-term L #:satisfying (is-a/b/c/d/e? a) = any +inf.0)
              '((is-a/b/c/d/e? a) = T))
  (test (generate-term L #:satisfying (is-a/b/c/d/e? b) = any +inf.0)
              '((is-a/b/c/d/e? b) = T))
  (test (generate-term L #:satisfying (is-a/b/c/d/e? c) = any +inf.0)
              '((is-a/b/c/d/e? c) = T))
  (test (generate-term L #:satisfying (is-a/b/c/d/e? d) = any +inf.0)
              '((is-a/b/c/d/e? d) = T))
  (test (generate-term L #:satisfying (is-a/b/c/d/e? e) = any +inf.0)
              '((is-a/b/c/d/e? e) = T))
  (test (generate-term L #:satisfying (is-a/b/c/d/e? f) = any +inf.0)
              '((is-a/b/c/d/e? f) = F))
  
  (test (generate-term L #:satisfying (is-a/b/c/d/e? a) = F +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a/b/c/d/e? b) = F +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a/b/c/d/e? c) = F +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a/b/c/d/e? d) = F +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a/b/c/d/e? e) = F +inf.0)
              #f)
  (test (generate-term L #:satisfying (is-a/b/c/d/e? f) = T +inf.0)
              #f))

;; errors for unsupprted pats
(let ()
  (define-language L (n any))
  (define-metafunction L
    [(g (side-condition any #f)) any])
  (define-metafunction L
    [(f n) (g n)])
  (test (with-handlers ((exn:fail? exn-message))
          (generate-term L #:satisfying (f any) = any +inf.0)
          "didn't raise an exception")
        #rx".*generate-term:.*side-condition.*"))
(let ()
  (define-language L (n any))
  (define-metafunction L
    [(g any_1 ...) (any_1 ...)])
  (define-metafunction L
    [(f n) (g n)])
  (test (with-handlers ((exn:fail? exn-message))
          (generate-term L #:satisfying (f any) = any +inf.0)
          "didn't raise an exception")
        #rx".*generate-term:.*repeat.*"))

(let ()
  (define-language L
    (n 1))
  
  (define-judgment-form L
    #:mode (jf I)
    [(jf (n_0 n_1 ...))
     (jf n_0)
     (where (n_2 ...) (n_1 ...))
     (jf (n_1 ...))]
    [(jf 1)]
    [(jf 2)]
    [(jf 3)])
  
  (test (with-handlers ([exn:fail? exn-message])
          (generate-term L #:satisfying (jf n_1 n_2 n_3) +inf.0))
        #rx".*generate-term:.*repeat.*"))

(let ()
  (define-language L
    (q any)
    (r number))
  
  (define-metafunction L
    [(f r_1)
     r_2
     (where r_2 ,(+ (term r) (term r)))]
    [(f (q_1 q_2))
     q_3
     (where q_3 (f q_1))
     (where q_3 (f q_2))])
  
  (test (with-handlers ([exn:fail? exn-message])
          (generate-term L #:satisfying (f r_1) = r_2 +inf.0))
        #rx".*generate-term:.*undatum.*"))


(let ()
  (define-language L (n 2))
  (define-metafunction L
    [(n any) any])
  (define-metafunction L
    [(f n) (n 1)])
  (test (generate-term L #:satisfying (f any_1) = any_2 +inf.0)
              '((f 2) = (2 1))))

(let ()
  (define-language L (n 2))
  (define-metafunction L
    [(n any) any])
  (define-metafunction L
    [(f n) n])
  (test (generate-term L #:satisfying (f any_1) = any_2 +inf.0)
              '((f 2) = 2)))

(let ()
  
  (define-language l (n number))
  
  (define-metafunction l
    [(t n n)
     1]
    [(t n 2)
     2]
    [(t 1 n)
     3]
    [(t n_1 n_2)
     4])
  
  (test (generate-term l #:satisfying (t 1 1) = 1 +inf.0)
              '((t 1 1) = 1))
  (test (generate-term l #:satisfying (t 1 1) = 2 +inf.0)
              #f)
  (test (generate-term l #:satisfying (t 1 2) = 2 +inf.0)
              '((t 1 2) = 2))
  (test (generate-term l #:satisfying (t 1 2) = 3 +inf.0)
              #f)
  (test (generate-term l #:satisfying (t 1 3) = 3 +inf.0)
              '((t 1 3) = 3))
  (test (generate-term l #:satisfying (t 1 3) = 4 +inf.0)
              #f)
  (test (generate-term l #:satisfying (t 6 7) = 4 +inf.0)
              '((t 6 7) = 4))
  (test (generate-term l #:satisfying (t 6 7) = 3 +inf.0)
              #f)
  (test (generate-term l #:satisfying (t 6 7) = 2 +inf.0)
              #f)
  (test (generate-term l #:satisfying (t 6 7) = 1 +inf.0)
              #f)
  
  (no-counterexample
   (redex-check l 
                #:satisfying 
                (t n_1 n_2) = n_3
                (equal? (term (t n_1 n_2)) (term n_3))
                #:attempts 50
                #:print? #f))
  
  )

(let ()
  (define-judgment-form L0
    #:mode (J I I)
    [(J any_1 any_1)])
  
  (define f (generate-term L0 #:satisfying (J any_2 any_3)))
  
  (let/ec k
    (for ([i (in-range 100)])
      (define t (f 10))
      (unless (equal? (list-ref t 1) (list-ref t 2))
        (test (list-ref t 1) (list-ref t 2))
        ;; after a single test failure, stop running the loop
        (k (void))))))


(let ()
  (define-language L
    (e (or e e) b)
    (b T F))
  
  (define-relation L
    [(or-eval T)]
    [(or-eval (or T e_2))]
    [(or-eval (or (or e_1 e_2) e_3)) (or-eval (or e_1 (or e_2 e_3)))]
    [(or-eval (or F e)) (or-eval e)])
  
  (test (generate-term L #:satisfying (or-eval F) +inf.0)
              #f)
  (test (generate-term L #:satisfying (or-eval T) +inf.0)
              '(or-eval T))
  (test (generate-term L #:satisfying (or-eval (or (or F F) T)) +inf.0)
              '(or-eval (or (or F F) T))))

(let ()
  (define-language wrong-nums
    (n z (s z)))
  
  (define-relation wrong-nums
    [(sum z n n)]
    [(sum (s n_1) n_2 (s n_3))
     (sum n_1 n_2 n_3)])
  
  (for ([n 10])
    (define r (random 100000))
    (random-seed r)
    (define g (redex-generator wrong-nums (sum n_1 n_2 n_3) 1))
    (with-handlers
        ([exn? (λ (e) 
                 (printf "random seed: ~s\n" r)
                 (raise e))])
      (for ([n 10])
        (g)))))

(let ()
  (define-relation L0
    [(a any)])
  (define-relation L0
    [(b any)])
  (define-relation L0
    [(c any) (a (b any))])
  
  (define-metafunction L0
    [(f any)
     (a ny)])

  (define-judgment-form L0
    #:mode (J I O)
    [(J any_1 any_2)
     (J (a any_1) any_2)]
    [(J #t #f)])
  
  (test (term (a #t))
        #t)
  (test (term (a 42))
        #t)
  (test (term (a #f))
        #t)
  
  (test (with-handlers ([exn:fail? exn-message])
          (generate-term L0 #:satisfying (c any) +inf.0))
        #rx".*generate-term:.*relation.*")
  
  (test (with-handlers ([exn:fail? exn-message])
          (generate-term L0 #:satisfying (f any_1) = any_2 +inf.0))
        #rx".*generate-term:.*relation.*")
  
  (test (with-handlers ([exn:fail? exn-message])
          (generate-term L0 #:satisfying (J any_1 any_2) +inf.0))
        #rx".*generate-term:.*relation.*"))

(let ()
  
  (define-relation L0
    [(R number)
     number]
    [(R string)])
  
  (define-relation L0
    [(R2 number)
     #f]
    [(R2 string)])
  
  (define-relation L0
    [(R3 any)
     any])
  
  (is-not-false (generate-term L0 #:satisfying (R 5) +inf.0))
  (is-not-false (generate-term L0 #:satisfying (R "hello") +inf.0))
  (is-false (generate-term L0 #:satisfying (R #t) +inf.0))
  (is-false (generate-term L0 #:satisfying (R #f) +inf.0))
  
  (is-false (generate-term L0 #:satisfying (R2 5) +inf.0))
  (is-not-false (generate-term L0 #:satisfying (R2 "hello") +inf.0))
  (is-false (generate-term L0 #:satisfying (R2 #t) +inf.0))
  (is-false (generate-term L0 #:satisfying (R2 #f) +inf.0))
  
  (is-not-false (generate-term L0 #:satisfying (R3 5) +inf.0))
  (is-not-false (generate-term L0 #:satisfying (R3 "hello") +inf.0))
  (is-not-false (generate-term L0 #:satisfying (R3 #t) +inf.0))
  (is-false (generate-term L0 #:satisfying (R3 #f) +inf.0))
  
  
  (define-judgment-form L0
    #:mode (J I)
    [(J (any))
     (side-condition any)]
    [(J (any_1 any_2))
     (J any_1)
     (J any_2)])
  
  (is-not-false (generate-term L0 #:satisfying (J (1)) +inf.0))
  (is-not-false (generate-term L0 #:satisfying (J ((1) (2))) +inf.0))
  (is-false (generate-term L0 #:satisfying (J ((1) (#f))) 5))
  (is-false (generate-term L0 #:satisfying (J ((#f) (2))) 5))
  (is-not-false (generate-term L0 #:satisfying (J ((#t) (2))) 5)))

(let ()
  
  (define-metafunction L0
    [(f (any_1 any_2))
     2]
    [(f any_1)
     1])
  
  (define-judgment-form L0
    #:mode (J I I)
    [(J (any_1 any_2) 2)]
    [(J any_1 1)])
  
  
  (test (generate-term L0
                             #:satisfying
                             (f (any_1 any_2)) = 1
                             +inf.0)
              #f)
  
  (test (not
               (generate-term L0
                              #:satisfying
                              (f (any_1 any_2)) = 2
                              +inf.0))
              #f)
  (is-not-false
   (for/and ([_ 50])
     (match (generate-term L0
                           #:satisfying
                           (f any) = 1
                           5)
       [`((f (,a ,b)) = 1) #f]
       [else #t]))))

(let ()
  
  (define-relation L0
    [(R (#f #f #f) 3)]
    [(R (#f #f) 2)]
    [(R #f 1)])
  
  (is-not-false
   (generate-term
    L0
    #:satisfying
    (R any_1 any_2)
    +inf.0))
  
  (is-not-false
   (generate-term
    L0
    #:satisfying
    (R (any_1 any_2) any_3)
    +inf.0))
  
  (is-not-false
   (generate-term
    L0
    #:satisfying
    (R (any_1 any_2 any_3) any_4)
    +inf.0))
  
  (is-false
   (generate-term
    L0
    #:satisfying
    (R (any_1 any_2) 3)
    +inf.0))
  
  (is-not-false
   (generate-term
    L0
    #:satisfying
    (R #f any)
    +inf.0))
  )

(let ()
  
  (define-relation L0
    [(not-mem any_1 (any_2 any_3))
     (not-mem any_1 any_3)
     (where (any_!_4 any_!_4) (any_1 any_2))]
    [(not-mem any_1 ())])
  
  (is-not-false
   (generate-term
    L0
    #:satisfying
    (not-mem d (a (b (c ()))))
    +inf.0))
  
  (is-false
   (generate-term
    L0
    #:satisfying
    (not-mem b (a (b (c ()))))
    +inf.0)))

(let ()
  (test (generate-term L0 natural #:i-th 0) 0)
  (let ([gen-ith (generate-term L0 natural #:i-th)])
    (test (gen-ith 0) 0)
    (test (gen-ith 1) 1)))

(let ()
  (define-language L
    (e ::= (name aha! any)))

  (define-judgment-form L
    #:mode (J I I)
    [(J e e)])
  
  (is-not-false
   (generate-term L #:satisfying (J e_1 e_2) 10)))