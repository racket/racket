#lang racket

(require "../reduction-semantics.rkt"
         "../private/jdg-gen.rkt"
         "../private/generate-term.rkt"
         "test-util.rkt"
         (only-in "../private/pat-unify.rkt"
                  bound
                  lvar))

(let ()
  (define-language L0)
  
  (test-equal (check-dq `a `a (make-hash) L0 (hash))
              #f)
  (test-equal (check-dq `a `b (make-hash) L0 (hash))
              #t)
  (test-equal (check-dq `(list a) `(list a) (make-hash) L0 (hash))
              #f)
  (test-equal (check-dq `(list a) `(list b) (make-hash) L0 (hash))
              #t)
  (test-equal (check-dq `(list number) `(list variable) (make-hash) L0 (hash))
              #t)
  (test-equal (check-dq `(list a) `(list number) (make-hash) L0 (hash))
              #t)
  (test-equal (check-dq `(list 2) `(list variable-not-otherwise-mentioned) (make-hash) L0 (hash))
              #t)
  (test-equal (check-dq `(list a b) `(list a number) (make-hash) L0 (hash))
              #t)
  (test-equal (check-dq `(list a b) `(list a b) (make-hash) L0 (hash))
              #f)
  (test-equal (check-dq `(list (name a ,(bound)))
                        `(list (name a ,(bound)))
                        (make-hash)
                        L0
                        (hash (lvar 'a) 'number))
              #f)
  (test-equal (check-dq `(name a ,(bound))
                        `(name b ,(bound))
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
  
  (test-equal (judgment-holds (sum z (s z) (s z)))
              #t)
  
  (test-equal (judgment-holds (sum (s z) (s z) (s (s z))))
              #t)
  
  (test-equal (generate-term nats #:satisfying (sum z (s z) n) +inf.0)
              (term (sum z (s z) (s z))))
  
  (test-equal (generate-term nats #:satisfying (sum (s z) (s z) n) +inf.0)
              (term (sum (s z) (s z) (s (s z)))))
  
  (for ([_ 100])
    (match (generate-term nats #:satisfying (sum n_1 n_2 n_3) 5)
      [`(sum ,l ,r ,res)
       (test-equal (judgment-holds (sum ,l ,r n) n)
                   `(,res))])))

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
  
  (for/and ([_ 100])
    (match (generate-term lists #:satisfying (not-in a l) 5)
      [`(not-in a ,l)
       (unless (judgment-holds (not-in a ,l)) (printf "l: ~s\n" l))
       (test-equal (judgment-holds (not-in a ,l))
                   #t)]
      [#f
       (void)])))

(let ()
  (define-language STLC
    (τ int
       (τ → τ))
    (Γ ([x τ] Γ)
       •)
    (x variable-not-otherwise-mentioned))
  
  (define-metafunction STLC
    [(lookup x ([x τ] Γ))
     τ]
    [(lookup x ([x_1 τ] Γ))
     (lookup x Γ)])
  
  (test-equal (generate-term STLC
                             #:satisfying
                             (lookup x ([x int] ([x (int → int)] •)))
                             (int → int)
                             6)
              #f))

(let ()
  
  (define-language simple
    (e (+ e e)
       (+ e e e)
       number))
  
  (define-judgment-form simple
    #:mode (double I I)
    [(double (+ e_1 e_2) e_3)
     (where e_3 (+ (+ e_1 e_1) (+ e_2 e_2)))])
  
  (for ([_ 100])
    (define t (generate-term simple #:satisfying (double e_1 e_2) +inf.0))
    (match t
      [`(double ,e1 ,e2)
       (test-equal (judgment-holds (double ,e1 ,e2))
                   #t)]
      [#f
       (void)]))
  
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
  
  (test-equal (term (duplicate 1 2))
              '(+ (+ 1 1) (+ 2 2)))
  
  (test-equal (term (duplicate 1))
              '(+ 1 1 1))
  
  (test-equal (term (duplicate 2 2))
              '(+ 2 2 2))
  
  (test-equal (judgment-holds (double2 (+ 1 2) (+ (+ 1 1) (+ 2 2))))
              #t)
  
  (test-equal (judgment-holds (double2 1 (+ 1 1 1)))
              #t)
  
  (test-equal (judgment-holds (double2 (+ 2 2) (+ 2 2 2)))
              #t)
  
  (for ([_ 100])
    (define t (generate-term simple #:satisfying (double2 e_1 e_2) +inf.0))
    (match t
      [`(double2 ,e1 ,e2)
       (test-equal (judgment-holds (double2 ,e1 ,e2))
                   #t)]
      [#f
       (void)])))

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
  
  (test-equal (judgment-holds (typeof ([x_1 int] ([x_1 (int → int)] •)) (x_1 5) int))
              #f)
  (test-equal (judgment-holds (typeof ([x_2 int] ([x_1 (int → int)] •)) (x_1 5) int))
              #t)
  
  (for ([_ 100])
    (define term (generate-term STLC #:satisfying (typeof Γ e τ) 6))
    (match term
      [`(typeof ,g ,e ,t)
       (define tp (judgment-holds (typeof ,g ,e τ) τ))
       (test-equal tp `(,t))]
      [#f
       (void)]))
  
  (for ([_ 100])
    (define term (generate-term if-l #:satisfying (typ-if Γ e τ) 5))
    (match term
      [`(typ-if ,g ,e ,t)
       (define tp (judgment-holds (typ-if ,g ,e τ) τ))
       (test-equal tp `(,t))]
      [#f
       (void)]))
  
  (define g (redex-generator STLC (typeof • e τ) 5))
  (define terms (filter values (for/list ([_ 400]) (g))))
  (test-equal (length terms)
              (length (remove-duplicates terms)))
  (map (match-lambda
         [`(typeof ,g ,e ,t)
          (define tp (judgment-holds (typeof ,g ,e τ) τ))
          (test-equal tp `(,t))])
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
  
  (test-equal (generate-term l #:satisfying (filtered (1 (2 (3 (4 •)))) 3 (1 (2 (4 •)))) +inf.0)
              '(filtered (1 (2 (3 (4 •)))) 3 (1 (2 (4 •)))))
  (test-equal (generate-term l #:satisfying (filtered (1 (2 (3 (4 •)))) 5 (1 (2 (4 •)))) +inf.0)
              #f)
  
  (for ([_ 50])
    (define term (generate-term l #:satisfying (filtered e_1 n e_2) 5))
    (match term
      [`(filtered ,e1 ,n ,e2)
       (define tp (judgment-holds (filtered ,e1 ,n e_2) e_2))
       (test-equal tp `(,e2))]
      [#f
       (void)]))
  
  (for ([_ 50])
    (define t (generate-term l #:satisfying (fltr n e) e_1 5))
    (match t
      [`((fltr ,n ,e) = ,e1)
       (test-equal (term (fltr ,n ,e)) e1)]
      [#f
       (void)]))
  
  (define g (redex-generator l (fltr n e_1) = e_2 5))
  (define terms (filter values (for/list ([_ 50]) (g))))
  (test-equal (length terms)
              (length (remove-duplicates terms)))
  (map (match-lambda
         [`((fltr ,n ,e) = ,e1)
          (test-equal (term (fltr ,n ,e)) e1)])
       terms)
  (void)

  )

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
  
  
  (test-equal (generate-term L #:satisfying (is-a? a) any +inf.0)
              '((is-a? a) = T))
  (test-equal (generate-term L #:satisfying (is-a? b) any +inf.0)
              '((is-a? b) = F))
  (test-equal (generate-term L #:satisfying (is-a? c) any +inf.0)
              '((is-a? c) = F))
  (test-equal (generate-term L #:satisfying (is-a/b? a) any +inf.0)
              '((is-a/b? a) = T))
  (test-equal (generate-term L #:satisfying (is-a/b? b) any +inf.0)
              '((is-a/b? b) = T))
  (test-equal (generate-term L #:satisfying (is-a/b? c) any +inf.0)
              '((is-a/b? c) = F))
  
  (test-equal (generate-term L #:satisfying (is-a/b/c/d/e? a) any +inf.0)
              '((is-a/b/c/d/e? a) = T))
  (test-equal (generate-term L #:satisfying (is-a/b/c/d/e? b) any +inf.0)
              '((is-a/b/c/d/e? b) = T))
  (test-equal (generate-term L #:satisfying (is-a/b/c/d/e? c) any +inf.0)
              '((is-a/b/c/d/e? c) = T))
  (test-equal (generate-term L #:satisfying (is-a/b/c/d/e? d) any +inf.0)
              '((is-a/b/c/d/e? d) = T))
  (test-equal (generate-term L #:satisfying (is-a/b/c/d/e? e) any +inf.0)
              '((is-a/b/c/d/e? e) = T))
  (test-equal (generate-term L #:satisfying (is-a/b/c/d/e? f) any +inf.0)
              '((is-a/b/c/d/e? f) = F)))

;; errors for unsupprted pats
(let ()
  (define-language L (n any))
  (define-metafunction L
    [(g (side-condition any #f)) any])
  (define-metafunction L
    [(f n) (g n)])
  (test (with-handlers ((exn:fail? exn-message))
          (generate-term L #:satisfying (f any) any +inf.0)
          "didn't raise an exception")
        #rx".*generate-term:.*side-condition.*"))
(let ()
  (define-language L (n any))
  (define-metafunction L
    [(g any_1 ...) (any_1 ...)])
  (define-metafunction L
    [(f n) (g n)])
  (test (with-handlers ((exn:fail? exn-message))
          (generate-term L #:satisfying (f any) any +inf.0)
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
          (generate-term L #:satisfying (f r_1) r_2 +inf.0))
        #rx".*generate-term:.*undatum.*"))
             

(let ()
  (define-language L (n 2))
  (define-metafunction L
    [(n any) any])
  (define-metafunction L
    [(f n) (n 1)])
  (test-equal (generate-term L #:satisfying (f any_1) any_2 +inf.0)
              '((f 2) = (2 1))))

(let ()
  (define-language L (n 2))
  (define-metafunction L
    [(n any) any])
  (define-metafunction L
    [(f n) n])
  (test-equal (generate-term L #:satisfying (f any_1) any_2 +inf.0)
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
  
  (test-equal (generate-term l #:satisfying (t 1 1) 1 +inf.0)
              '((t 1 1) = 1))
  (test-equal (generate-term l #:satisfying (t 1 1) 2 +inf.0)
              #f)
  (test-equal (generate-term l #:satisfying (t 1 2) 2 +inf.0)
              '((t 1 2) = 2))
  (test-equal (generate-term l #:satisfying (t 1 2) 3 +inf.0)
              #f)
  (test-equal (generate-term l #:satisfying (t 1 3) 3 +inf.0)
              '((t 1 3) = 3))
  (test-equal (generate-term l #:satisfying (t 1 3) 4 +inf.0)
              #f)
  (test-equal (generate-term l #:satisfying (t 6 7) 4 +inf.0)
              '((t 6 7) = 4))
  (test-equal (generate-term l #:satisfying (t 6 7) 3 +inf.0)
              #f)
  (test-equal (generate-term l #:satisfying (t 6 7) 2 +inf.0)
              #f)
  (test-equal (generate-term l #:satisfying (t 6 7) 1 +inf.0)
              #f))

#;
(let ()
  (define-language L
    (e (or e e) b)
    (b T F))
  
  (define-relation L
    [(or-eval T)]
    [(or-eval (or T e_2))]
    [(or-eval (or (or e_1 e_2) e_3)) (or-eval (or e_1 (or e_2 e_3)))]
    [(or-eval (or F e)) (or-eval e)])
  
  (test-equal (generate-term L #:satisfying (or-eval F) +inf.0)
              #f)
  (test-equal (generate-term L #:satisfying (or-eval T) +inf.0)
              '(or-eval T))
  (test-equal (generate-term L #:satisfying (or-eval (or (or F F) T)) +inf.0)
              '(or-eval (or (or F F) T))))
