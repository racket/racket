#lang racket
(require redex)
(provide λ λv red typeof)

(define-language λ
  (e (e e)
     x
     (λ (x τ) e)
     (if0 e e e)
     (+ e e)
     number)
  (τ (τ -> τ) num)
  (x variable-not-otherwise-mentioned))

(define-extended-language λv λ
  (v (λ (x τ) e) number)
  (E hole
     (v E) (E e)
     (if0 E e e)
     (+ E e) (+ v E))
  (Γ · (x τ Γ)))

(define red
  (reduction-relation
   λv
   (--> (in-hole E (+ number_1 number_2))
        (in-hole E (Σ number_1 number_2))
        "+")
   (--> (in-hole E (if0 0 e_1 e_2))
        (in-hole E e_1)
        "if0t")
   (--> (in-hole E (if0 number e_1 e_2))
        (in-hole E e_2)
        "if0f"
        (side-condition 
          (not (= 0 (term number)))))
   (--> (in-hole E ((λ (x τ) e) v))
        (in-hole E (subst x v e))
        "βv")))

(define-metafunction λv
  Σ : number number -> number
  [(Σ number_1 number_2)
   ,(+ (term number_1) (term number_2))])

(define-metafunction λv
  subst : x any any -> any
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst x_1 any_1 (λ (x_1 τ_1) any_2))
   (λ (x_1 τ_1) any_2)]
  ;; 2. general purpose capture avoiding case
  [(subst x_1 any_1 (λ (x_2 τ_2) any_2))
   (λ (x_new τ_2)
     (subst x_1 any_1
            (subst-var x_2 x_new
                       any_2)))
   (where (x_new)
          ,(variables-not-in
            (term (x_1 any_1 any_2)) 
            (term (x_2))))]
  ;; 3. replace x_1 with e_1
  [(subst x_1 any_1 x_1) any_1]
  ;; 4. x_1 and x_2 are different, so don't replace
  [(subst x_1 any_1 x_2) x_2]
  ;; the last cases cover all other expressions
  [(subst x_1 any_1 (any_2 ...))
   ((subst x_1 any_1 any_2) ...)]
  [(subst x_1 any_1 any_2) any_2])

(define-metafunction λv
  subst-var : x any any -> any
  [(subst-var x_1 any_1 x_1) any_1]
  [(subst-var x_1 any_1 (any_2 ...))
   ((subst-var x_1 any_1 any_2) ...)]
  [(subst-var x_1 any_1 any_2) any_2])

(define-judgment-form λv
  #:mode (typeof I I O)
  #:contract (typeof Γ e τ)
  
  [---------------------
   (typeof Γ number num)]
  
  [(typeof Γ e_1 num)
   (typeof Γ e_2 num)
   --------------------------
   (typeof Γ (+ e_1 e_2) num)]
  
  [(typeof Γ e_1 num)
   (typeof Γ e_2 τ)
   (typeof Γ e_3 τ)
   ------------------------------
   (typeof Γ (if0 e_1 e_2 e_3) τ)]
  
  [(where τ (lookup Γ x))
   ----------------------
   (typeof Γ x τ)]
  
  [(typeof Γ e_1 (τ_2 -> τ))
   (typeof Γ e_2 τ_2)
   --------------------------
   (typeof Γ (e_1 e_2) τ)]
  
  [(typeof (x_1 τ_1 Γ) e τ)
   ----------------------------------------
   (typeof Γ (λ (x_1 τ_1) e) (τ_1 -> τ))])

(define-metafunction λv
  lookup : Γ x -> τ or #f
  [(lookup (x τ Γ) x) τ]
  [(lookup (x_1 τ Γ) x_2) (lookup Γ x_2)]
  [(lookup · x) #f])


;; remove this #; to run an example
#;
(traces red
        (term 
         (+ ((λ (n num)
               (if0 n
                    1
                    0))
             (+ 2 2))
            2)))

;; remove this #; to generate a random well-typed term
#;
(generate-term λv #:satisfying (typeof · e num) 5)


(define (preservation e)
  (define types (judgment-holds (typeof · ,e τ) τ))
  (unless (null? types)
    (unless (= 1 (length types)) (error 'preservation "multiple types! ~s" e)))
  (cond
    [(null? types) #t]
    [else
     (for/and ([v (apply-reduction-relation* red e)])
       (equal? (judgment-holds (typeof · ,v τ) τ)
               types))]))

(define (try-it)
  (redex-check λv 
               #:satisfying (typeof · e num) 
               (preservation (term e))))

(test-->> red (term ((λ (x num) x) 1)) 1)
(test-->> red (term (((λ (x num) (λ (y num) x)) 1) 2)) 1)
(test-->> red (term (((λ (x num) (λ (y num) y)) 1) 2)) 2)
(test-->> red (term (((λ (x num) (λ (x num) x)) 1) 2)) 2)
(test-->> red (term (((λ (x num) (λ (y num) x)) 1) 2)) 1)
(test-->> red (term ((λ (x num) (+ x x)) 2)) 4)
(test-->> red (term ((λ (x num) (if0 x x (+ x 1))) 2)) 3)
(test-->> red (term ((λ (x num) (if0 x x (+ x 1))) 0)) 0)
(test-->> red 
          (term (((λ (x num) (λ (y num) (λ (z num) x))) 1) 2))
          (term (λ (z num) 1)))
(test-->> red
          (term (+ (+ 1 2) (+ 3 4)))
          (term 10))

(test-equal (term (subst x y x)) (term y))
(test-equal (term (subst x y z)) (term z))
(test-equal (term (subst x y (x (y z)))) (term (y (y z))))
(test-equal (term (subst x y ((λ (x num) x) ((λ (y1 num) y1) (λ (x num) z)))))
            (term ((λ (x num) x) ((λ (y2 num) y2) (λ (x num) z)))))
(test-equal (term (subst x y (if0 (+ 1 x) x x)))
            (term (if0 (+ 1 y) y y)))
(test-equal (term (subst x (λ (z num) y) (λ (y num) x)))
            (term (λ (y1 num) (λ (z num) y))))
(test-equal (term (subst x 1 (λ (y num) x)))
            (term (λ (y num) 1)))
(test-equal (term (subst x y (λ (y num) x)))
            (term (λ (y1 num) y)))
(test-equal (term (subst x (λ (y num) y) (λ (z num) (z2 z))))
            (term (λ (z1 num) (z2 z1))))
(test-equal (term (subst x (λ (z num) z) (λ (z num) (z1 z))))
            (term (λ (z2 num) (z1 z2))))
(test-equal (term (subst x z (λ (z num) (z1 z))))
            (term (λ (z2 num) (z1 z2))))
(test-equal (term (subst x3 5 (λ (x2 num) x2)))
            (term (λ (x1 num) x1)))
(test-equal (term (subst z * (λ (z num) (λ (x num) 1))))
            (term (λ (z num) (λ (x num) 1))))
(test-equal (term (subst q (λ (x num) z) (λ (z num) (λ (x num) q))))
            (term (λ (z1 num) (λ (x1 num) (λ (x num) z)))))
(test-equal (term (subst x 1 (λ (x num) (λ (x num) x))))
            (term (λ (x num) (λ (x num) x))))

(define (typecheck G e)
  (match (judgment-holds (typeof ,G ,e τ) τ)
    [(list) #f]
    [(list t) t]
    [_ (error 'typecheck
              "multiple typing derivations for term ~a in environment ~a"
              e G)]))

(test-equal (typecheck (term ·) (term 1))
            (term num))
(test-equal (typecheck (term ·) (term (1 1)))
            #f)
(test-equal (typecheck (term (x num ·)) (term x))
            (term num))
(test-equal (typecheck (term ·) (term x))
            #f)
(test-equal (typecheck (term ·) (term ((λ (x num) x) 1)))
            (term num))
(test-equal (typecheck (term ·) (term (((λ (x num) x) 1) 2)))
            #f)
(test-equal (typecheck (term ·) 
                       (term (((λ (f (num -> num))
                                 (λ (x num)
                                   (f x)))
                               (λ (x num) x))
                              1)))
            (term num))
(test-equal (typecheck (term ·)
                       (term (((λ (f (num -> num))
                                 (λ (x num)
                                   (f x)))
                               1)
                              (λ (x num) x))))
            #f)
(test-equal (typecheck (term ·) (term (+ (+ 1 2) 3)))
            (term num))
(test-equal (typecheck (term ·) (term (if0 1 (λ (x num) x) 3)))
            #f)
(test-equal (typecheck (term ·) (term (if0 1 2 3)))
            (term num))
(test-equal (typecheck (term ·) (term (λ (x num) (x 2))))
            #f)
(test-equal (typecheck (term ·)
                       (term (λ (x num)
                               (λ (x (num -> num))
                                 (x 0)))))
            (term (num -> ((num -> num) -> num))))
(test-equal (typecheck (term ·)
                       (term (λ (x (num -> num))
                               (λ (x num)
                                 (x 0)))))
            #f)

(test-results)
