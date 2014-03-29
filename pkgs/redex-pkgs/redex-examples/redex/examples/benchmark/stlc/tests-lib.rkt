#lang racket/base
(require redex/reduction-semantics)
(provide stlc-tests
         consistent-with?)

(define (consistent-with? t1 t2)
  (define table (make-hash))
  (let loop ([t1 t1]
             [t2 t2])
    (cond
      [(and (pair? t1) (pair? t2))
       (and (loop (car t1) (car t2))
            (loop (cdr t1) (cdr t2)))]
      [(and (symbol? t1) 
            (symbol? t2)
            (not (equal? t1 t2))
            (same-first-char-or-empty-and-numbers? t1 t2))
       (cond
         [(equal? t1 t2) #t]
         [else
          (define bound (hash-ref table t1 #f))
          (cond
            [bound (equal? bound t2)]
            [else
             (hash-set! table t1 t2)
             #t])])]
      [else (equal? t1 t2)])))

(define (same-first-char-or-empty-and-numbers? t1 t2)
  (define (first-char s) (string-ref (symbol->string s) 0))
  (cond
    [(equal? t1 '||)
     (regexp-match #rx"[^[0-9]*$" (symbol->string t2))]
    [(equal? t2 '||)
     (regexp-match #rx"[^[0-9]*$" (symbol->string t1))]
    [else
     (equal? (first-char t1)
             (first-char t2))]))

(define-syntax-rule
  (stlc-tests uses-bound-var?
              typeof
              red
              reduction-step-count
              Eval
              subst)
  (begin
    
    (test-equal (term (uses-bound-var? () 5))
                #f)
    (test-equal (term (uses-bound-var? () nil))
                #f)
    (test-equal (term (uses-bound-var? () (λ (x int) x)))
                #t)
    (test-equal (term (uses-bound-var? () (λ (x int) y)))
                #f)
    (test-equal (term (uses-bound-var? () ((λ (x int) x) 5)))
                #t)
    (test-equal (term (uses-bound-var? () ((λ (x int) xy) 5)))
                #f)
    
    (test-equal (consistent-with? '(λ (z1 int) (λ (z2 int) z2))
                                  '(λ (z int) (λ (z1 int) z)))
                #f)
    (test-equal (consistent-with? '(λ (z1 int) (λ (z2 int) z2))
                                  '(λ (z int) (λ (z1 int) z1)))
                #t)
    
    (test-equal (term (subst ((+ 1) 1) x 2))
                (term ((+ 1) 1)))
    (test-equal (term (subst ((+ x) x) x 2))
                (term ((+ 2) 2)))
    (test-equal (term (subst ((+ y) x) x 2))
                (term ((+ y) 2)))
    (test-equal (term (subst ((+ y) z) x 2))
                (term ((+ y) z)))
    (test-equal (term (subst ((λ (x int) x) x) x 2))
                (term ((λ (x int) x) 2)))
    (test-equal (consistent-with? (term (subst ((λ (y int) x) x) x 2))
                                  (term ((λ (y int) 2) 2)))
                #t)
    (test-equal (consistent-with? (term (subst ((λ (y int) x) x) x (λ (q int) z)))
                                  (term ((λ (y int) (λ (q int) z)) (λ (q int) z))))
                #t)
    (test-equal (consistent-with? (term (subst ((λ (y int) x) x) x (λ (q int) y)))
                                  (term ((λ (y2 int) (λ (q int) y)) (λ (q int) y))))
                #t)
    (test-equal (consistent-with? (term (subst (λ (z int) (λ (z1 int) z)) q 1))
                                  (term (λ (z int) (λ (z1 int) z))))
                #t)
    
    (test-equal (judgment-holds (typeof • 5 τ) τ)
                (list (term int)))
    (test-equal (judgment-holds (typeof • nil τ) τ)
                (list (term (list int))))
    (test-equal (judgment-holds (typeof • (cons 1) τ) τ)
                (list (term ((list int) → (list int)))))
    (test-equal (judgment-holds (typeof • ((cons 1) nil) τ) τ)
                (list (term (list int))))
    (test-equal (judgment-holds (typeof • (λ (x int) x) τ) τ)
                (list (term (int → int))))
    (test-equal (judgment-holds (typeof • (λ (x (int → int)) (λ (y int) x)) τ) τ)
                (list (term ((int → int) → (int → (int → int))))))
    (test-equal (judgment-holds (typeof • ((+ ((+ 1) 2)) ((+ 3) 4)) τ) τ)
                (list (term int)))
    
    (test-->> red (term ((λ (x int) x) 7)) (term 7))
    (test-->> red (term (((λ (x int) (λ (x int) x)) 2) 1)) (term 1))
    (test-->> red (term (((λ (x int) (λ (y int) x)) 2) 1)) (term 2))
    (test-->> red 
              (term ((λ (x int) ((cons x) nil)) 11))
              (term ((cons 11) nil)))
    (test-->> red 
              (term ((λ (x int) ((cons x) nil)) 11))
              (term ((cons 11) nil)))
    (test-->> red 
              (term ((cons ((λ (x int) x) 11)) nil))
              (term ((cons 11) nil)))
    (test-->> red
              (term (cons ((λ (x int) x) 1)))
              (term (cons 1)))
    (test-->> red
              (term ((cons ((λ (x int) x) 1)) nil))
              (term ((cons 1) nil)))
    (test-->> red
              (term (hd ((λ (x int) ((cons x) nil)) 11)))
              (term 11))
    (test-->> red
              (term (tl ((λ (x int) ((cons x) nil)) 11)))
              (term nil))
    (test-->> red
              (term (tl nil))
              "error")
    (test-->> red
              (term (hd nil))
              "error")
    (test-->> red
              (term ((+ 1) (hd nil)))
              "error")
    (test-->> red
              (term ((+ ((+ 1) 2)) ((+ 3) 4)))
              (term 10))
    (test-->> red
              (term ((λ (f (int → (list int))) (f 3)) (cons 1)))
              (term ((cons 1) 3)))
    (test-->> red
              (term ((λ (f (int → int)) (f 3)) (+ 1)))
              (term 4))
              
    (test-equal (Eval (term ((λ (x int) x) 3)))
                (term 3))
    
    (test-equal (reduction-step-count (term (λ (x int) x)))
                0)
    (test-equal (reduction-step-count (term ((λ (x int) x) 1)))
                1)
    (test-equal (reduction-step-count (term ((λ (x int) x) 1)))
                1)
    (test-equal (reduction-step-count (term ((cons 1) nil)))
                0)
    (test-equal (reduction-step-count (term (hd ((cons 1) nil))))
                1)
    (test-equal (reduction-step-count (term (hd nil)))
                1)
    (test-equal (reduction-step-count (term ((λ (x int) x) (hd ((cons 1) nil)))))
                2)
    
    (test-results)))