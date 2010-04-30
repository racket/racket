#lang scheme
(require "bitmap-test-util.ss"
         "../main.ss")

;; tests: 
;;  - language,
;;  - multi-line non-terminals, single-line non-terminals
(define-language lang
  (e (e e) 
     x
     (λ (x) e)
     number)
  (v number (λ (x) e))
  ((x y) variable-not-otherwise-mentioned))

(test (render-language lang) "language.png")

(test (render-language lang #:nts '(e v)) "language-nox.png")

(define-extended-language lang++ lang
  (e .... number (+ e e))
  (v .... number))

(test (render-language lang++) "extended-language.png")

(define red
  (reduction-relation
   lang
   (--> ((λ (x) e) v) (S x v e))))

;; tests: reduction-relation
(test (render-reduction-relation red)
      "reduction-relation.png")

(test (render-reduction-relation 
       (extend-reduction-relation red lang (--> 1 2)))
      "extended-reduction-relation.png")

;; this test should fail because it gets the order wrong
;; for the where/side-conditions
(define red2
  (reduction-relation
   lang
   (--> (number_a number_b number_c number_d)
        any_z
        (where (any_x any_y) (number_a number_b))
        (side-condition (= (term number_c) 5))
        (where any_z any_x)
        (side-condition (= (term number_d) 5)))))

(test (render-reduction-relation red2)
      "red2.png")
        
(define-metafunction lang
  [(S x v e) e])

(test (render-metafunction S)
      "metafunction.png")

(define-metafunction lang
  [(T x y)
   1
   (side-condition (not (eq? (term x) (term y))))]
  [(T x x) 
   (any_1 any_2)
   (where any_1 2)
   (where any_2 2)])

;; in this test, the metafunction has 2 clauses 
;; with a side-condition on the first clause
;; and a 'where' in the second clause
(test (render-metafunction T) "metafunction-T.png")

;; in this test, the `x' is italic and the 'z' is sf, since 'x' is in the grammar, and 'z' is not.
(test (render-lw 
       lang 
       (to-lw ((λ (x) (x x))
               (λ (z) (z z)))))
      "lw.png")

(define-metafunction lang
  [(TL 1) (a
           ,(term-let ((x (term 1)))
                      (term x))
           below-only)]
  [(TL 2) (a
           ,(term-let ((x (term 1)))
                      (term x)) beside
           below)])

;; this tests that term-let is sucked away properly
;; when the metafunction is rendered
(test (render-metafunction TL) "metafunction-TL.png")

(define-metafunction lang
  [(Name (name x-arg arg)) 
   ,(term-let ((x-term-let (term 1)))
              (term (x-where x-term-let)))
   (where x-where 2)])

;; this tests that the three variable bindings
;; (x-arg, x-term-let, and x-where) 
;; all show up in the output.
(test (render-metafunction Name) "metafunction-Name.png")

;; same as previous, but with vertical organization of the bindings
(test (parameterize ([metafunction-pict-style 'up-down/vertical-side-conditions])
        (render-metafunction Name))
      "metafunction-Name-vertical.png")

;; makes sure that there is no overlap inside or across metafunction calls  
;; or when there are unquotes involved
(define-metafunction lang
  [(multi-arg a
              b
              c)
   ((multi-arg a 
               b 
               c)
    (multi-arg a 
               b 
               c))]
  [(multi-arg unquote-test)
   (,@(term (multi-arg with-unquote))
    ,@(term (multi-arg with-unquote))
    ,@(term (multi-arg with-unquote)))])

(test (render-metafunction multi-arg) "metafunction-multi-arg.png")

;; makes sure that the LHS and RHS of metafunctions are appropriately
;; rewritten

(define-metafunction lang
  subst : e x e -> e
  [(subst x x e) e]
  [(subst number x e) number]
  [(subst x_1 x_2 e) x_1]
  [(subst (e_1 e_2) x e)
   ((subst e_1 x e) (subst e_2 x e))]
  [(subst (λ (x) e_b) x e)
   (λ (x) e)]
  [(subst (λ (x_f) e_f) x_a e_a)
   (λ (x_f) (subst e_f x_a e_a))])

(define (subst-rw lws)
  (list ""
        (list-ref lws 2)
        "{"
        (list-ref lws 3)
        ":="
        (list-ref lws 4)
        "}"))

(test (with-compound-rewriter 'subst subst-rw
        (render-metafunction subst))
      "metafunction-subst.png")


;; make sure two metafunctions simultaneously rewritten line up properly
(test (render-metafunctions S T TL) "metafunctions-multiple.png")

;; Non-terminal superscripts
(test (render-lw lang (to-lw (x_^abcdef x_q^abcdef)))
      "superscripts.png")

;; `variable-not-in' in `where' RHS rendered as `fresh'
(define-metafunction lang
  [(f (name n 1)) 
   (x x_1 x_2 x_3)
   (where x ,(variable-not-in 'y 'x))
   (where (x_1 x_2) ,(variables-not-in 'z '(x1 x2)))
   (where x_3 (variables-not-in 'z '(x1 x2)))])
(test (render-metafunction f) "var-not-in.png")
(let ([variable-not-in list])
  (define-metafunction lang
    [(g 1) 
     x
     (where x ,(variable-not-in 'y 'x))])
  (test (render-metafunction g) "var-not-in-rebound.png"))

;; hidden `where' and `side-condition' clauses
(define-metafunction lang
  [(mf-hidden 1)
   2
   (where/hidden number 7)
   (side-condition/hidden (= 1 2))])
(test (render-metafunction mf-hidden) "mf-hidden.png")
(test (render-reduction-relation
       (reduction-relation
        lang
        (--> 1
             2
             (where/hidden number 7)
             (side-condition/hidden (= 1 2)))))
      "rr-hidden.png")

(printf "bitmap-test.ss: ")
(done)
