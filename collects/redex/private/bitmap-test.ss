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

(define-metafunction lang
  [(S x v e) e])

(test (render-metafunction S)
      "metafunction.png")

(define-metafunction lang
  [(T x y)
   1
   (side-condition (not (eq? (term x) (term y))))]
  [(T x x) 
   n
   (where n 2)])

;; in this test, the metafunction has 2 clauses 
;; with a side-condition on the first clause
;; and a 'where' in the second clause
(test (render-metafunction T) "metafunction-T.png")

;; in this test, teh `x' is italic and the 'z' is sf, since 'x' is in the grammar, and 'z' is not.
(test (render-lw 
       lang 
       (to-lw ((λ (x) (x x))
               (λ (z) (z z)))))
      "lw.png")

(define-metafunction lang
  [(TL 1) (a
           ,(term-let ((x 1))
                      (term x))
           below-only)]
  [(TL 2) (a
           ,(term-let ((x 1))
                      (term x)) beside
           below)])

;; this tests that term-let is sucked away properly
;; when the metafunction is rendered
(test (render-metafunction TL) "metafunction-TL.png")

(define-metafunction lang
  [(Name (name x-arg arg)) 
   ,(term-let ((x-term-let 1))
              (term (x-where x-term-let)))
   (where x-where 2)])

;; this tests that the three variable bindings
;; (x-arg, x-term-let, and x-where) 
;; all show up in the output.
(test (render-metafunction Name) "metafunction-Name.png")

(printf "bitmap-test.ss: ")
(done)
