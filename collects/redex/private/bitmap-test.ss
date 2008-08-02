(module bitmap-test mzscheme
  (require "bitmap-test-util.ss"
           "../pict.ss"
           "../reduction-semantics.ss")
  
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
  (test (language->pict lang #f) "language.png")
  
  (define-extended-language lang++ lang
    (e .... number (+ e e))
    (v .... number))
  
  (test (language->pict lang++ #f) "extended-language.png")
  
  (define red
    (reduction-relation
     lang
     (--> ((λ (x) e) v) (S x v e))))
  
  ;; tests: reduction-relation
  (test (reduction-relation->pict red)
        "reduction-relation.png")

  (test (reduction-relation->pict 
         (extend-reduction-relation red lang (--> 1 2)))
        "extended-reduction-relation.png")
  
  (define-metafunction lang
    [(S x v e) e])
  
  (test (metafunction->pict S)
        "metafunction.png")
  
  (printf "bitmap-test.ss: ")
  (done))
