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
  (test (render-language lang) "language.png")
  
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
  
  (printf "bitmap-test.ss: ")
  (done))
