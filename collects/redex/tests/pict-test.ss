(module pict-test mzscheme
  ;; these tests just make sure that errors don't
  ;; happen. These tests are really only last resorts
  ;; for testing functions that aren't easily extraced
  ;; from the pict.ss library
  
  (require "../reduction-semantics.ss"
           "../pict.ss")
  
  (require (lib "mrpict.ss" "texpict")
           (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (define-language empty-language)
  
  (define-language var-ab
    [var (a 
          b)])
  (render-language var-ab)
  
  (define-language var-not-ab
    [var (variable-except x
                          y)])
  (render-language var-not-ab)
  
  (let ()
    (define-metafunction empty-language [(zero any_in) 0])
    (render-metafunction zero))

  (render-reduction-relation
   (reduction-relation
    empty-language
    (--> number_const
         ,(term
           (+ number_const 0)))))
  
  (render-reduction-relation 
   (reduction-relation
    empty-language
    (--> a b
         (fresh x)
         (fresh y))))
  

  (define-language x1-9 
    (x 1 2 3 4 5 6 7 8 9))
  
  (define-extended-language x0-10 x1-9
    (x 0 .... 10))
  
  (render-language x0-10)
  
  (printf "pict-test.ss passed\n"))
