#lang racket/base
(require racket/file)
  ;; these tests just make sure that errors don't
  ;; happen. These tests are really only last resorts
  ;; for testing functions that aren't easily extraced
  ;; from the pict.rkt library

(module test racket/base)
  
  (require redex/reduction-semantics
           redex/pict)
  
  (require texpict/mrpict mred/mred mzlib/class)
  
  (define-language empty-language)
  
  (define-language var-ab
    [var (a 
          b)])
  (void (render-language var-ab))
  
  (define-language var-not-ab
    [var (variable-except x
                          y)])
  (void (render-language var-not-ab))
  
  (let ()
    (define-metafunction empty-language [(zero any_in) 0])
    (void (render-metafunction zero)))

  (void
   (render-reduction-relation
    (reduction-relation
     empty-language
     (--> number_const
          ,(term
            (+ number_const 0))))))
  
  (void
   (render-reduction-relation 
    (reduction-relation
     empty-language
     (--> a b
          (fresh x)
          (fresh y)))))
  

  (define-language x1-9 
    (x 1 2 3 4 5 6 7 8 9))
  
  (define-extended-language x0-10 x1-9
    (x 0 .... 10))
  
  (void (render-language x0-10))
  
  (let ([tmp (make-temporary-file "redex-pict-test~a.pdf")])
    (render-language x0-10 tmp)
    (delete-file tmp))
  
  (printf "pict-test.rkt passed\n")
