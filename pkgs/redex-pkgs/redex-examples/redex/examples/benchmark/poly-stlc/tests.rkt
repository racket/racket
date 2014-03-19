#lang racket/base
(require redex/reduction-semantics 
         "poly-stlc-base.rkt")

(test-equal (judgment-holds (typeof • 5 τ) τ)
            (list (term int)))
(test-equal (judgment-holds (typeof • [nil @ int] τ) τ)
            (list (term (list int))))
(test-equal (judgment-holds (typeof • ([cons @ int] 1) τ) τ)
            (list (term ((list int) → (list int)))))
(test-equal (judgment-holds (typeof • (([cons @ int] 1) [nil @ int]) τ) τ)
            (list (term (list int))))
(test-equal (judgment-holds (typeof • (λ (x int) x) τ) τ)
            (list (term (int → int))))
(test-equal (judgment-holds (typeof • (λ (x (int → int)) (λ (y int) x)) τ) τ)
            (list (term ((int → int) → (int → (int → int))))))
(test-equal (judgment-holds
             (typeof •
                     ([tl @ int]
                      ([hd @ (list int)]
                       ((λ (l (list (list int)))
                          (([cons @ (list int)] (([cons @ int] 1) [nil @ int]))
                           l))
                        [nil @ (list int)])))
                     τ)
             τ)
            (list (term (list int))))
(test-equal (judgment-holds
             (typeof •
                     (([[map @ int] @ (list int)]
                       (λ (x int) (([cons @ int] x) [nil @ int])))
                      (([cons @ int] 2)
                       (([cons @ int] 4)
                        [nil @ int])))
                     τ)
             τ)
            (list (term (list (list int)))))

(test-->> red (term ((λ (x int) x) 7)) (term 7))
(test-->> red (term (((λ (x int) (λ (x int) x)) 2) 1)) (term 1))
(test-->> red (term (((λ (x int) (λ (y int) x)) 2) 1)) (term 2))
(test-->> red 
          (term ((λ (x int) (([cons @ int] x) [nil @ int])) 11))
          (term (([cons @ int] 11) [nil @ int])))
(test-->> red 
          (term ((λ (x int) (([cons @ int] x) [nil @ int])) 11))
          (term (([cons @ int] 11) [nil @ int])))
(test-->> red 
          (term (([cons @ int] ((λ (x int) x) 11)) [nil @ int]))
          (term (([cons @ int] 11) [nil @ int])))
(test-->> red
          (term ([cons @ int] ((λ (x int) x) 1)))
          (term ([cons @ int] 1)))
(test-->> red
          (term (([cons @ int] ((λ (x int) x) 1)) [nil @ int]))
          (term (([cons @ int] 1) [nil @ int])))
(test-->> red
          (term ([hd @ int] ((λ (x int) (([cons @ int] x) [nil @ int])) 11)))
          (term 11))
(test-->> red
          (term ([tl @ int] ((λ (x int) (([cons @ int] x) [nil @ int])) 11)))
          (term [nil @ int]))
(test-->> red
          (term ([tl @ int] [nil @ int]))
          "error")
(test-->> red
          (term ([hd @ int] [nil @ int]))
          "error")
(test-->> red
          (term ((λ (f (int → (list int))) (f 3)) ([cons @ int] 1)))
          (term (([cons @ int] 1) 3)))
(test-->> red
          (term 
           ([tl @ int]
            ([hd @ (list int)]
             ((λ (l (list (list int)))
                (([cons @ (list int)] (([cons @ int] 1) [nil @ int]))
                 l))
              [nil @ (list int)]))))
          (term [nil @ int]))

(test-->> red
          (term (([[map @ int] @ (list int)]
                  (λ (x int) (([cons @ int] x) [nil @ int])))
                 (([cons @ int] 2)
                  (([cons @ int] 4)
                   [nil @ int]))))
          (term (((cons @ (list int)) (((cons @ int) 2) (nil @ int)))
                 (((cons @ (list int)) (((cons @ int) 4) (nil @ int)))
                  (nil @ (list int))))))
(test-equal (Eval (term ((λ (x int) x) 3)))
            (term 3))


(test-equal (judgment-holds (typeof • ((+ ((+ 1) 2)) ((+ 3) 4)) τ) τ)
            (list (term int)))
(test-->> red
          (term ((+ 1) ([hd @ int] [nil @ int])))
          "error")
(test-->> red
          (term ((+ ((+ 1) 2)) ((+ 3) 4)))
          (term 10))
(test-->> red
          (term ((λ (f (int → int)) (f 3)) (+ 1)))
          (term 4))

(test-results)
