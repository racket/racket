#lang s-exp typed-racket/base-env/extra-env-lang

(require syntax/stx)

(module types typed/racket/base
  (provide Stxof
           Stx-Null
           Stx-Pairof
           Stx-Listof)
  (define-type (Stxof t)
    (U t (Syntaxof t)))
  (define-type Stx-Null
    (Stxof Null))
  (define-type (Stx-Pairof a b)
    (Stxof (Pairof a b)))
  (define-type (Stx-Listof a)
    (Rec lst
         (U Stx-Null
            (Stx-Pairof a lst)))))
(require 'types)
(provide (all-from-out 'types))

(begin-for-syntax
  (define (-Stxof t)
    (Un t (-Syntax t)))
  (define -Stx-Null
    (-Stxof -Null))
  (define (-Stx-Pairof a b)
    (-Stxof (-pair a b)))
  (define (-Stx-Listof a)
    (-mu lst
         (Un -Stx-Null
             (-Stx-Pairof a lst)))))

(type-environment
 [stx-null? (make-pred-ty -Stx-Null)]
 [stx-pair? (make-pred-ty (-Stx-Pairof Univ Univ))]
 [stx-list? (make-pred-ty (-Stx-Listof Univ))]
 [stx->list (-poly (a)
              (cl->* (-> (-Stx-Listof a) (-lst a))
                     (-> (-Syntax Univ)
                         (Un (-val #f) (-lst (-Syntax Univ))))))]
 [stx-car (-poly (a b)
            (cl->*
             (-> (-Stx-Pairof a b) a)
             (-> (-Stx-Listof a) a)))]
 [stx-cdr (-poly (a b)
            (cl->*
             (-> (-Stx-Pairof a b) b)
             (-> (-lst a) (-lst a))
             (-> (-Stx-Listof a) (-Stx-Listof a))))]
 [stx-map (-polydots (c a b)
            (cl->*
             (-> (-> a c) (-Stx-Pairof a (-Stx-Listof a)) (-pair c (-lst c)))
             ((list
               ((list a) (b b) . ->... . c)
               (-Stx-Listof a))
              ((-Stx-Listof b) b) . ->... .(-lst c))))]
 [module-or-top-identifier=?
  (-> (-Syntax -Symbol) (-Syntax -Symbol) -Boolean)])
