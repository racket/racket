#lang s-exp typed-racket/base-env/extra-env-lang

(require syntax/stx)

[stx-null? (make-pred-ty (Un (-val '()) (-Syntax (-val '()))))]
[stx-pair? (make-pred-ty (Un (-pair Univ Univ) (-Syntax (-pair Univ Univ))))]
[stx-list? (make-pred-ty (Un (-lst Univ) (-Syntax (-lst Univ))))]
[stx-car (-poly (a b)
           (cl->*
            (-> (-pair a b) a)
            (-> (-lst a) a)
            (-> (-Syntax (-pair a b)) (-Syntax a))
            (-> (-Syntax (-lst a)) (-Syntax a))))]
[stx-cdr (-poly (a b)
           (cl->*
            (-> (-pair a b) b)
            (-> (-lst a) (-lst a))
            (-> (-Syntax (-pair a (-lst b))) (-lst (-Syntax b)))
            (-> (-Syntax (-pair a b)) (-Syntax b))
            (-> (-Syntax (-lst a)) (-lst (-Syntax a)))))]
[stx-map (-polydots (c a b)
           (cl->*
            (-> (-> (-Syntax a) c) (-pair a (-lst a)) (-pair c (-lst c)))
            (-> (-> (-Syntax a) c) (-Syntax (-pair a (-lst a))) (-pair c (-lst c)))
            ((list
              ((list (-Syntax a)) ((-Syntax b) b) . ->... . c)
              (Un (-lst a) (-Syntax (-lst a))))
             ((Un (-lst b) (-Syntax (-lst b))) b) . ->... .(-lst c))))]
[module-or-top-identifier=?
 (-> (-Syntax -Symbol) (-Syntax -Symbol) -Boolean)]

