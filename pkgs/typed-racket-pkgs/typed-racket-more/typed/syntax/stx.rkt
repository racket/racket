#lang s-exp typed-racket/base-env/extra-env-lang

(require syntax/stx)

(begin-for-syntax
 (define (-stx-list type)
   (Un (-lst type) (-Syntax (-lst type)))))

(type-environment
 [stx-null? (make-pred-ty (Un -Null (-Syntax -Null)))]
 [stx-pair? (make-pred-ty (Un (-pair Univ Univ) (-Syntax (-pair Univ Univ))))]
 [stx-list? (make-pred-ty (-stx-list Univ))]
 [stx->list (-poly (a)
              (cl->* (-> (-lst a) (-lst a))
                     (-> (-Syntax (-lst a)) (-lst (-Syntax a)))
                     (-> (-Syntax Univ) (-val #f))))]
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
             (-> (-> a c) (-pair a (-lst a)) (-pair c (-lst c)))
             (-> (-> a c) (-Syntax (-pair a (-lst a))) (-pair c (-lst c)))
             ((list
               ((list (-Syntax a)) ((-Syntax b) b) . ->... . c)
               (Un (-lst a) (-Syntax (-lst a))))
              ((Un (-lst b) (-Syntax (-lst b))) b) . ->... .(-lst c))))]
 [module-or-top-identifier=?
  (-> (-Syntax -Symbol) (-Syntax -Symbol) -Boolean)])
