#lang racket/base
(require syntax/parse
         (for-template racket/unsafe/ops racket/base (prefix-in k: '#%kernel))
         "../utils/utils.rkt"
         (utils tc-utils)
         (optimizer utils logging))

(provide apply-opt-expr)

(define-syntax-class apply-op
  #:commit
  #:literals (+ *)
  (pattern + #:with identity #'0)
  (pattern * #:with identity #'1))

(define-syntax-class apply-opt-expr
  #:commit
  #:literals (k:apply map #%plain-app #%app)
  (pattern ((~and kw #%plain-app) (~and appl k:apply) op:apply-op
            ((~and kw2 #%plain-app) (~and m map) f l))
           #:with opt
           (begin (reset-unboxed-gensym)
           (with-syntax ([(f* lp v lst) (map unboxed-gensym '(f* loop v lst))]
                         [l ((optimize) #'l)]
                         [f ((optimize) #'f)])
             (log-optimization "apply-map" "apply-map deforestation."
                               this-syntax)
             (add-disappeared-use #'appl)
             (add-disappeared-use #'kw2)
             (add-disappeared-use #'m)
             (syntax/loc/origin
              this-syntax #'kw
              (let ([f* f])
                (let lp ([v op.identity] [lst l])
                  (if (null? lst)
                      v
                      (lp (op v (f* (unsafe-car lst)))
                          (unsafe-cdr lst))))))))))
