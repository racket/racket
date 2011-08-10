#lang scheme/base

(require syntax/parse
         (for-template scheme/base)
         "../utils/utils.rkt"
         (types type-table)
         (optimizer utils logging))

(provide dead-code-opt-expr)

(define-syntax-class dead-code-opt-expr
  #:commit
  ;; if one of the brances of an if is unreachable, we can eliminate it
  ;; we have to keep the test, in case it has side effects
  (pattern ((~and kw (~literal if)) tst:expr thn:expr els:expr)
           #:when (tautology? #'tst)
           #:with opt
           (begin (log-optimization "dead else branch"
                                    "Unreachable else branch elimination."
                                    #'els)
                  (quasisyntax/loc/origin 
                   this-syntax #'kw
                   (#%expression (begin #,((optimize) #'tst)
                                        #,((optimize) #'thn))))))
  (pattern ((~and kw (~literal if)) tst:expr thn:expr els:expr)
           #:when (contradiction? #'tst)
           #:with opt
           (begin (log-optimization "dead then branch"
                                    "Unreachable then branch elimination."
                                    #'thn)
                  (quasisyntax/loc/origin 
                   this-syntax #'kw
                   (#%expression (begin #,((optimize) #'tst)
                                        #,((optimize) #'els)))))))
