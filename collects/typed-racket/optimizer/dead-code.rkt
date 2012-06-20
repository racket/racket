#lang racket/base

(require syntax/parse
         (for-template racket/base racket/flonum racket/fixnum)
         "../utils/utils.rkt"
         (types type-table)
         (utils tc-utils)
         (optimizer utils logging))

(provide dead-code-opt-expr)

(define-syntax-class predicate
  #:literals (flvector? fxvector? exact-integer? fixnum? flonum? vector? string? bytes?)
  [pattern (~and x:id (~or flvector? exact-integer? fixnum? flonum? vector? fxvector?))])

(define (pure? stx)
  (syntax-parse stx
    #:literals (#%plain-app)
    [(#%plain-app f:predicate x:id)
     #:when (eq? 'lexical (identifier-binding #'x))
     (add-disappeared-use #'f)
     (add-disappeared-use #'x)
     #true]
    [else #false]))

(define (optimize/drop-pure stx) 
  (cond [(pure? stx)
         (log-optimization "useless pure code"
                           "Unreachable pure code elimination."
                           stx)
         (syntax/loc stx (void))]
        [else ((optimize) stx)]))

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
                   (#%expression (begin #,(optimize/drop-pure #'tst)
                                        #,((optimize) #'thn))))))
  (pattern ((~and kw (~literal if)) tst:expr thn:expr els:expr)
           #:when (contradiction? #'tst)
           #:with opt
           (begin (log-optimization "dead then branch"
                                    "Unreachable then branch elimination."
                                    #'thn)
                  (quasisyntax/loc/origin 
                   this-syntax #'kw
                   (#%expression (begin #,(optimize/drop-pure #'tst)
                                        #,((optimize) #'els)))))))
