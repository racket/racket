#lang racket/base

(require syntax/parse racket/promise syntax/stx unstable/sequence
         (for-template racket/base)
         "../utils/utils.rkt"
         (types type-table)
         (utils tc-utils)
         (optimizer utils logging))

(provide dead-code-opt-expr)

;; The type based 'dead code elimination' done by this file just makes the dead code obvious.
;; The actual elimination step is left to the compiler.


;; if the conditional has a known truth value, we can reveal this
;; we have to keep the test, in case it has side effects
(define-syntax-class tautology
  #:attributes (opt)
  (pattern e:opt-expr
    #:when (tautology? #'e)
    #:attr opt (delay #'(begin e.opt #t))))

(define-syntax-class contradiction
  #:attributes (opt)
  (pattern e:opt-expr
    #:when (contradiction? #'e)
    #:attr opt (delay #'(begin e.opt #f))))


(define-syntax-class dead-code-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  (pattern ((~and kw if) tst:tautology thn:opt-expr els:expr)
    #:do [(log-optimization "dead else branch" "Unreachable else branch elimination." #'els)]
    #:with opt (syntax/loc/origin this-syntax #'kw (if tst.opt thn.opt els)))
  (pattern ((~and kw if) tst:contradiction thn:expr els:opt-expr)
    #:do [(log-optimization "dead then branch" "Unreachable then branch elimination." #'thn)]
    #:with opt (syntax/loc/origin this-syntax #'kw (if tst.opt thn els.opt)))
  (pattern ((~and kw lambda) formals . bodies)
    #:when (dead-lambda-branch? #'formals)
    #:with opt this-syntax)
  (pattern ((~and kw case-lambda) (formals . bodies) ...)
    #:when (for/or ((formals (in-syntax #'(formals ...))))
             (dead-lambda-branch? formals))
    #:with opt
      (quasisyntax/loc/origin
        this-syntax #'kw
        (begin0
          (case-lambda
            #,@(for/list ((formals (in-syntax #'(formals ...)))
                          (bodies  (in-syntax #'(bodies ...)))
                          #:unless (dead-lambda-branch? formals))
                  (cons formals (stx-map (optimize) bodies))))
          ;; We need to keep the syntax objects around in the generated code with the correct bindings
          ;; so that CheckSyntax displays the arrows correctly
          #,@(for/list ((formals (in-syntax #'(formals ...)))
                        (bodies  (in-syntax #'(bodies ...)))
                        #:when (dead-lambda-branch? formals))
                (log-optimization
                  "dead case-lambda branch"
                  "Unreachable case-lambda branch elimination."
                  formals)
                #`(λ #,formals . #,bodies))))))
