#lang racket/base

(require syntax/parse syntax/stx unstable/sequence
         racket/syntax
         (for-template racket/base)
         "../utils/utils.rkt"
         (types type-table)
         (optimizer utils logging))

(provide dead-code-opt-expr)

;; The type based 'dead code elimination' done by this file just makes the dead code obvious.
;; The actual elimination step is left to the compiler.

(define-syntax-class dead-code-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  (pattern ((~and kw if) tst:opt-expr thn:opt-expr els:opt-expr)
    #:do [(define takes-true (test-position-takes-true-branch #'tst))
          (define takes-false (test-position-takes-false-branch #'tst))
          (unless takes-true
            (log-optimization "dead then branch" "Unreachable then branch elimination." #'thn))
          (unless takes-false
            (log-optimization "dead else branch" "Unreachable else branch elimination." #'els))]
    #:with thn-opt (if takes-true #'thn.opt #'thn)
    #:with els-opt (if takes-false #'els.opt #'els)
    ;; if the conditional has a known truth value, we can reveal this
    ;; we have to keep the test, in case it has side effects
    #:with opt
      (cond
        [(and (not takes-true) (not takes-false))
         (quasisyntax/loc/origin this-syntax #'kw
           (if #t tst.opt (begin thn-opt els-opt)))]
        [else
          (define/with-syntax tst-opt
            (cond
              [(and takes-true takes-false) #'tst.opt]
              [takes-true #'(begin tst.opt #t)]
              [takes-false #'(begin tst.opt #f)]))
          (quasisyntax/loc/origin this-syntax #'kw
            (if tst-opt thn-opt els-opt))]))
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
                          (bodies  (in-syntax #'(bodies ...))))
                 (if (dead-lambda-branch? formals)
                     ;; keep the clause (to have a case-lambda with the right arity)
                     ;; but not the body (to make the function smaller for inlining)
                     ;; TODO could do better, and keep a single clause per arity
                     (list formals #'(void)) ; return type doesn't matter, should never run
                     (cons formals (stx-map (optimize) bodies)))))
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
