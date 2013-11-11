#lang racket/base

(require syntax/parse syntax/stx unstable/syntax unstable/sequence
         syntax/parse/experimental/template
         racket/list racket/dict racket/match racket/syntax
         racket/promise
         "../utils/utils.rkt"
         (for-template racket/base)
         (types numeric-tower utils type-table)
         (rep type-rep) (env mvar-env)
         (optimizer utils logging float-complex unboxed-tables))

(provide unboxed-let-opt-expr)

;; possibly replace bindings of complex numbers by bindings of their 2
;; components useful for intermediate results used more than once and for
;; loop variables
(define-syntax-class unboxed-let-opt-expr
  #:commit
  #:attributes (opt)
  (pattern :app-of-unboxed-let-opt-expr)
  (pattern (~var || (unboxed-let-opt-expr-internal #f))))

;; let loops expand to an application of a letrec-values
;; thus, the loop function technically escapes from the letrec, but it
;; escapes in the operator position of a call site we control (here)
;; we can extend unboxing
(define-syntax-class app-of-unboxed-let-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  #:attributes (opt)
  (pattern (#%plain-app
             (~and (letrec-values _ :id) ; sole element of the body is an id
                   (~var operator (unboxed-let-opt-expr-internal #t))
                   (letrec-values _ loop-fun:unboxed-fun)) .
             (~var call (float-complex-call-site-opt-expr #'loop-fun.unboxed-info)))
    #:do [(log-opt "unboxed call site" "Complex number unboxing")
          (log-optimization "unboxed let loop" arity-raising-opt-msg #'loop-fun)]
    #:with opt #`(let*-values (((op) operator.opt)) #,((attribute call.opt-app) #'op))))

;; does the bulk of the work
;; detects which let bindings can be unboxed, same for arguments of let-bound
;; functions
(define-syntax-class (unboxed-let-opt-expr-internal let-loop?)
  #:commit
  #:literal-sets (kernel-literals)
  #:attributes (opt)
  (pattern
   (letk:let-like-keyword ((~and clause (lhs rhs)) ...)
                          body:opt-expr ...)
   #:do [;; Ids that do not escape
         (define-syntax-class non-escaping-function-id
           (pattern fun-name:id
              #:when (not (or (escapes? #'fun-name #'(begin rhs ...) #f)
                              (escapes? #'fun-name #'(begin body ...) let-loop?)))))

         ;; Syntax classes for detecting clauses which can be unboxed.
         ;; This is split from actually unboxing so that variables defined in later clauses will be
         ;; unboxed in expressions in earlier clauses.

         ;; Clauses of form ((v) rhs), currently only supports 1 lhs var
         (define-syntax-class unboxable-let-clause?
           (pattern ((id:id) rhs:float-complex-expr)
             #:when (could-be-unboxed-in? #'id #'(begin body ...))))

         ;; Clauses that define functions that can be lifted
         (define-syntax-class unboxable-fun-clause?
           (pattern (~and ((_:non-escaping-function-id) body:expr)
                          _:unboxable-fun-definition)))

         ;; Bindings are delayed so that all clauses are matched before optimizations happen.
         ;; This ensures that unboxable variables defined in later clauses are detected before
         ;; optimization starts.
         (define-syntax-class unboxed-clause
            #:attributes (bindings)
            (pattern v:unboxable-let-clause?
              #:with (real-binding imag-binding) (binding-names)
              #:do [(add-unboxed-var! #'v.id #'real-binding #'imag-binding)]
              #:attr bindings
                (delay
                  (syntax-parse #'v
                    [((id:id) c:unboxed-float-complex-opt-expr)
                     #:do [(log-opt "unboxed let bindings" arity-raising-opt-msg)]
                     #'(c.bindings ...
                        ((real-binding) c.real-binding)
                        ((imag-binding) c.imag-binding))])))
            (pattern v:unboxable-fun-clause?
              #:attr bindings
                (delay
                  (syntax-parse #'v
                    [c:unbox-fun-clause
                     #'(c.bindings ...)])))
            (pattern v
              #:attr bindings
                (delay
                  (syntax-parse #'v
                    [(vs rhs:opt-expr)
                     #'((vs rhs.opt))]))))
         (define-syntax-class unboxed-clauses
           #:attributes ([bindings 1])
           (pattern (clauses:unboxed-clause ...)
             #:with (bindings ...) (template ((?@ . clauses.bindings) ...))))]

   #:with opt
     (syntax-parse #'(clause ...)
      [clauses:unboxed-clauses
       (quasisyntax/loc/origin
          this-syntax #'letk.kw
          (letk.key ... (clauses.bindings ...) body.opt ...))])))



(define-syntax-class constant-var
  #:attributes ()
  (pattern v:id
    #:when (not (is-var-mutated? #'v))))

;; A function definition is unboxable when the following are true:
;; 1. Its binding is never mutated.
;; 2. Its type has no keyword arguments or rest/drest arguments.
;; 3. At least one of the arguments is of the type FloatComplex and used in a manner which benefits
;; from unboxing.
(define-syntax-class unboxable-fun-definition
  #:attributes ()
  #:literal-sets (kernel-literals)
  (pattern ((fun-name:constant-var) (~and fun (#%plain-lambda params body ...)))
    #:do [(define doms
            (match (type-of #'fun)
              [(tc-result1: (Function: (list (arr: doms rngs
                                                   (and rests #f)
                                                   (and drests #f)
                                                   (and kws '())))))
               doms]
              [_ #f]))]
    #:when doms
    #:do [
       ;; at least 1 argument has to be of type float-complex
       ;; and can be unboxed
       (define unboxed-args
              (for/list ([param (in-syntax #'params)]
                         [dom doms]
                         [i (in-naturals)])
                (cond
                  [(and (equal? dom -FloatComplex)
                        (could-be-unboxed-in?
                          param
                          #'(begin body ...)))
                   ;; we can unbox
                   (log-optimization "unboxed var -> table" arity-raising-opt-msg param)
                   #t]
                  [else #f])))]
    #:when
       ;; can we unbox anything?
       (and (member #t unboxed-args)
            ;; if so, add to the table of functions with
            ;; unboxed params, so we can modify its call
            ;; sites, its body and its header
            (add-unboxed-fun! #'fun-name unboxed-args))))


(define-splicing-syntax-class let-like-keyword
  #:commit
  #:literal-sets (kernel-literals)
  #:attributes ([key 1] kw)
  (pattern (~and kw let-values)
           ;; we need let*-values because we bind intermediate unboxed results,
           ;; and the bindings for the final results refer to them
           #:with (key ...) #'(let*-values))
  (pattern (~and kw letrec-values)
           #:with (key ...) #'(kw))
  (pattern (~seq (~and kw letrec-syntaxes+values) stx-bindings)
           #:with (key ...) #'(kw stx-bindings)))


(define (direct-child-of? v exp)
  (for/or ((x (in-syntax exp)))
    (and (identifier? x)
         (free-identifier=? x v))))

;; if a variable is used at least once in complex arithmetic operations,
;; it's worth unboxing
(define (could-be-unboxed-in? v exp)

  ;; if v is a direct child of exp, that means it's used in a boxed
  ;; fashion, and is not safe to unboxed
  ;; if not, recur on the subforms
  (define (look-at exp)
    (ormap rec (syntax->list exp)))

  (define (rec exp)
    (syntax-parse exp
      ;; can be used in a complex arithmetic expr, can be a direct child
      [(~and (~not :id) exp:float-complex-arith-expr)
       (or (direct-child-of? v #'exp)
           (ormap rec (syntax->list #'exp)))]
      ;; if the variable gets rebound to something else, we look for unboxing
      ;; opportunities for the new variable too
      ;; this case happens in the expansion of the for macros, so we care
      [(l:let-like-keyword ([ids e-rhs:expr] ...) e-body:expr ...)
       (define rebindings
         (filter (lambda (x) x)
                 (stx-map (syntax-parser
                           [((id) rhs:identifier)
                            #:when (free-identifier=? v #'rhs)
                            #'id]
                           [_ #f])
                          #'((ids e-rhs) ...))))
       (or (look-at #'(e-rhs ... e-body ...))
           (for/or ((x (in-list rebindings)))
             (could-be-unboxed-in? x exp)))]

      ;; recur down
      [e:kernel-expression
       (look-at #'(e.sub-exprs ...))]))

  ;; of course, if the var is mutated, we can't do anything
  (and (not (is-var-mutated? v))
       (rec exp)))

;; Very simple escape analysis for functions.
;; If a function is used in a non-operator position, we consider it escapes.
;; If it doesn't escape, we may be able to pass its float complex args unboxed.
;; If we are in a let loop, don't consider functions that escape by being the
;; sole thing in the let's body as escaping, since they would only escape to
;; a call site that we control, which is fine.
(define (escapes? v exp let-loop?)

  (define (look-at exp)
    (or (direct-child-of? v exp)
        (ormap rec (syntax->list exp))))

  (define (rec exp)
    (syntax-parse exp
      #:literal-sets (kernel-literals)

      [(#%plain-app rator:expr rands:expr ...)
       (or (direct-child-of? v #'(rands ...)) ; used as an argument, escapes
           (ormap rec (syntax->list #'(rator rands ...))))]
      [e:kernel-expression
       (look-at #'(e.sub-exprs ...))]))


  ;; if the given var is the _only_ element of the body and we're in a
  ;; let loop, we let it slide
  (and (not (and let-loop?
                 (syntax-parse exp
                   #:literal-sets (kernel-literals)
                   ;; the body gets wrapped in a begin before it's sent here
                   [(begin i:identifier)
                    (free-identifier=? #'i v)]
                   [_ #f])))
       (rec exp)))

;; let clause whose rhs is a function with some float complex arguments
;; these arguments may be unboxed
;; the new function will have all the unboxed arguments first, then all the
;; boxed
(define-syntax-class unbox-fun-clause
  #:commit
  #:attributes ([bindings 1])
  (pattern ((fun:unboxed-fun) (#%plain-lambda params body:opt-expr ...))
    #:with (real-params ...)
    (stx-map (lambda (x) (generate-temporary "unboxed-real-")) #'(fun.unboxed ...))
    #:with (imag-params ...)
    (stx-map (lambda (x) (generate-temporary "unboxed-imag-")) #'(fun.unboxed ...))
    #:do [(log-optimization "fun -> unboxed fun" arity-raising-opt-msg #'fun)]
    #:with (bindings ...)
    ;; add unboxed parameters to the unboxed vars table
    (let ((to-unbox (syntax->datum #'(fun.unboxed ...))))
      (for ([index (in-list to-unbox)]
            [real-part (in-syntax #'(real-params ...))]
            [imag-part (in-syntax #'(imag-params ...))])
        (add-unboxed-var! (list-ref (syntax->list #'params) index) real-part imag-part))
      (define boxed
        (for/list ([param (in-syntax #'params)]
                   [i (in-naturals)]
                   #:unless (memq i to-unbox))
          param))
      ;; real parts of unboxed parameters go first, then all
      ;; imag parts, then boxed occurrences of unboxed
      ;; parameters will be inserted when optimizing the body
      #`(((fun) (#%plain-lambda
                  (real-params ... imag-params ... #,@(reverse boxed))
                  body.opt ...))))))

