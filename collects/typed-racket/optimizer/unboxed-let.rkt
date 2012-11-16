#lang racket/base

(require syntax/parse unstable/syntax
         racket/list racket/dict racket/match
         "../utils/utils.rkt"
         "../utils/tc-utils.rkt"
         (for-template racket/base)
         (types numeric-tower utils type-table)
         (rep type-rep) (env mvar-env)
         (optimizer utils logging float-complex))

(provide unboxed-let-opt-expr)

;; possibly replace bindings of complex numbers by bindings of their 2
;; components useful for intermediate results used more than once and for
;; loop variables
(define-syntax-class unboxed-let-opt-expr
  #:commit
  (pattern e:app-of-unboxed-let-opt-expr
           #:with opt #'e.opt)
  (pattern (~var e (unboxed-let-opt-expr-internal #f))
           #:with opt #'e.opt))

;; let loops expand to an application of a letrec-values
;; thus, the loop function technically escapes from the letrec, but it
;; escapes in the operator position of a call site we control (here)
;; we can extend unboxing
(define-syntax-class app-of-unboxed-let-opt-expr
  #:commit
  #:literal-sets (kernel-literals)
  (pattern (#%plain-app
            (~and let-e ((~literal letrec-values)
                         bindings
                         loop-fun:id)) ; sole element of the body
            args:expr ...)
           #:with (~var operator (unboxed-let-opt-expr-internal #t)) #'let-e
           #:with unboxed-info (dict-ref unboxed-funs-table #'loop-fun #f)
           #:when (syntax->datum #'unboxed-info)
           #:with (~var e* (float-complex-call-site-opt-expr
                            #'unboxed-info #'operator.opt))
           this-syntax
           #:with opt
           (begin (log-optimization "unboxed let loop"
                                    arity-raising-opt-msg
                                    #'loop-fun)
                  #'e*.opt)))

;; does the bulk of the work
;; detects which let bindings can be unboxed, same for arguments of let-bound
;; functions
(define-syntax-class (unboxed-let-opt-expr-internal let-loop?)
  #:commit
  #:literal-sets (kernel-literals)
  (pattern
   (letk:let-like-keyword ((~and clause (lhs rhs ...)) ...)
                          body:expr ...)
   ;; we look for bindings of complexes that are not mutated and only
   ;; used in positions where we would unbox them
   ;; these are candidates for unboxing
   #:with ((candidates ...) (function-candidates ...) (others ...))
   (let*-values
       (((candidates rest)
         ;; clauses of form ((v) rhs), currently only supports 1 lhs var
         (partition
          (lambda (p)
            (and (subtypeof? (cadr p) -FloatComplex)
                 (could-be-unboxed-in? (car (syntax-e (car p)))
                                       #'(begin body ...))))
          (syntax-map syntax->list #'(clause ...))))
        ((function-candidates others)
         ;; extract function bindings that have float-complex arguments
         ;; we may be able to pass arguments unboxed
         ;; this covers loop variables
         (partition
          (lambda (p)
            (and
             ;; typed racket introduces let-values that bind no values
             ;; we can't optimize these
             (not (null? (syntax-e (car p))))
             (let ((fun-name (car (syntax-e (car p)))))
               (and
                ;; if the function escapes, we can't change its interface
                (not (is-var-mutated? fun-name))
                (not (escapes? fun-name #'(begin rhs ... ...) #f))
                (not (escapes? fun-name #'(begin body ...) let-loop?))
                (match (type-of (cadr p)) ; rhs, we want a lambda
                  [(tc-result1: (Function: (list (arr: doms rngs
                                                       (and rests #f)
                                                       (and drests #f)
                                                       (and kws '())))))
                   ;; at least 1 argument has to be of type float-complex
                   ;; and can be unboxed
                   (syntax-parse (cadr p)
                     [((~literal #%plain-lambda) params body ...)
                      ;; keep track of the param # of each param that can be
                      ;; unboxed
                      (let loop ((unboxed '())
                                 (boxed   '())
                                 (i        0)
                                 (params   (syntax->list #'params))
                                 (doms     doms))
                        (cond [(null? params)
                               ;; done. can we unbox anything?
                               (and (> (length unboxed) 0)
                                    ;; if so, add to the table of functions with
                                    ;; unboxed params, so we can modify its call
                                    ;; sites, its body and its header
                                    (dict-set! unboxed-funs-table fun-name
                                               (list (reverse unboxed)
                                                     (reverse boxed))))]
                              [(and (equal? (car doms) -FloatComplex)
                                    (could-be-unboxed-in?
                                     (car params) #'(begin body ...)))
                               ;; we can unbox
                               (log-optimization "unboxed var -> table"
                                                 arity-raising-opt-msg
                                                 (car params))
                               (loop (cons i unboxed) boxed
                                     (add1 i) (cdr params) (cdr doms))]
                              [else ; can't unbox
                               (loop unboxed (cons i boxed)
                                     (add1 i) (cdr params) (cdr doms))]))]
                     [_ #f])]
                  [_ #f])))))
          rest)))
     (list candidates function-candidates others))
   #:with (opt-candidates:unboxed-let-clause ...) #'(candidates ...)
   #:with (opt-functions:unboxed-fun-clause ...) #'(function-candidates ...)
   #:with (opt-others:opt-let-clause ...) #'(others ...)
   #:with opt
   (begin (when (not (null? (syntax->list #'(opt-candidates.id ...))))
            ;; only log when we actually optimize
            (log-optimization "unboxed let bindings"
                              arity-raising-opt-msg
                              this-syntax))
          ;; add the unboxed bindings to the table, for them to be used by
          ;; further optimizations
          (for ((v (in-list (syntax->list
                             #'(opt-candidates.id ...))))
                (r (in-list (syntax->list
                             #'(opt-candidates.real-binding ...))))
                (i (in-list (syntax->list
                             #'(opt-candidates.imag-binding ...)))))
            (dict-set! unboxed-vars-table v (list r i v)))
          ;; in the case where no bindings are unboxed, we create a let
          ;; that is equivalent to the original, but with all parts
          ;; optimized
          (quasisyntax/loc/origin
           this-syntax #'letk.kw
           (letk.key ...
                     (opt-functions.res ...
                      opt-others.res ...
                      opt-candidates.bindings ... ...)
                     #,@(syntax-map (optimize) #'(body ...)))))))

(define-splicing-syntax-class let-like-keyword
  #:commit
  #:literal-sets (kernel-literals)
  #:attributes ([key 1] kw)
  (pattern (~and kw (~literal let-values))
           ;; we need let*-values because we bind intermediate unboxed results,
           ;; and the bindings for the final results refer to them
           #:with (key ...) #'(let*-values))
  (pattern (~and kw (~literal letrec-values))
           #:with (key ...) #'(kw))
  (pattern (~seq (~and kw (~literal letrec-syntaxes+values)) stx-bindings)
           #:with (key ...) #'(kw stx-bindings)))


(define (direct-child-of? v exp)
  (ormap (lambda (x) (and (identifier? x) (free-identifier=? x v)))
         (syntax->list exp)))

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
      #:literal-sets (kernel-literals)

      ;; can be used in a complex arithmetic expr, can be a direct child
      [exp:float-complex-arith-opt-expr
       #:when (not (identifier? #'exp))
       (or (direct-child-of? v #'exp)
           (ormap rec (syntax->list #'exp)))]
      ;; if the variable gets rebound to something else, we look for unboxing
      ;; opportunities for the new variable too
      ;; this case happens in the expansion of the for macros, so we care
      [(l:let-like-keyword
        ([ids e-rhs:expr] ...) e-body:expr ...)
       #:with rebindings
       (filter (lambda (x) x)
               (syntax-map (syntax-parser
                            [((id) rhs)
                             #:when (and (identifier? #'rhs)
                                         (free-identifier=? v #'rhs))
                             #'id]
                            [_ #f])
                           #'((ids e-rhs) ...)))
       (or (look-at #'(e-rhs ... e-body ...))
           (ormap (lambda (x) (could-be-unboxed-in? x exp))
                  (syntax->list #'rebindings)))]

      ;; recur down
      [((~and op (~or (~literal #%plain-lambda) (~literal define-values)))
        formals e:expr ...)
       (look-at #'(e ...))]
      [(case-lambda [formals e:expr ...] ...)
       (look-at #'(e ... ...))]
      [(kw:identifier expr ...)
       #:when (ormap (lambda (k) (free-identifier=? k #'kw))
                     (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%app
                           #'#%expression #'#%variable-reference
                           #'with-continuation-mark))
       (look-at #'(expr ...))]

      ;; not used, not worth unboxing
      [_ #f]))

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

      [((~or (~literal #%plain-app) (~literal #%app))
        rator:expr rands:expr ...)
       (or (direct-child-of? v #'(rands ...)) ; used as an argument, escapes
           (ormap rec (syntax->list #'(rator rands ...))))]

      [((~and op (~or (~literal #%plain-lambda) (~literal define-values)))
        formals e:expr ...)
       (look-at #'(e ...))]
      [(case-lambda [formals e:expr ...] ...)
       (look-at #'(e ... ...))]
      [((~or (~literal let-values) (~literal letrec-values))
        ([ids e-rhs:expr] ...) e-body:expr ...)
       (look-at #'(e-rhs ... e-body ...))]
      [(letrec-syntaxes+values stx-bindings
                               ([(ids ...) e-rhs:expr] ...)
                               e-body:expr ...)
       (look-at #'(e-rhs ... e-body ...))]
      [(kw:identifier expr ...)
       #:when (ormap (lambda (k) (free-identifier=? k #'kw))
                     (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%app
                           #'#%expression #'#%variable-reference
                           #'with-continuation-mark))
       (look-at #'(expr ...))]

      ;; does not escape
      [_ #f]))

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

;; let clause whose rhs is going to be unboxed (turned into multiple bindings)
(define-syntax-class unboxed-let-clause
  #:commit
  (pattern ((v:id) rhs:unboxed-float-complex-opt-expr)
           #:with id #'v
           #:with real-binding #'rhs.real-binding
           #:with imag-binding #'rhs.imag-binding
           #:with (bindings ...) #'(rhs.bindings ...)))

;; let clause whose rhs is a function with some float complex arguments
;; these arguments may be unboxed
;; the new function will have all the unboxed arguments first, then all the
;; boxed
(define-syntax-class unboxed-fun-clause
  #:commit
  (pattern ((v:id) (#%plain-lambda params body:expr ...))
           #:with id #'v
           #:with unboxed-info (dict-ref unboxed-funs-table #'v #f)
           #:when (syntax->datum #'unboxed-info)
           ;; partition of the arguments
           #:with ((to-unbox ...) (boxed ...)) #'unboxed-info
           #:with (real-params ...)
           (syntax-map (lambda (x) (unboxed-gensym "unboxed-real-"))
                       #'(to-unbox ...))
           #:with (imag-params ...)
           (syntax-map (lambda (x) (unboxed-gensym "unboxed-imag-"))
                       #'(to-unbox ...))
           #:with res
           (begin
             (log-optimization "fun -> unboxed fun" arity-raising-opt-msg #'v)
             ;; add unboxed parameters to the unboxed vars table
             (let ((to-unbox (syntax-map syntax->datum #'(to-unbox ...))))
               (let loop ((params     (syntax->list #'params))
                          (i          0)
                          (real-parts (syntax->list #'(real-params ...)))
                          (imag-parts (syntax->list #'(imag-params ...)))
                          (boxed      '()))
                 (cond [(null? params) ; done, create the new clause
                        ;; real parts of unboxed parameters go first, then all
                        ;; imag parts, then boxed occurrences of unboxed
                        ;; parameters will be inserted when optimizing the body
                        #`((v) (#%plain-lambda
                                (real-params ... imag-params ...
                                 #,@(reverse boxed))
                                #,@(syntax-map (optimize) #'(body ...))))]

                       [(memq i to-unbox)
                        ;; we unbox the current param, add to the table
                        (dict-set! unboxed-vars-table (car params)
                                   (list (car real-parts)
                                         (car imag-parts)
                                         (car params)))
                        (loop (cdr params) (add1 i)
                              (cdr real-parts) (cdr imag-parts)
                              boxed)]
                       [else ; that param stays boxed, keep going
                        (loop (cdr params) (add1 i)
                              real-parts imag-parts
                              (cons (car params) boxed))]))))))

(define-syntax-class opt-let-clause
  #:commit
  (pattern (vs rhs:expr)
           #:with res #`(vs #,((optimize) #'rhs))))
