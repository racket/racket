#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "simple.rkt"
         "parameter-result.rkt"
         "constructed-procedure.rkt"
         "literal.rkt"
         "inline.rkt"
         "mutated-state.rkt"
         "optimize.rkt")

(provide infer-known
         can-improve-infer-known?
         lambda?)

;; For definitions, it's useful to infer `a-known-constant` to reflect
;; that the variable will get a value without referencing anything
;; too early. If `post-schemify?`, then `rhs` has been schemified.
(define (infer-known rhs defn id knowns prim-knowns imports mutated simples unsafe-mode?
                     #:primitives [primitives #hasheq()] ; for `optimize-inline?` mode
                     #:optimize-inline? [optimize-inline? #f]
                     #:post-schemify? [post-schemify? #f])
  (let loop ([rhs rhs])
    (cond
      [(lambda? rhs)
       (define-values (lam inlinable?) (extract-lambda rhs))
       (define arity-mask (lambda-arity-mask lam))
       (if (and inlinable?
                (not post-schemify?)
                (or (can-inline? lam)
                    (wrap-property defn 'compiler-hint:cross-module-inline)))
           (let ([lam (if optimize-inline?
                          (optimize* lam prim-knowns primitives knowns imports mutated unsafe-mode?)
                          lam)])
             (known-procedure/can-inline arity-mask lam))
           (known-procedure arity-mask))]
      [(and (literal? rhs)
            (not (hash-ref mutated (unwrap id) #f)))
       (known-literal (unwrap-literal rhs))]
      [(and (symbol? (unwrap rhs))
            (not (hash-ref mutated (unwrap id) #f)))
       (define u-rhs (unwrap rhs))
       (cond
         [(hash-ref prim-knowns u-rhs #f)
          => (lambda (known) (known-copy u-rhs))]
         [(not (simple-mutated-state? (hash-ref mutated u-rhs #f)))
          ;; referenced variable is mutated, but not necessarily the target
          (and defn a-known-constant)]
         [(hash-ref-either knowns imports u-rhs)
          => (lambda (known)
               (cond
                 [(known-procedure/can-inline/need-imports? known)
                  ;; can't just return `known`, since that loses the connection to the import;
                  ;; the `inline-clone` function specially handles an identifier as the
                  ;; expression to inline
                  (known-procedure/can-inline (known-procedure-arity-mask known)
                                              rhs)]
                 [(or (known-procedure/can-inline? known)
                      (known-literal? known))
                  known]
                 [(or (not defn)
                      ;; can't just return `known`; like `known-procedure/can-inline/need-imports`,
                      ;; we'd lose track of the need to potentially propagate imports
                      (known-copy? known))
                  (known-copy rhs)]
                 [else known]))]
         [defn a-known-constant]
         [else (known-copy rhs)])]
      [(parameter-result? rhs prim-knowns knowns mutated)
       (known-procedure 3)]
      [(constructed-procedure-arity-mask rhs)
       => (lambda (m) (known-procedure m))]
      [else
       (match rhs
         [`(let-values () ,e)
          (loop e)]
         [`(begin ,e)
          (loop e)]
         [`,_
          (cond
            [(and defn
                  (simple? rhs prim-knowns knowns imports mutated simples))
             a-known-constant]
            [else #f])])])))

;; ----------------------------------------

(define (can-improve-infer-known? k)
  (or (not k)
      (eq? k a-known-constant)))

;; ----------------------------------------

;; Recognize forms that produce plain procedures; expression can be
;; pre- or post-schemify
(define (lambda? v #:simple? [simple? #f])
  (match v
    [`(lambda . ,_) #t]
    [`(case-lambda . ,_) #t]
    [`(let-values ([(,id) ,rhs]) ,body) (let-lambda? id rhs body #:simple? simple?)]
    [`(letrec-values ([(,id) ,rhs]) ,body) (let-lambda? id rhs body #:simple? simple?)]
    [`(let ([,id ,rhs]) ,body) (let-lambda? id rhs body #:simple? simple?)]
    [`(letrec* ([,id ,rhs]) ,body) (let-lambda? id rhs body #:simple? simple?)]
    [`(let-values ,_ ,body) (and (not simple?) (lambda? body))]
    [`(letrec-values ,_ ,body) (and (not simple?) (lambda? body))]
    [`(begin ,body) (lambda? body #:simple? simple?)]
    [`(values ,body) (lambda? body #:simple? simple?)]
    [`,_ #f]))

(define (let-lambda? id rhs body #:simple? simple?)
  (or (and (wrap-eq? id body) (lambda? rhs #:simple? simple?))
      (and (not simple?)
           (lambda? body #:simple? simple?))))

;; Extract procedure from a form on which `lambda?` produces true
(define (extract-lambda v)
  (match v
    [`(lambda . ,_) (values v #t)]
    [`(case-lambda . ,_) (values v #t)]
    [`(let-values ([(,id) ,rhs]) ,body) (extract-let-lambda #f id rhs body)]
    [`(letrec-values ([(,id) ,rhs]) ,body) (extract-let-lambda #t id rhs body)]
    [`(let ([,id ,rhs]) ,body) (extract-let-lambda #f id rhs body)]
    [`(letrec* ([,id ,rhs]) ,body) (extract-let-lambda #t id rhs body)]
    [`(let-values ,_ ,body) (extract-lambda* body)]
    [`(letrec-values ,_ ,body) (extract-lambda* body)]
    [`(let ,_ ,body) (extract-lambda* body)]
    [`(letrec* ,_ ,body) (extract-lambda* body)]
    [`(begin ,body) (extract-lambda body)]
    [`(values ,body) (extract-lambda body)]))

(define (extract-let-lambda rec? id rhs body)
  (if (wrap-eq? id body)
      (if rec?
          (extract-lambda* rhs)
          (extract-lambda rhs))
      (extract-lambda* body)))

(define (extract-lambda* v)
  (define-values (lam inlinable?) (extract-lambda v))
  (values lam #f))

(define (lambda-arity-mask v)
  (match v
    [`(lambda ,args . ,_) (args-arity-mask args)]
    [`(case-lambda [,argss . ,_] ...)
     (for/fold ([mask 0]) ([args (in-list argss)])
       (bitwise-ior mask (args-arity-mask args)))]))

(define (args-arity-mask args)
  (cond
    [(wrap-null? args) 1]
    [(wrap-pair? args)
     (arithmetic-shift (args-arity-mask (wrap-cdr args)) 1)]
    [else -1]))
