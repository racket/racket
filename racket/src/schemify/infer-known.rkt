#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "simple.rkt"
         "pthread-parameter.rkt"
         "literal.rkt"
         "inline.rkt"
         "mutated-state.rkt"
         "optimize.rkt")

(provide infer-known
         lambda?)

;; For definitions, it's useful to infer `a-known-constant` to reflect
;; that the variable will get a value without referencing anything
;; too early.
(define (infer-known rhs defn rec? id knowns prim-knowns imports mutated unsafe-mode?
                     #:optimize-inline? [optimize-inline? #f])
  (cond
    [(lambda? rhs)
     (define-values (lam inlinable?) (extract-lambda rhs))
     (define arity-mask (lambda-arity-mask lam))
     (if (and inlinable?
              (or (can-inline? lam)
                  (wrap-property defn 'compiler-hint:cross-module-inline)))
         (let ([lam (if optimize-inline?
                        (optimize* lam prim-knowns knowns imports mutated unsafe-mode?)
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
               [(not defn)
                (known-copy rhs)]
               [else known]))]
       [defn a-known-constant]
       [else (known-copy rhs)])]
    [(pthread-parameter? rhs prim-knowns knowns mutated)
     (known-procedure 3)]
    [(and defn
          (simple? rhs prim-knowns knowns imports mutated))
     a-known-constant]
    [else #f]))
  
;; ----------------------------------------

;; Recognize forms that produce plain procedures
(define (lambda? v #:simple? [simple? #f])
  (match v
    [`(lambda . ,_) #t]
    [`(case-lambda . ,_) #t]
    [`(let-values ([(,id) ,rhs]) ,body) (or (and (wrap-eq? id body) (lambda? rhs))
                                            (lambda? body))]
    [`(letrec-values ([(,id) ,rhs]) ,body) (or (and (wrap-eq? id body) (lambda? rhs))
                                               (lambda? body))]
    [`(let-values ,_ ,body) (and (not simple?) (lambda? body))]
    [`(letrec-values ,_ ,body) (and (not simple?) (lambda? body))]
    [`(begin ,body) (lambda? body)]
    [`(values ,body) (lambda? body)]
    [`,_ #f]))

;; Extract procedure from from forms that produce plain procedures
(define (extract-lambda v)
  (match v
    [`(lambda . ,_) (values v #t)]
    [`(case-lambda . ,_) (values v #t)]
    [`(let-values ([(,id) ,rhs]) ,body)
     (if (wrap-eq? id body)
         (extract-lambda rhs)
         (extract-lambda* body))]
    [`(letrec-values ([(,id) ,rhs]) ,body)
     (if (wrap-eq? id body)
         (extract-lambda* rhs)
         (extract-lambda* body))]
    [`(let-values ,_ ,body) (extract-lambda* body)]
    [`(letrec-values ,_ ,body) (extract-lambda* body)]
    [`(begin ,body) (extract-lambda body)]
    [`(values ,body) (extract-lambda body)]))

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
