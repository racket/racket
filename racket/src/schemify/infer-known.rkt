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
         "optimize.rkt"
         "single-valued.rkt"
         "lambda.rkt"
         "aim.rkt")

(provide infer-known
         can-improve-infer-known?
         lambda?)

;; For definitions, it's useful to infer `a-known-constant` to reflect
;; that the variable will get a value without referencing anything
;; too early. If `post-schemify?`, then `rhs` has been schemified.
(define (infer-known rhs defn id knowns prim-knowns imports mutated simples unsafe-mode? target
                     #:primitives [primitives #hasheq()] ; for `optimize-inline?` mode
                     #:optimize-inline? [optimize-inline? #f]
                     #:post-schemify? [post-schemify? #f])
  (let loop ([rhs rhs])
    (cond
      [(lambda? rhs)
       (define-values (lam inlinable?) (extract-lambda rhs))
       (define arity-mask (lambda-arity-mask lam))
       (cond
         [(and inlinable?
               (not post-schemify?)
               (or (can-inline? lam)
                   (wrap-property defn 'compiler-hint:cross-module-inline)))
          (let ([lam (if optimize-inline?
                         (optimize* lam prim-knowns primitives knowns imports mutated unsafe-mode?)
                         lam)])
            (known-procedure/can-inline arity-mask (if (and unsafe-mode? (not (aim? target 'cify)))
                                                       (add-begin-unsafe lam)
                                                       lam)))]
         [(single-valued-lambda? lam knowns prim-knowns imports mutated)
          (known-procedure/single-valued arity-mask)]
         [else
          (known-procedure arity-mask)])]
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
                      (known-copy? known)
                      (known-struct-constructor/need-imports? known)
                      (known-struct-predicate/need-imports? known)
                      (known-field-accessor/need-imports? known)
                      (known-field-mutator/need-imports? known))
                  (known-copy rhs)]
                 [else known]))]
         [defn a-known-constant]
         [(hash-ref imports u-rhs #f)
          ;; imported, but nothing known about it => could be mutable
          a-known-constant]
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
                  (simple? #:ordered? #t rhs prim-knowns knowns imports mutated simples unsafe-mode?))
             a-known-constant]
            [else #f])])])))

;; ----------------------------------------

(define (can-improve-infer-known? k)
  (or (not k)
      (eq? k a-known-constant)))

;; ----------------------------------------

(define (add-begin-unsafe lam)
  (reannotate
   lam
   (match lam
     [`(lambda ,args . ,body)
      `(lambda ,args (begin-unsafe . ,body))]
     [`(case-lambda [,argss . ,bodys] ...)
      `(case-lambda ,@(for/list ([args (in-list argss)]
                                 [body (in-list bodys)])
                        `[,args (begin-unsafe . ,body)]))]
     [`,_ lam])))
