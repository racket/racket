#lang racket/base
(require "match.rkt"
         "wrap.rkt"
         "import.rkt"
         "known.rkt"
         "find-known.rkt"
         "mutated-state.rkt"
         "literal.rkt")

(provide optimize
         optimize*)

;; Perform shallow optimizations. The `schemify` pass calls `optimize`
;; on each schemified form, which means that subexpressions of the
;; immediate expression have already been optimized.

(define (optimize v prim-knowns knowns imports mutated)
  (match v
    [`(if ,t ,e1 ,e2)
     (if (literal? t)
         (if (unwrap t) e1 e2)
         v)]
    [`(not ,t)
     (if (literal? t)
         `,(not (unwrap t))
         v)]
    [`(procedure? ,e)
     (define u (unwrap e))
     (cond
       [(symbol? u)
        (define k (find-known u prim-knowns knowns imports mutated))
        (if (known-procedure? k)
            '#t
            v)]
       [else v])]
    [`(procedure-arity-includes? ,e ,n)
     (define u (unwrap e))
     (define u-n (unwrap n))
     (cond
       [(and (symbol? u)
             (exact-nonnegative-integer? n))
        (define k (find-known u prim-knowns knowns imports mutated))
        (if (and (known-procedure? k)
                 (bitwise-bit-set? (known-procedure-arity-mask k) u-n))
            '#t
            v)]
       [else v])]
    [`,_
     (define u (unwrap v))
     (cond
       [(symbol? u)
        (define k (hash-ref-either knowns imports u))
        (cond
          [(and (known-literal? k)
                (simple-mutated-state? (hash-ref mutated u #f)))
           (known-literal-expr k)]
          ;; Note: we can't do `known-copy?` here, because a copy of
          ;; an imported or exported name will need to be schemified
          ;; to a different name
          [else v])]
       [else v])]))

;; ----------------------------------------

;; Recursive optimization --- useful when not fused with schemify,
;; such as for an initial optimization pass on a definition of a
;; function that can be inlined (where converting away
;; `variable-reference-from-unsafe?` is particularly important)

(define (optimize* v prim-knowns knowns imports mutated unsafe-mode?)
  (define (optimize* v)
    (define new-v
      (reannotate
       v
       (match v
         [`(lambda ,formal ,body ...)
          `(lambda ,formal ,@(optimize*-body body))]
         [`(case-lambda [,formalss ,bodys ...] ...)
          `(case-lambda ,@(for/list ([formals (in-list formalss)]
                                     [body (in-list bodys)])
                            `[,formals ,@(optimize*-body body)]))]
         [`(let-values . ,_) (optimize*-let v)]
         [`(letrec-values . ,_) (optimize*-let v)]
         [`(if ,tst ,thn ,els)
          `(if ,(optimize* tst) ,(optimize* thn) ,(optimize* els))]
         [`(with-continuation-mark ,key ,val ,body)
          `(with-continuation-mark ,(optimize* key) ,(optimize* val) ,(optimize* body))]
         [`(begin ,body ...)
          `(begin ,@(optimize*-body body))]
         [`(begin0 ,e ,body ...)
          `(begin0 ,(optimize* e) ,@(optimize*-body body))]
         [`(set! ,id ,rhs)
          `(set! ,id ,(optimize* rhs))]
         [`(variable-reference-from-unsafe? (#%variable-reference))
          unsafe-mode?]
         [`(#%variable-reference) v]
         [`(#%variable-reference ,id) v]
         [`(,rator ,exps ...)
          `(,(optimize* rator) ,@(optimize*-body exps))]
         [`,_ v])))
    (optimize new-v prim-knowns knowns imports mutated))

  (define (optimize*-body body)
    (for/list ([v (in-wrap-list body)])
      (optimize* v)))

  (define (optimize*-let v)
    (match v
      [`(,let-id ([,idss ,rhss] ...) ,body ...)
       `(,let-id ,(for/list ([ids (in-list idss)]
                             [rhs (in-list rhss)])
                    `[,ids ,(optimize* rhs)])
                 ,@(optimize*-body body))]))

  (optimize* v))
