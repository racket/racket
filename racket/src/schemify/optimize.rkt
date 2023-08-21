#lang racket/base
(require "match.rkt"
         "wrap.rkt"
         "import.rkt"
         "known.rkt"
         "find-known.rkt"
         "mutated-state.rkt"
         "literal.rkt"
         "lambda.rkt"
         "simple.rkt"
         "fold.rkt")

(provide optimize
         optimize*)

;; Perform shallow optimizations. The `schemify` pass calls `optimize`
;; on each schemified form, which means that subexpressions of the
;; immediate expression have already been optimized.

(define (optimize v prim-knowns primitives knowns imports mutated)
  (match v
    [`(if ,t ,e1 ,e2)
     (if (literal? t)
         (if (unwrap t) e1 e2)
         v)]
    [`(begin (quote ,_) ,e . ,es) ; avoid `begin` that looks like it provides a name
     (optimize (reannotate v `(begin ,e . ,es)) prim-knowns primitives knowns imports mutated)]
    [`(not ,t)
     (if (literal? t)
         `,(not (unwrap t))
         v)]
    [`(procedure? ,e)
     (cond
       [(lambda? e)
        (define-values (lam inlinable?) (extract-lambda e))
        (if inlinable?
            #t
            `(begin ,e #t))]
       [else
        (define u (unwrap e))
        (cond
          [(symbol? u)
           (define k (find-known u prim-knowns knowns imports mutated))
           (if (known-procedure? k)
               '#t
               v)]
          [else v])])]
    [`(procedure-arity-includes? ,e ,n . ,opt)
     (define u-n (unwrap n))
     (cond
       [(and (exact-nonnegative-integer? n)
             (or (null? opt)
                 (and (null? (cdr opt))
                      (literal? (car opt)))))
        (cond
          [(lambda? e)
           (define-values (lam inlinable?) (extract-lambda e))
           (define inc? (bitwise-bit-set? (lambda-arity-mask lam) n))
           (if inlinable?
               inc?
               `(begin ,e ,inc?))]
          [else
           (define u (unwrap e))
           (cond
             [(symbol? u)
              (define k (find-known u prim-knowns knowns imports mutated))
              (if (known-procedure? k)
                  (bitwise-bit-set? (known-procedure-arity-mask k) u-n)
                  v)]
             [else v])])]
       [else v])]
    [`(procedure-specialize ,e)
     (if (lambda? e) e v)]
    [`(,rator . ,rands)
     (define u-rator (unwrap rator))
     (define k (and (symbol? u-rator) (hash-ref prim-knowns u-rator #f)))
     (cond
       [(and k
             (or (known-procedure/folding? k)
                 (known-procedure/pure/folding? k)
                 (known-procedure/then-pure/folding-unsafe? k)
                 (known-procedure/has-unsafe/folding? k))
             (for/and ([rand (in-list rands)])
               (literal? rand))
             (try-fold-primitive u-rator k rands prim-knowns primitives))
        => (lambda (l) (car l))]
       [else v])]
    [`,_
     (define u (unwrap v))
     (cond
       [(symbol? u)
        (define k (hash-ref-either knowns imports u))
        (cond
          [(and (known-literal? k)
                (simple-mutated-state? (hash-ref mutated u #f)))
           (wrap-literal (known-literal-value k))]
          ;; Note: we can't do `known-copy?` here, because a copy of
          ;; an imported or exported name will need to be schemified
          ;; to a different name
          [else v])]
       [else v])]))

;; ----------------------------------------

;; Recursive optimization on pre-schemified --- useful when not fused with
;; schemify, such as for an initial optimization pass on a definition of a
;; function that can be inlined (where converting away
;; `variable-reference-from-unsafe?` is particularly important)

(define (optimize* v prim-knowns primitives knowns imports mutated unsafe-mode?)
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
         [`(begin-unsafe ,body ...)
          `(begin-unsafe ,@(optimize*-body body))]
         [`(begin0 ,e ,body ...)
          `(begin0 ,(optimize* e) ,@(optimize*-body body))]
         [`(set! ,id ,rhs)
          `(set! ,id ,(optimize* rhs))]
         [`(variable-reference-from-unsafe? (#%variable-reference))
          unsafe-mode?]
         [`(#%variable-reference) v]
         [`(#%variable-reference ,id) v]
         [`(quote ,_) v]
         [`(,rator ,exps ...)
          `(,(optimize* rator) ,@(optimize*-body exps))]
         [`,_ v])))
    (optimize new-v prim-knowns primitives knowns imports mutated))

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
