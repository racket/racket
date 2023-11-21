#lang racket/base
(require "match.rkt"
         "wrap.rkt"
         "import.rkt"
         "known.rkt"
         "find-known.rkt"
         "mutated-state.rkt"
         "literal.rkt"
         "lambda.rkt"
         "fold.rkt"
         "ffi-static-core.rkt"
         "unwrap-let.rkt")

(provide optimize
         optimize*)

;; Perform shallow optimizations. The `schemify` pass calls `optimize`
;; on each schemified form, which means that subexpressions of the
;; immediate expression have already been optimized.

(define (optimize v prim-knowns primitives knowns imports mutated target compiler-query)
  (match (unwrap-let v)
    [`(if ,t ,e1 ,e2)
     (if (literal? t)
         (if (unwrap t) e1 e2)
         v)]
    [`(begin (quote ,_) ,e . ,es) ; avoid `begin` that looks like it provides a name
     (optimize (reannotate v `(begin ,e . ,es)) prim-knowns primitives knowns imports mutated target compiler-query)]
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
    [`(ffi-maybe-call-and-callback-core ,must-at
                                        ,abi
                                        ,varargs-after
                                        ,blocking?
                                        ,async-apply
                                        ,result-type
                                        . ,arg-types)
     ;; This case is aided by an ad hoc ffi-maybe-call-and-callback-core
     (or (and (eq? target 'compile)
              (or (make-ffi-static-core arg-types result-type
                                        abi varargs-after blocking? async-apply
                                        prim-knowns primitives knowns imports mutated)
                  (and (unwrap must-at)
                       (error 'compile "unable to generate foreign function statically: ~s"
                              (match must-at
                                [`(quote ,e) e]
                                [`,_ must-at])))))
         v)]
    [`(system-type . ,_)
     (match v
       [`(system-type 'vm)
        '(quote chez-scheme)]
       [`(system-type) (let ([sym (compiler-query '(system-type))])
                         (if sym
                             `(quote ,sym)
                             v))]
       [`(system-type 'word) (let ([n (compiler-query '(foreign-sizeof 'void*))])
                               (cond
                                 [(eqv? n 8) 64]
                                 [(eqv? n 4) 32]
                                 [else v]))]
       [`,_ v])]
    [`(compiler-sizeof ',arg)
     (define scheme-arg (let loop ([arg arg])
                          (case (unwrap arg)
                            [(int char wchar float double short long) arg]
                            [else
                             (define u (unwrap-list arg))
                             (and (list? u)
                                  (= 2 (length u))
                                  (cond
                                    [(and (eq? 'long (unwrap (car u)))
                                          (eq? 'long (unwrap (cadr u))))
                                     'long-long]
                                    [(and (eq? (unwrap (cadr u)) '*)
                                          (let ([a (unwrap (car u))])
                                            (and (symbol? a)
                                                 (or (eq? 'void a)
                                                     (loop a)))))
                                     'void*]
                                    [else #f]))])))
     (or (and scheme-arg
              (let ([opt (compiler-query `(foreign-sizeof ',scheme-arg))])
                (or (and (integer? opt) opt))))
         v)]
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

(define (optimize* v prim-knowns primitives knowns imports mutated unsafe-mode? target compiler-query)
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
    (optimize new-v prim-knowns primitives knowns imports mutated target (lambda (v) #f)))

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
