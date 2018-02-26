#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "known.rkt"
         "import.rkt"
         "export.rkt")

(provide init-inline-fuel
         can-inline?
         inline-clone
         known-inline->export-known)

(define inline-base 3)
(define inline-factor 3)
(define init-inline-fuel 8)

(define (can-inline? v)
  (match v
    [`(lambda ,args . ,bodys)
     (smaller-than? bodys (+ inline-base (* inline-factor (args-size args))))]
    [`(case-lambda [,argss . ,bodyss] ...)
     (for/and ([args (in-list argss)]
               [bodys (in-list bodyss)])
       (smaller-than? bodys (+ inline-base (* inline-factor (args-size args)))))]
    [`,_ #f]))

(define (args-size args)
  (cond
    [(wrap-pair? args) (+ 1 (args-size (wrap-cdr args)))]
    [else 1]))

(define (smaller-than? v size)
  (positive?
   (let loop ([v v] [size size])
     (cond
       [(zero? size) 0]
       [(wrap-pair? v)
        (loop (wrap-cdr v) (loop (wrap-car v) size))]
       [else (sub1 size)]))))

;; ----------------------------------------

;; All binding identifiers in a clone must be fresh to stay consistent
;; with the unique-variable invariant of expanded/schemified form.

(define (inline-clone k im add-import! mutated imports reannotate)
  (define env (if (known-procedure/can-inline/need-imports? k)
                  ;; The `needed->env` setup can fail if a needed
                  ;; import cannot be made available:
                  (needed->env (known-procedure/can-inline/need-imports-needed k)
                               add-import!
                               im)
                  '()))
  (and
   env
   (match (known-procedure/can-inline-expr k)
     [`(lambda ,args . ,bodys)
      (define-values (new-args new-env) (clone-args args env mutated))
      `(lambda ,new-args . ,(clone-body bodys new-env mutated reannotate))]
     [`(case-lambda [,argss . ,bodyss] ...)
      `(case-lambda ,@(for/list ([args (in-list argss)]
                                 [bodys (in-list bodyss)])
                        (define-values (new-args new-env) (clone-args args env mutated))
                        `[,new-args . ,(clone-body bodys new-env mutated reannotate)]))]
     [`,id
      ;; We expect `id` to refer to an imported variable, where inlining the
      ;; imported variable will need to pull from there
      (cond
        [(hash-ref imports (unwrap id) #f)
         => (lambda (im)
              (define i-k (import-lookup im))
              (and (known-procedure/can-inline? i-k)
                   (inline-clone i-k im add-import! mutated imports reannotate)))]
        [else #f])])))

;; Build a mapping from ids in the expr to imports into the current
;; linklet, where `add-import!` arranges for the import to exist as
;; needed and if possible. The result is #f if some import cannot be
;; made available.
(define (needed->env needed add-import! im)
  (for/fold ([env '()]) ([need (in-list needed)])
    (and env
         (let ([id (add-import! im (cadr need) (cddr need))])
           (and id
                (cons (cons (car need) id) env))))))

(define (clone-args args base-env mutated)
  (define env
    (let loop ([args args])
      (cond
        [(wrap-null? args) base-env]
        [(wrap-pair? args)
         (define u (unwrap (wrap-car args)))
         (define g (gensym u))
         (define m (hash-ref mutated u #f))
         (when m
           (hash-set! mutated g m))
         (cons (cons u g)
               (loop (wrap-cdr args)))]
        [else
         (define u (unwrap args))
         (list (cons u (gensym u)))])))
  (values (let loop ([args args] [env env])
            (cond
              [(wrap-null? args) '()]
              [(wrap-pair? args)
               (define u (unwrap (wrap-car args)))
               (cons (cdr (car env))
                     (loop (wrap-cdr args) (cdr env)))]
              [else
               (cdr (car env))]))
          env))

(define (clone-body l env mutated reannotate)
  (for/list ([e (in-wrap-list l)])
    (clone-expr e env mutated reannotate)))

(define (clone-let v env mutated reannotate)
  (match v
    [`(,let-id ([,idss ,rhss] ...) ,bodys ...)
     (define-values (rev-new-idss new-env)
       (for/fold ([rev-new-idss null] [env env]) ([ids (in-list idss)])
         (define-values (new-ids new-env) (clone-args ids env mutated))
         (values (cons new-ids rev-new-idss) new-env)))
     `(,let-id ,(for/list ([ids (in-list (reverse rev-new-idss))]
                           [rhs (in-list rhss)])
                  `[,ids ,(clone-expr rhs new-env mutated reannotate)])
               . ,(clone-body bodys new-env mutated reannotate))]))

(define (clone-expr v env mutated reannotate)
  (reannotate
   v
   (match v
     [`(lambda ,args . ,bodys)
      `(lambda ,args . ,(clone-body bodys env mutated reannotate))]
     [`(case-lambda [,argss . ,bodyss] ...)
      `(case-lambda ,@(for/list ([args (in-list argss)]
                                 [bodys (in-list bodyss)])
                        `[,args . ,(clone-body bodys env mutated reannotate)]))]
     [`(quote ,_) v]
     [`(let-values . ,_) (clone-let v env mutated reannotate)]
     [`(letrec-values . ,_) (clone-let v env mutated reannotate)]
     [`(if ,tst ,thn ,els)
      `(if ,(clone-expr tst env mutated reannotate)
           ,(clone-expr thn env mutated reannotate)
           ,(clone-expr els env mutated reannotate))]
     [`(with-continuation-mark ,key ,val ,body)
      `(with-continuation-mark ,(clone-expr key env mutated reannotate)
                               ,(clone-expr val env mutated reannotate)
                               ,(clone-expr body env mutated reannotate))]
     [`(begin ,exps ...)
      `(begin . ,(clone-body exps env mutated reannotate))]
     [`(begin0 ,exps ...)
      `(begin0 . ,(clone-body exps env mutated reannotate))]
     [`(set! ,id ,rhs)
      `(set! ,id ,(clone-expr rhs env mutated reannotate))]
     [`(#%variable-reference) v]
     [`(#%variable-reference ,id)
      `(#%variable-reference ,(clone-expr id env mutated reannotate))]
     [`(,rator . ,_)
      (clone-body v env mutated reannotate)]
     [`,_
      (let ([u-v (unwrap v)])
        (cond
          [(symbol? u-v)
           (lookup env u-v v)]
          [else v]))])))

(define (lookup env sym default)
  (cond
    [(null? env) default]
    [(eq? (caar env) sym)
     (cdar env)]
    [else (lookup (cdr env) sym default)]))

;; ----------------------------------------

(define (known-inline->export-known k prim-knowns imports exports)
  (cond
    [(known-procedure/can-inline? k)
     (define needed
       (needed-imports (known-procedure/can-inline-expr k) prim-knowns imports exports '() '#hasheq()))
     (cond
       [(not needed) (known-procedure (known-procedure-arity-mask k))]
       [(hash-empty? needed) k]
       [else
        (known-procedure/can-inline/need-imports
         (known-procedure-arity-mask k)
         (known-procedure/can-inline-expr k)
         (for/list ([(k v) (in-hash needed)])
           (cons k v)))])]
    [else k]))

(define (needed-imports v prim-knowns imports exports env needed)
  (and
   needed
   (match v
     [`(lambda ,args . ,bodys)
      (body-needed-imports bodys prim-knowns imports exports (add-args env args) needed)]
     [`(case-lambda [,argss . ,bodyss] ...)
      (for/fold ([needed needed]) ([args (in-list argss)]
                                   [bodys (in-list bodyss)])
        (body-needed-imports bodys prim-knowns imports exports (add-args env args) needed))]
     [`(quote ,_) needed]
     [`(let-values . ,_) (let-needed-imports v prim-knowns imports exports env needed)]
     [`(letrec-values . ,_) (let-needed-imports v prim-knowns imports exports env needed)]
     [`(if ,tst ,thn ,els)
      (needed-imports tst prim-knowns imports exports env
                      (needed-imports thn prim-knowns imports exports env
                                      (needed-imports els prim-knowns imports exports env
                                                      needed)))]
     [`(with-continuation-mark ,key ,val ,body)
      (needed-imports key prim-knowns imports exports env
                      (needed-imports val prim-knowns imports exports env
                                      (needed-imports body prim-knowns imports exports env
                                                      needed)))]
     [`(begin ,exps ...)
      (body-needed-imports exps prim-knowns imports exports env needed)]
     [`(begin0 ,exps ...)
      (body-needed-imports exps prim-knowns imports exports env needed)]
     [`(set! ,id ,rhs)
      (define u (unwrap id))
      (cond
        [(hash-ref exports id #f)
         ;; Cannot inline assignment to an exported variable
         #f]
        [else
         (needed-imports id prim-knowns imports exports env
                         (needed-imports rhs prim-knowns imports exports env
                                         needed))])]
     [`(#%variable-reference . ,_)
      ;; Cannot inline a variable reference
      #f]
     [`(,rator . ,_)
      (body-needed-imports v prim-knowns imports exports env needed)]
     [`,_
      (let ([u-v (unwrap v)])
        (cond
          [(symbol? u-v)
           (cond
             [(or (memq u-v env)
                  (hash-ref prim-knowns u-v #f)
                  (hash-ref needed u-v #f))
              needed]
             [(hash-ref exports u-v #f)
              => (lambda (ex)
                   (hash-set needed u-v (cons (export-ext-id ex) #f)))]
             [(hash-ref imports u-v #f)
              => (lambda (im)
                   (hash-set needed u-v (cons (import-ext-id im)
                                              (import-group-index (import-grp im)))))]
             [else
              ;; Free variable (possibly defined but not exported) => cannot inline
              #f])]
          [else needed]))])))

(define (body-needed-imports l prim-knowns imports exports env needed)
  (for/fold ([needed needed]) ([e (in-wrap-list l)])
    (needed-imports e prim-knowns imports exports env needed)))

(define (let-needed-imports v prim-knowns imports exports env needed)
  (match v
    [`(,let-id ([,idss ,rhss] ...) ,bodys ...)
     (define new-env (for*/fold ([env env]) ([ids (in-list idss)]
                                             [id (in-list ids)])
                       (cons (unwrap id) env)))
     (body-needed-imports bodys prim-knowns imports exports new-env
                          (for/fold ([needed needed]) ([rhs (in-list rhss)])
                            (needed-imports rhs prim-knowns imports exports new-env
                                            needed)))]))

(define (add-args env args)
  (cond
    [(wrap-null? args) env]
    [(wrap-pair? args)
     (add-args (cons (unwrap (wrap-car args)) env)
               (wrap-cdr args))]
    [else
     (cons (unwrap args) env)]))
