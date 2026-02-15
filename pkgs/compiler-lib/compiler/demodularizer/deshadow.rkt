#lang racket/base
(require racket/match)

(provide deshadow-linklet)

;; A linklet is not allowed to have shadowing bindings. After merging
;; multiple linklets, some local variables may shadow imports or
;; definitions thath were from other linklets. Rename as needed.

(define (deshadow-linklet root-phase l)
  (match l
    [`(linklet
          ([,iss ...] ...)
          (,es ...)
        ,body ...)
     (define i-env
       (for*/fold ([env #hasheq()]) ([is (in-list iss)]
                                     [i (in-list is)])
         (match i
           [`[,ext-id ,int-id] (hash-set env int-id int-id)]
           [_ (hash-set env i i)])))
     (define top-env
       (for/fold ([env i-env]) ([b (in-list body)])
         (let loop ([b b] [env env])
           (match b
             [`(define-values ,ids ,rhs)
              (for/fold ([env env]) ([id (in-list ids)])
                (hash-set env id id))]
             [`(begin . ,body)
              (for/fold ([env env]) ([b (in-list body)])
                (loop b env))]
             [else env]))))
     (define (rename-formals arg env)
       (cond
         [(null? arg) (values arg env)]
         [(symbol? arg)
          (let loop ([name arg] [i 0])
            (cond
              [(hash-ref env name #f)
               (let ([i (add1 i)])
                 (loop (string->symbol (format "~a_~a" name i)) i))]
              [else
               (values name (hash-set env arg name))]))]
         [(pair? arg)
          (define-values (a a-env) (rename-formals (car arg) env))
          (define-values (d d-env) (rename-formals (cdr arg) a-env))
          (values (cons a d) d-env)]
         [else (error "bad formal")]))
     (define (rename-formalss arg env)
       (rename-formals arg env))
     (define new-body
       (for/list ([b (in-list body)])
         (let loop ([b b] [env top-env])
           (define (rloop b) (loop b env))
           (define (lookup id) (hash-ref env id id))
           (match b
             [`(define-values ,ids ,rhs)
              `(define-values ,ids ,(rloop rhs))]
             [`(lambda ,args ,body)
              (define-values (new-args new-env) (rename-formals args env))
              `(lambda ,new-args ,(loop body new-env))]
             [`(case-lambda [,argss ,bodys] ...)
              `(case-lambda ,@(for/list ([args (in-list argss)]
                                         [body (in-list bodys)])
                                (define-values (new-args new-env) (rename-formals args env))
                                `[,new-args ,(loop body new-env)]))]
             [`(let-values ([,idss ,rhss] ...) ,body)
              (define-values (new-idss new-env) (rename-formalss idss env))
              `(let-values ,(for/list ([ids (in-list new-idss)]
                                       [rhs (in-list rhss)])
                              `[,ids ,(loop rhs new-env)])
                 ,(loop body new-env))]
             [`(letrec-values ([,idss ,rhss] ...) ,body)
              (define-values (new-idss new-env) (rename-formalss idss env))
              `(letrec-values ,(for/list ([ids (in-list idss)]
                                          [rhs (in-list rhss)])
                                 `[,ids ,(loop rhs env)])
                 ,(loop body env))]
             [`(if ,tst ,thn ,els)
              `(if ,(rloop tst) ,(rloop thn) ,(rloop els))]
             [`(begin . ,body)
              `(begin ,@(map rloop body))]
             [`(begin0 ,e . ,body)
              `(begin0 ,(rloop e) ,@(map rloop body))]
             [`(set! ,id ,rhs)
              `(set! ,(lookup id) ,(rloop rhs))]
             [`(quote . ,_) b]
             [`(#%foreign-inline . ,_) b]
             [`(with-continuation-mark ,key ,val ,body)
              `(with-continuation-mark ,(rloop key) ,(rloop val) ,(rloop body))]
             [`(#%variable-reference ,id)
              `(#%variable-reference ,(lookup id))]
             [`(#%variable-reference . ,_) b]
             [`(,rator ,rands ...)
              `(,(rloop rator) ,@(map rloop rands))]
             [_ (if (symbol? b)
                    (lookup b)
                    b)]))))
     `(linklet ,iss ,es ,@new-body)]))
