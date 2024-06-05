#lang racket/base
(require racket/match
         racket/set
         compiler/zo-structs
         compiler/faslable-correlated)

(provide remap-names)

(define (remap-names body
                     remap-name ; symbol -> symbol-or-import
                     #:remap-defined-name [remap-defined-name remap-name]
                     #:application-hook [application-hook (lambda (rator rands remap) #f)])
  (for/list ([b (in-list body)])
    (let loop ([b b])
      (cond
        [(faslable-correlated? b)
         (struct-copy faslable-correlated b
                      [e (loop (faslable-correlated-e b))])]
        [else
         (match b
           [`(define-values ,ids ,rhs)
            `(define-values ,(map remap-defined-name ids) ,(loop rhs))]
           [`(lambda ,args ,body)
            `(lambda ,args ,(loop body))]
           [`(case-lambda [,argss ,bodys] ...)
            `(case-lambda ,@(for/list ([args (in-list argss)]
                                       [body (in-list bodys)])
                              `[,args ,(loop body)]))]
           [`(let-values ([,idss ,rhss] ...) ,body)
            `(let-values ,(for/list ([ids (in-list idss)]
                                     [rhs (in-list rhss)])
                            `[,ids ,(loop rhs)])
               ,(loop body))]
           [`(letrec-values ([,idss ,rhss] ...) ,body)
            `(letrec-values ,(for/list ([ids (in-list idss)]
                                        [rhs (in-list rhss)])
                               `[,ids ,(loop rhs)])
               ,(loop body))]
           [`(if ,tst ,thn ,els)
            `(if ,(loop tst) ,(loop thn) ,(loop els))]
           [`(begin . ,body)
            `(begin ,@(map loop body))]
           [`(begin0 ,e . ,body)
            `(begin0 ,(loop e) ,@(map loop body))]
           [`(set! ,id ,rhs)
            `(set! ,(remap-name id) ,(loop rhs))]
           [`(quote . _) b]
           [`(with-continuation-mark ,key ,val ,body)
            `(with-continuation-mark ,(loop key) ,(loop val) ,(loop body))]
           [`(#%variable-reference ,id)
            `(#%variable-reference ,(remap-name id))]
           [`(#%variable-reference . ,_) b]
           [`(,rator ,rands ...)
            (or (application-hook rator rands loop)
                `(,(loop rator) ,@(map loop rands)))]
           [_ (if (symbol? b)
                  (remap-name b)
                  b)])]))))
