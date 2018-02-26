#lang racket/base
(require (for-syntax racket/base))

(provide define-inline)

(define-syntax (define-inline stx)
  (syntax-case stx ()
    [(_ (proc-id arg ...) body ...)
     (with-syntax ([(arg-id ...)
                    (for/list ([arg (in-list (syntax->list #'(arg ...)))])
                      (syntax-case arg ()
                        [(id def-val) #'id]
                        [else arg]))])
       (with-syntax ([(gen-id ...)
                      (generate-temporaries #'(arg-id ...))])
         #`(define-syntax proc-id
             (syntax-rules ()
               [(_ gen-id ...)
                (let ([arg-id gen-id] ...)
                  body ...)]
               #,@(let loop ([args (syntax->list #'(arg ...))] [ids null])
                    (cond
                     [(null? args) null]
                     [(identifier? (car args)) (loop (cdr args) (cons (car args) ids))]
                     [else
                      (syntax-case (car args) ()
                        [(id def-expr)
                         (cons #`[(_ #,@(reverse ids))
                                  (proc-id #,@(reverse ids) def-expr)]
                               (loop (cdr args) (cons #'id ids)))])]))))))]))
