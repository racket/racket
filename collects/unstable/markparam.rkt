#lang racket/base
(require racket/serialize
         racket/function
         racket/contract/base
         (for-syntax racket/base))

(define mark-parameter-first
  (curry continuation-mark-set-first #f))
(define (mark-parameter-all mp [prompt-tag (default-continuation-prompt-tag)])
  (continuation-mark-set->list (current-continuation-marks) mp prompt-tag))
(define (mark-parameters-all mps [none-v #f] [prompt-tag (default-continuation-prompt-tag)])
  (continuation-mark-set->list* (current-continuation-marks) mps none-v prompt-tag))

(serializable-struct
 mark-parameter ()
 #:transparent
 #:property prop:procedure mark-parameter-first)

(define-syntax with-continuation-mark*
  (syntax-rules ()
    [(_ () body-expr ...)
     (begin body-expr ...)]
    [(_ ([k v]) body-expr ...)
     (with-continuation-mark k v body-expr ...)]
    [(_ ([k0 v0] [k1 v1] ...) body-expr ...)
     (with-continuation-mark k0 v0
       (with-continuation-mark* ([k1 v1] ...)
         body-expr ...))]))

(define-syntax (mark-parameterize stx)
  (syntax-case stx ()
    [(_ ([mp expr] ...) body-expr ...)
     (with-syntax ([(expr-val ...) (generate-temporaries #'(expr ...))])
       (syntax/loc stx
         (let ([expr-val expr] ...)
           (with-continuation-mark* ([mp expr-val] ...)
             body-expr ...))))]))

(provide mark-parameterize
         (struct-out mark-parameter))
(provide/contract
 [mark-parameter-first ((mark-parameter?) (continuation-prompt-tag?) . ->* . any/c)]
 [mark-parameter-all ((mark-parameter?) (continuation-prompt-tag?) . ->* . list?)]
 [mark-parameters-all (((listof mark-parameter?)) (any/c continuation-prompt-tag?) . ->* . (listof vector?))])
