#lang racket/base
(require (for-syntax racket/base)
         racket/contract
         web-server/lang/serial-lambda
         racket/list)

(provide/contract
 [web-parameter? (any/c . -> . boolean?)])
(provide make-web-parameter
         web-parameterize)

(define (web-parameter? any)
  (and (procedure? any)
       (procedure-arity-includes? any 0)
       (procedure-arity-includes? any 2)))  

(define next-web-parameter-id
  (let ([i (box 0)])
    (lambda ()
      (begin0 (unbox i)
              (set-box! i (add1 (unbox i)))))))

; This is syntax so that the web-language transformations can occur.
(define-syntax make-web-parameter
  (syntax-rules ()
    [(_ default)
     ; Key is a lambda, the defunctionalization process will turn it into a serializable value with the module's label embedded in it, that way the parameters are not guessable AND sensitive to changes in the source
     ; I don't like the assumption of deserialization though, but I have to do this grossness because w-c-m uses equal? and post-deserialization, the two lambdas are not equal.
     (let* ([id (next-web-parameter-id)]
            [label (closure->deserialize-name (lambda () 'web-param))]
            [key (string->symbol (format "~a-~a" label id))])
       (case-lambda
         [()
          (let ([cur
                 (continuation-mark-set->list 
                  (current-continuation-marks) 
                  key)])
            (if (empty? cur)
                default
                (first cur)))]
         [(v thunk)
          (with-continuation-mark key v (thunk))]))]))

(define-syntax web-parameterize/values
  (syntax-rules ()
    [(_ () e ...)
     (begin e ...)]
    [(_ ([wp v]) e ...)
     (wp v (lambda () e ...))]
    [(_ ([fwp fv] [wp v] ...) e ...)
     (web-parameterize/values ([fwp fv]) (web-parameterize/values ([wp v] ...) e ...))]))

(define-syntax (web-parameterize stx)
  (syntax-case stx ()
    [(_ ([wp ve] ...) e ...)
     (with-syntax ([(v ...) (generate-temporaries (syntax->list #'(ve ...)))])
       (syntax/loc stx
         (let ([v ve] ...)
           (web-parameterize/values ([wp v] ...) e ...))))]))
