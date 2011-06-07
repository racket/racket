#lang scheme/base

(require scheme/list)

(provide rewrite-lookup-error-message
         rewrite-contract-error-message)

(define (rewrite-lookup-error-message e id was-in-app-position)
  (let ([var-or-function (if was-in-app-position "function" "variable")])
    (raise-syntax-error
     #f
     (format "this ~a is not defined" var-or-function)
     id)))

(define (change-contract-exn-messages e msg)
  (define constructor
    (cond [(exn:fail:contract:arity? e) make-exn:fail:contract:arity]
          [(exn:fail:contract:divide-by-zero? e) make-exn:fail:contract:divide-by-zero]
          [(exn:fail:contract:non-fixnum-result? e) make-exn:fail:contract:non-fixnum-result]
          [(exn:fail:contract:continuation? e) make-exn:fail:contract:continuation]
          [else make-exn:fail:contract]))
  (constructor msg (exn-continuation-marks e)))

(define (rewrite-contract-error-message e)
  (define replacements
    (list (list #rx"expects argument of type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #rx"expects type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #rx"^procedure "
                (lambda (all) ""))
          ))
  (define new-message
    (for/fold ([msg (exn-message e)]) ([repl. replacements])
      (regexp-replace* (first repl.) msg (second repl.))))
  (change-contract-exn-messages e new-message))