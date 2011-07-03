#lang scheme/base

(require mzlib/etc 
         mzlib/list
         (for-syntax "firstorder.ss"
                     scheme/base))

(provide rewrite-contract-error-message
         reraise-rewriten-lookup-error-message
         get-rewriten-error-message
         plural
         raise-not-bound-error
         argcount-error-message)

(define (reraise-rewriten-lookup-error-message e id was-in-app-position)
  (let ([var-or-function (if was-in-app-position "function" "variable")])
    (raise-syntax-error
     #f
     (format "this ~a is not defined" var-or-function)
     id)))

(define (exn-needs-rewriting? exn)
  (exn:fail:contract? exn))
  
(define (plural n)
  (if (> (string->number n) 1) "s" ""))

(define (raise-not-bound-error id)
  (if (syntax-property id 'was-in-app-position)
      (raise-syntax-error
       #f
       "this function is not defined"
       id)
      (raise-syntax-error
       #f
       "this variable is not defined"
       id)))

(define (argcount-error-message arity found)
  (define fn-is-large (> (string->number arity) (string->number found)))
  (format "expects ~a~a argument~a, but found ~a~a"
          (if fn-is-large "" "only ")
          arity (plural arity)
          (if fn-is-large "only " "")
          found))

(define (rewrite-contract-error-message msg)
  (define replacements
    (list (list #rx"expects argument of type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #rx"expects type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #px"expects at least (\\d+) argument.?, given (\\d+): .*"
                (lambda (all one two) (format "expects at least ~a argument~a, but found only ~a."
                                              one (plural one) two)))
          (list #px"expects (\\d+) argument.?, given (\\d+): .*"
                (lambda (all one two) (argcount-error-message one two)))
          (list #rx"^procedure "
                (lambda (all) ""))
          ))
  (for/fold ([msg msg]) ([repl. replacements])
      (regexp-replace* (first repl.) msg (second repl.))))

(define (get-rewriten-error-message exn)
  (if (exn-needs-rewriting? exn)
      (rewrite-contract-error-message (exn-message exn))
      (exn-message exn)))
