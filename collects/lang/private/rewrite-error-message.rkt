#lang scheme/base

(require mzlib/etc
         mzlib/list
         (for-syntax "firstorder.rkt"
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

(define (ensure-number n-or-str)
  (if (string? n-or-str) (string->number n-or-str) n-or-str))

(define (plural n)
  (if (> (ensure-number n) 1) "s" ""))

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

(define (argcount-error-message arity found [at-least #f])
  (define arity:n (ensure-number arity))
  (define found:n (ensure-number found))
  (define fn-is-large (> arity:n found:n))
  (format "expects ~a~a~a argument~a, but found ~a~a"
          (if at-least "at least " "")
          (if (or (= arity:n 0) fn-is-large) "" "only ")
          (if (= arity:n 0) "no" arity:n) (plural arity:n)
          (if (and (not (= found:n 0)) fn-is-large) "only " "")
          (if (= found:n 0) "none" found:n)))

(define (rewrite-contract-error-message msg)
  (define replacements
    (list (list #rx"procedure application: expected procedure, given: (.*) \\(no arguments\\)"
                (lambda (all one) 
                  (format "function call: expected a function after the open parenthesis, but received ~a" one)))
          (list #rx"procedure application: expected procedure, given: (.*); arguments were:.*"
                (lambda (all one)  
                  (format "function call: expected a function after the open parenthesis, but received ~a" one)))
          (list #rx"reference to an identifier before its definition: (.*)"
                (lambda (all one) (format "~a is used here before its definition" one)))
          (list #rx"expects argument of type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #rx"expected argument of type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #rx"expects type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #px"expects at least (\\d+) argument.?, given (\\d+)(: .*)?"
                (lambda (all one two three) (argcount-error-message one two #t)))
          (list #px"expects (\\d+) argument.?, given (\\d+)(: .*)?"
                (lambda (all one two three) (argcount-error-message one two)))
          (list #rx"^procedure "
                (lambda (all) ""))
          (list #rx", given: "
                (lambda (all) ", given "))
          (list #rx"; other arguments were:.*"
                (lambda (all) ""))
          (list #rx"expects a (struct:)"
                (lambda (all one) "expects a "))
          (list #rx"list or cyclic list"
                (lambda (all) "list"))
          (list (regexp-quote "given #(struct:object:image% ...)")
                (lambda (all) "given an image"))
          (list (regexp-quote "given #(struct:object:image-snip% ...)")
                (lambda (all) "given an image"))
          (list (regexp-quote "given #(struct:object:cache-image-snip% ...)")
                (lambda (all) "given an image"))
          (list (regexp-quote "#(struct:object:image% ...)")
                (lambda (all) "(image)"))
          (list (regexp-quote "#(struct:object:image-snip% ...)")
                (lambda (all) "(image)"))
          (list (regexp-quote "#(struct:object:cache-image-snip% ...)")
                (lambda (all) "(image)"))))
  (for/fold ([msg msg]) ([repl. replacements])
    (regexp-replace* (first repl.) msg (second repl.))))

(define (get-rewriten-error-message exn)
  (if (exn-needs-rewriting? exn)
      (rewrite-contract-error-message (exn-message exn))
      (exn-message exn)))
