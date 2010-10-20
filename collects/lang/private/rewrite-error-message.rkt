#lang scheme/base

(require mzlib/etc mzlib/list)
(require (for-syntax scheme/base))
(require (for-syntax "firstorder.ss"))

(provide rewrite-contract-error-message
         rewrite-lookup-error-message/rand
         rewrite-lookup-error-message/rator
         wrap-for-contract-error-message
         wrap-for-lookup-error-message
         ::)

(define (rewrite-lookup-error-message/rator e)
  (rewrite-lookup-error-message e "function"))

(define (rewrite-lookup-error-message/rand e)
  (rewrite-lookup-error-message e "variable"))

(define (rewrite-lookup-error-message e var-or-function)
  (define new-message
    (regexp-replace* #rx"reference to an identifier before its definition"
                     (exn-message e)
                     (format "this is ~a not defined" var-or-function)))
  (struct-copy exn e [message new-message]))

(define-syntax (wrap-for-lookup-error-message stx)
  (syntax-case stx ()
    [(_ . id)
     (with-syntax ([top (syntax/loc stx #%top)])
       (syntax/loc stx
         (with-handlers ([exn:fail:contract:variable?
                          (compose raise rewrite-lookup-error-message)])
           (top . id))))]))

(define (rewrite-contract-error-message e)
  (define replacements
    (list (list #rx"expects argument of type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #rx"expects type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))))
  (define new-message
    (for/fold ([msg (exn-message e)]) ([repl. replacements])
      (regexp-replace* (first repl.) msg (second repl.))))
  (struct-copy exn e [message new-message]))

(define-for-syntax (wrap-for-contract-error-message* stx)
  (syntax-case stx ()
    [(_ new old)
     #'(define (new . args)
         (with-handlers ([exn:fail:contract? (compose raise rewrite-contract-error-message)])
           (apply old args)))]))

(define-syntax wrap-for-contract-error-message wrap-for-contract-error-message*)

(define-syntax :: wrap-for-contract-error-message*) ;; to circumvent most of the ugliness of provide-and-document/wrap's renaming of the function's infered name