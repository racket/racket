#lang racket/base

(define err-stx #'(error '"bad"))

(define (try expr)
  (define out-str
    (parameterize ([current-namespace (make-base-namespace)])
      (parameterize ([current-compile (dynamic-require 'errortrace/errortrace-lib
                                                       'errortrace-compile-handler)]
                     [error-display-handler (dynamic-require 'errortrace/errortrace-lib
                                                             'errortrace-error-display-handler)])
        (let ([o (open-output-string)])
          (parameterize ([current-error-port o])
            (call-with-continuation-prompt
             (lambda ()
               (eval expr))))
          (get-output-string o)))))
  (unless (regexp-match? (regexp-quote (format "~s" (syntax->datum err-stx)))
                         out-str)
    (error 'test "not in context for: ~s" (syntax->datum expr))))

(try #`(begin (module m racket/base #,err-stx) (require 'm)))
(try err-stx)
(try #`(syntax-case 'a ()
         (_ #,err-stx)))
