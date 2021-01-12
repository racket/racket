#lang racket/base
(require "../port/string-port.rkt"
         (submod "../print/main.rkt" internal)
         "../format/printf.rkt")

(provide error
         raise-user-error
         error-print-source-location)

(define (error init . args)
  (raise
   (format-error 'error exn:fail init args)))

(define (raise-user-error init . args)
  (raise
   (format-error 'raise-user-error exn:fail:user init args)))

(define (format-error who exn:fail init args)
  (cond
   [(and (symbol? init)
         (null? args))
    (exn:fail
     (string-append "error: " (symbol->string init))
     (current-continuation-marks))]
   [(symbol? init)
    (unless (string? (car args))
      (raise-argument-error who "string?" (car args)))
    (define o (open-output-string))
    (do-printf who o (car args) (cdr args))
    (exn:fail
     (string-append (symbol->string init)
                    ": "
                    (get-output-string o))
     (current-continuation-marks))]
   [(string? init)
    (exn:fail
     (apply string-append
            init
            (for/list ([arg (in-list args)])
              (string-append " "
                             ((error-value->string-handler)
                              arg
                              (error-print-width)))))
     (current-continuation-marks))]
   [else
    (raise-argument-error who "(or/c symbol? string?)" init)]))

(define error-print-source-location
  (make-parameter #t (lambda (v) (and v #t)) 'error-print-source-location))

;; Install the default error-value->string handler,
;; replacing the non-working primitive placeholder
(define (install-error-value->string-handler!)
  (error-value->string-handler
   (let ([default-error-value->string-handler
           (lambda (v len)
             (unless (exact-nonnegative-integer? len)
               (raise-argument-error 'default-error-value->string-handler
                                     "exact-nonnegative-integer?"
                                     len))
             (define o (open-output-string))
             (do-global-print 'default-error-value->string-handler v o 0 len)
             (get-output-string o))])
     default-error-value->string-handler)))

(void (install-error-value->string-handler!))

(module+ place-init
  (provide install-error-value->string-handler!))
