#lang racket/base
(require racket/contract
         (for-syntax racket/base))

;; network-error: symbol string . values -> void
;; throws a formatted exn:fail:network
(define (network-error src fmt . args)
  (raise (make-exn:fail:network (format "~a: ~a" src (apply format fmt args))
                                (current-continuation-marks))))

;; exn->string : (or/c exn any) -> string
(define (exn->string exn)
  (if (exn? exn)
      (parameterize ([current-error-port (open-output-string)])
        ((error-display-handler) (exn-message exn) exn)
        (get-output-string (current-error-port)))
      (format "~s\n" exn)))
;; Eli: (or/c exn any)??

(define-syntax (try stx)
  (syntax-case stx ()
    [(_ e) #'(#%expression e)]
    [(_ e0 e ...)
     (syntax/loc stx
       (with-handlers* ([exn:fail? (lambda (x) (try e ...))])
         (#%expression e0)))]))

(provide try)

(provide/contract
 [network-error (->* [symbol? string?] [] #:rest list? void?)]
 [exn->string (-> any/c string?)])
