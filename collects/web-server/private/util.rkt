#lang racket/base
(require unstable/bytes
         unstable/contract
         unstable/list
         unstable/path
         unstable/string
         unstable/net/url)
(provide
 (all-from-out
  unstable/bytes
  unstable/contract
  unstable/list
  unstable/path
  unstable/string
  unstable/net/url))

(require racket/contract/base
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

(provide/contract
 [network-error (->* [symbol? string?] [] #:rest list? void?)]
 [exn->string (-> any/c string?)])
