#lang racket/base
(require mzlib/contract)

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

(provide/contract
 [network-error ((symbol? string?) (listof any/c) . ->* . (void))]
 [exn->string ((or/c exn? any/c) . -> . string?)])
