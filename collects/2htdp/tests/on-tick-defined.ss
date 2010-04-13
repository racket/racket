#lang scheme/load

;; purpose: when on-tick or on-xxx has been redefined, 
;; --- raise more specific error message
;; (why am I running this in scheme/load for the namespace in eval)

(error-print-source-location #f)

(define legal "on-tick: not a legal clause in a world description")
(define double ", on-tick has been redefined")

(with-handlers ((exn:fail:syntax? 
                 (lambda (x) 
                   (unless 
                       (string=? (exn-message x) (string-append legal double))
                     (raise x)))))
  (eval '(module a scheme 
           (require 2htdp/universe)
           (local ((define (run) (big-bang 0 (on-tick on-tick)))
                   (define (on-tick t) 0))
             10))))
