#lang scheme/load

;; purpose: when on-tick or on-xxx has been redefined, 
;; --- raise more specific error message
;; (why am I running this in scheme/load for the namespace in eval)

(error-print-source-location #f)

(define legal "~a: not a legal clause in a world description")
(define double 
  (string-append (format legal 'on-tick) ", on-tick has been redefined"))

(with-handlers ((exn:fail:syntax? 
                 (lambda (x) 
                   (unless (string=? (exn-message x) double) (raise x)))))
  (eval '(module a scheme 
           (require 2htdp/universe)
           (local ((define (run) (big-bang 0 (on-tick on-tick)))
                   (define (on-tick t) 0))
             10))))

;; purpose: catch illegal shapes of the form (kwd . stuff)

(with-handlers ((exn:fail:syntax? 
                 (lambda (e)
                   (unless (string=? (exn-message e) (format legal 'on-tic))
                     (raise e)))))
  (eval '(module a scheme
           (require 2htdp/universe)
           (big-bang 0 (on-tic add1)))))

;; purpose: catch illegal atomic clauses 

(with-handlers ((exn:fail:syntax? 
                 (lambda (e)
                   (unless (string=? (exn-message e) (format legal 'stop-when))
                     (raise e)))))
  (eval '(module a scheme
           (require 2htdp/universe)
           (big-bang 0 (on-tick add1) stop-when))))