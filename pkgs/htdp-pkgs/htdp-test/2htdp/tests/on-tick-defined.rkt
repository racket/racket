#lang racket/load

;; purpose: when on-tick or on-xxx has been redefined, 
;; --- raise more specific error message
;; (why am I running this in scheme/load for the namespace in eval)

(error-print-source-location #f)

(define legal "big-bang: ~a clauses are not allowed within big-bang")
(define double "big-bang: the on-tick keyword seems to have been used as a variable")
(define atleast "big-bang: expects a [to-draw handler] clause, missing")

;; is the mandatort to-draw clause specified 
(with-handlers ((exn:fail:syntax? 
                 (lambda (x) 
                   (unless (string=? (exn-message x) atleast) (raise x)))))
  (eval '(module a scheme 
           (require 2htdp/universe)
           (local ((define (run) (big-bang 0 (on-tick add1))))
             10))))

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
                   (unless (string=? (exn-message e) 
                                     "big-bang: expected a clause, but found something else")
                     (raise e)))))
  (eval '(module a scheme
           (require 2htdp/universe)
           (big-bang 0 (on-tick add1) stop-when))))

;; ---------------------------------------------------------------------------------------------------
;; purpose: catch illegal big-bang use w/o world expression

(with-handlers 
    ((exn:fail:syntax? 
      (lambda (x) 
        (unless (string=? (exn-message x) "big-bang: expected an initial state, but found a clause")
          (raise x)))))
  (eval '(module a scheme 
           (require 2htdp/universe)
           (big-bang (on-key add1)))))

(with-handlers
    ((exn:fail:syntax? 
      (lambda (x) 
        (unless (string=? (exn-message x) "universe: expected an initial state, but found a clause")
          (raise x)))))
  (eval '(module a scheme 
           (require 2htdp/universe)
           (universe (on-msg sub1) (on-new add1)))))
