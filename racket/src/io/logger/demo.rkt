#lang racket/base
(require "../host/bootstrap.rkt"
         "main.rkt")

(define-syntax-rule (test expect rhs)
  (let ([e expect]
        [v rhs])
    (unless (equal? e v)
      (error 'failed "~s: ~e" 'rhs v))))

(define root-logger (make-logger))

(test 'none (log-max-level root-logger))
(add-stderr-log-receiver! root-logger 'warning)

(test 'warning (log-max-level root-logger))

(log-message root-logger 'error "this should print to stderr" 5)

(define demo1-logger (make-logger 'demo1 root-logger))
(define demo2-logger (make-logger 'demo2 root-logger 'fatal))

(log-message demo1-logger 'error "this should print to stderr, too" 5)
(log-message demo2-logger 'error "this should not print to stderr" 5)

(test 'warning (log-max-level demo1-logger))
(test 'fatal (log-max-level demo2-logger))

(define lr1 (make-log-receiver root-logger 'info 'cats))

(test 'info (log-max-level demo1-logger))
(test 'fatal (log-max-level demo2-logger))

(test 'info (log-max-level demo1-logger 'cats))
(test 'fatal (log-max-level demo2-logger 'cats))

(test 'warning (log-max-level demo1-logger 'dogs))
(test 'fatal (log-max-level demo2-logger 'dogs))

(test #t (log-level? demo1-logger 'info 'cats))
(test #f (log-level? demo1-logger 'debug 'cats))
(test #f (log-level? demo1-logger 'info 'dogs))

(define msg1 #f)
(define th1 (thread (lambda () (set! msg1 (sync lr1)))))
(sync (system-idle-evt))
(test #f msg1)

(log-message demo1-logger 'info 'cats "hello" 7)
(sync (system-idle-evt))
(test '#(info "cats: hello" 7 cats) msg1)

(log-message demo1-logger 'info 'cats "goodbye" 9)
(test '#(info "cats: goodbye" 9 cats) (sync lr1))
