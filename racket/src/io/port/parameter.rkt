#lang racket/base
(require "../host/place-local.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "output-port.rkt"
         "input-port.rkt"
         "fd-port.rkt")

(provide current-input-port
         current-output-port
         current-error-port

         orig-input-port
         orig-output-port
         orig-error-port

         init-current-ports!
         get-original-error-port)

(define (make-stdin)
  (open-input-fd (check-rktio-error
                  (rktio_std_fd rktio RKTIO_STDIN)
                  "error initializing stdin")
                 'stdin))

(define (make-stdout)
  (open-output-fd (check-rktio-error
                   (rktio_std_fd rktio RKTIO_STDOUT)
                   "error initializing stdout")
                  'stdout
                  #:buffer-mode 'infer))

(define (make-stderr)
  (open-output-fd (check-rktio-error
                   (rktio_std_fd rktio RKTIO_STDERR)
                   "error initializing stderr")
                  'stderr
                  #:buffer-mode 'none))

(define-place-local orig-input-port (make-stdin))
(define-place-local orig-output-port (make-stdout))
(define-place-local orig-error-port (make-stderr))

(define current-input-port
  (make-parameter orig-input-port
                  (lambda (v)
                    (unless (input-port? v)
                      (raise-argument-error 'current-input-port
                                            "input-port?"
                                            v))
                    v)))

(define current-output-port
  (make-parameter orig-output-port
                  (lambda (v)
                    (unless (output-port? v)
                      (raise-argument-error 'current-output-port
                                            "output-port?"
                                            v))
                    v)))

(define current-error-port
  (make-parameter orig-error-port
                  (lambda (v)
                    (unless (output-port? v)
                      (raise-argument-error 'current-error-port
                                            "output-port?"
                                            v))
                    v)))

(define (init-current-ports! in-fd out-fd err-fd cust plumber)
  (set! orig-input-port (open-input-fd in-fd "stdin"
                                       #:custodian cust))
  (current-input-port orig-input-port)
  (set! orig-output-port (open-output-fd out-fd "stdout"
                                         #:custodian cust
                                         #:plumber plumber))
  (current-output-port orig-output-port)
  (set! orig-error-port (open-output-fd err-fd "srderr"
                                        #:custodian cust
                                        #:plumber plumber))
  (current-error-port orig-error-port))

(define (get-original-error-port)
  orig-error-port)
