#lang racket/base
(require "../host/rktio.rkt"
         "../host/error.rkt"
         "output-port.rkt"
         "input-port.rkt"
         "fd-port.rkt")

(provide current-input-port
         current-output-port
         current-error-port
         init-current-ports!)

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

(define current-input-port
  (make-parameter (make-stdin)
                  (lambda (v)
                    (unless (input-port? v)
                      (raise-argument-error 'current-input-port
                                            "input-port?"
                                            v))
                    v)))

(define current-output-port
  (make-parameter (make-stdout)
                  (lambda (v)
                    (unless (output-port? v)
                      (raise-argument-error 'current-output-port
                                            "output-port?"
                                            v))
                    v)))

(define current-error-port
  (make-parameter (make-stderr)
                  (lambda (v)
                    (unless (output-port? v)
                      (raise-argument-error 'current-error-port
                                            "output-port?"
                                            v))
                    v)))

(define (init-current-ports!)
  (current-input-port (make-stdin))
  (current-output-port (make-stdout))
  (current-error-port (make-stderr)))
