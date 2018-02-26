#lang racket/base
(require "../host/rktio.rkt"
         "../host/error.rkt"
         "output-port.rkt"
         "input-port.rkt"
         "fd-port.rkt")

(provide current-input-port
         current-output-port
         current-error-port)

(define current-input-port
  (make-parameter (open-input-fd (check-rktio-error
                                  (rktio_std_fd rktio RKTIO_STDIN)
                                  "error initializing stdin")
                                 'stdin)
                  (lambda (v)
                    (unless (input-port? v)
                      (raise-argument-error 'current-input-port
                                            "input-port?"
                                            v))
                    v)))
                  
(define current-output-port
  (make-parameter (open-output-fd (check-rktio-error
                                   (rktio_std_fd rktio RKTIO_STDOUT)
                                   "error initializing stdout")
                                  'stdout
                                  #:buffer-mode 'infer)
                  (lambda (v)
                    (unless (output-port? v)
                      (raise-argument-error 'current-output-port
                                            "output-port?"
                                            v))
                    v)))
                  
(define current-error-port
  (make-parameter (open-output-fd (check-rktio-error
                                   (rktio_std_fd rktio RKTIO_STDERR)
                                   "error initializing stderr")
                                  'stderr
                                  #:buffer-mode 'none)
                  (lambda (v)
                    (unless (output-port? v)
                      (raise-argument-error 'current-error-port
                                            "output-port?"
                                            v))
                    v)))
