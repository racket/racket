#lang racket/base
(require "../host/rktio.rkt"
         "../host/error.rkt")

(provide raise-network-error
         raise-network-arguments-error)

(define (raise-network-error who orig-err base-msg)
  (define err (remap-rktio-error orig-err))
  (define msg (format-rktio-message who err base-msg))
  (raise
   (cond
     [(not (eq? (rktio-errkind err) RKTIO_ERROR_KIND_RACKET))
      (exn:fail:network:errno
       msg
       (current-continuation-marks)
       (cons (rktio-errno err)
             (let ([kind (rktio-errkind err)])
               (cond
                 [(eqv? kind RKTIO_ERROR_KIND_POSIX) 'posix]
                 [(eqv? kind RKTIO_ERROR_KIND_WINDOWS) 'windows]
                 [(eqv? kind RKTIO_ERROR_KIND_GAI) 'gai]
                 [else (error 'raise-network-error "confused about rktio error")]))))]
     [else
      (exn:fail:network
       msg
       (current-continuation-marks))])))

(define (raise-network-arguments-error who msg socket-str u)
  (unless (equal? socket-str "socket")
    (raise-argument-error 'raise-network-arguments-error
                          "\"socket\""
                          socket-str))
  (raise
   (exn:fail:network
    (string-append (symbol->string who) ": " msg
                   "\n  socket: "
                   ((error-value->string-handler) u (error-print-width)))
    (current-continuation-marks))))
