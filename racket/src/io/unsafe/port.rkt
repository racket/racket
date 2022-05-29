#lang racket/base
(require "../common/check.rkt"
         "../host/rktio.rkt"
         "../host/thread.rkt"
         "../string/convert.rkt"
         "../port/fd-port.rkt"
         "../network/tcp-port.rkt"
         "../sandman/ltps.rkt")

(provide unsafe-file-descriptor->port
         unsafe-port->file-descriptor
         unsafe-file-descriptor->semaphore

         unsafe-socket->port
         unsafe-port->socket
         unsafe-socket->semaphore

         unsafe-poll-fd)

(define/who (unsafe-file-descriptor->port system-fd name mode)
  (check who exact-integer? system-fd)
  (check who list? #:contract "(listof (or/c 'read 'write 'text 'regular-file))" mode)
  (define read? (memq 'read mode))
  (define write? (memq 'write mode))
  (define refcount (box (if (and read? write?) 2 1)))
  (define fd (rktio_system_fd rktio system-fd
                              (bitwise-ior
                               (if read? RKTIO_OPEN_READ 0)
                               (if write? RKTIO_OPEN_WRITE 0)
                               (if (memq 'text mode) RKTIO_OPEN_TEXT 0)
                               (if (memq 'regular-file mode) RKTIO_OPEN_REGFILE 0))))
  (define i (and read?
                 (open-input-fd fd name #:fd-refcount refcount)))
  (define o (and write?
                 (open-output-fd fd name #:fd-refcount refcount)))
  (if (and i o)
      (values i o)
      (or i o)))

(define/who (unsafe-socket->port system-fd name mode)
  (check who exact-integer? system-fd)
  (check who bytes? name)
  (check who list? #:contract "(listof (or/c 'no-close))" mode)
  (open-input-output-tcp system-fd (string->symbol (bytes->string/utf-8 name))
                         #:close? (not (memq 'no-close mode))))

(define (unsafe-port->file-descriptor p)
  (define fd (fd-port-fd p))
  (and fd
       (not (rktio_fd_is_pending_open rktio fd))
       (rktio_fd_system_fd rktio fd)))

(define (unsafe-port->socket p)
  (and (tcp-port? p)
       (unsafe-port->file-descriptor p)))

(define/who (unsafe-fd->semaphore system-fd mode socket?)
  (check who exact-integer? system-fd)
  (check who symbol? #:contract "(or/c 'read 'write 'check-read 'check-write 'remove)" mode)
  (start-atomic)
  (define fd (rktio_system_fd rktio system-fd
                              (bitwise-ior RKTIO_OPEN_READ
                                           RKTIO_OPEN_WRITE
                                           (if socket? RKTIO_OPEN_SOCKET 0))))
  (define sema (fd-semaphore-update! fd mode))
  (rktio_forget rktio fd)
  (end-atomic)
  sema)

(define (unsafe-file-descriptor->semaphore system-fd mode)
  (unsafe-fd->semaphore system-fd mode #f))

(define (unsafe-socket->semaphore system-fd mode)
  (unsafe-fd->semaphore system-fd mode #t))

(define (unsafe-poll-fd system-fd mode [socket? #t])
  (atomically
   (define fd (rktio_system_fd rktio system-fd (if socket? RKTIO_OPEN_SOCKET 0)))
   (define ready?
     (case mode
       [(read) (eqv? (rktio_poll_read_ready rktio fd) RKTIO_POLL_READY)]
       [(write) (eqv? (rktio_poll_write_ready rktio fd) RKTIO_POLL_READY)]
       [else #f]))
   (rktio_forget rktio fd)
   ready?))
