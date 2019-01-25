#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../path/path.rkt"
         "../file/parameter.rkt"
         "../file/host.rkt"
         "../file/error.rkt"
         "../format/main.rkt"
         "fd-port.rkt"
         "close.rkt"
         "parameter.rkt"
         "count.rkt")

(provide open-input-file
         open-output-file
         open-input-output-file
         call-with-input-file
         call-with-output-file
         with-input-from-file
         with-output-to-file)

(define none (gensym))

(define/who (open-input-file path [mode1 none] [mode2 none])
  (check who path-string? path)
  (define (mode->flags mode)
    (case mode
      [(text) RKTIO_OPEN_TEXT]
      [else 0]))
  (define host-path (->host path who '(read)))
  (start-atomic)
  (check-current-custodian who)
  (define fd (rktio_open rktio
                         host-path
                         (+ RKTIO_OPEN_READ
                            (mode->flags mode1)
                            (mode->flags mode2))))
  (when (rktio-error? fd)
    (end-atomic)
    (when (or (eq? mode1 'module) (eq? mode2 'module))
      (maybe-raise-missing-module who (host-> host-path) "" "" ""
                                  (format-rktio-system-error-message fd)))
    (raise-filesystem-error who
                            fd
                            (format (string-append
                                     "cannot open input file\n"
                                     "  path: ~a")
                                    (host-> host-path))))
  (define p (open-input-fd fd (host-> host-path)))
  (end-atomic)
  (when (port-count-lines-enabled)
    (port-count-lines! p))
  p)

(define (do-open-output-file #:plus-input? [plus-input? #f] who path mode1 mode2)
  (check who path-string? path)
  (define (mode->flags mode)
    (case mode
      [(test) RKTIO_OPEN_TEXT]
      [(truncate truncate/replace) (+ RKTIO_OPEN_TRUNCATE
                                      RKTIO_OPEN_CAN_EXIST)]
      [(must-truncate) (+ RKTIO_OPEN_TRUNCATE
                          RKTIO_OPEN_MUST_EXIST)]
      [(can-update) RKTIO_OPEN_CAN_EXIST]
      [(update) RKTIO_OPEN_MUST_EXIST]
      [(append) RKTIO_OPEN_APPEND]
      [else 0]))
  (define (mode? v)
    (or (eq? mode1 v) (eq? mode2 v)))
  (define host-path (->host path who (append '(write)
                                             (if (or (mode? 'replace)
                                                     (mode? 'truncate/replace))
                                                 '(delete)
                                                 '())
                                             (if (or (mode? 'append)
                                                     (mode? 'update)
                                                     (mode? 'must-update))
                                                 '(read)
                                                 '()))))
  (start-atomic)
  (check-current-custodian who)
  (define flags
    (+ RKTIO_OPEN_WRITE
       (if plus-input? RKTIO_OPEN_READ 0)
       (mode->flags mode1)
       (mode->flags mode2)))
  (define fd0
    (rktio_open rktio host-path flags))
  (define fd
    (cond
      [(not (rktio-error? fd0)) fd0]
      [(and (or (racket-error? fd0 RKTIO_ERROR_EXISTS)
                (racket-error? fd0 RKTIO_ERROR_ACCESS_DENIED))
            (or (mode? 'replace) (mode? 'truncate/replace)))
       (define r (rktio_delete_file rktio
                                    host-path
                                    (current-force-delete-permissions)))
       (when (rktio-error? r)
         (end-atomic)
         (raise-filesystem-error who
                                 r
                                 (format (string-append
                                          "error deleting file\n"
                                          "  path: ~a")
                                         (host-> host-path))))
       (rktio_open rktio host-path flags)]
      [else fd0]))
  (when (rktio-error? fd)
    (end-atomic)
    (raise-filesystem-error who
                            fd
                            (format (string-append
                                     "~a\n"
                                     "  path: ~a")
                                    (cond
                                      [(racket-error? fd0 RKTIO_ERROR_EXISTS)
                                       "file exists"]
                                      [(racket-error? fd0 RKTIO_ERROR_IS_A_DIRECTORY)
                                       "path is a directory"]
                                      [else "error opening file"])
                                    (host-> host-path))))
  (define opened-path (host-> host-path))
  (define refcount (box (if plus-input? 2 1)))
  (define op (open-output-fd fd opened-path #:fd-refcount refcount))
  (define ip (and plus-input?
                  (open-input-fd fd opened-path #:fd-refcount refcount)))
  (end-atomic)
  (when (port-count-lines-enabled)
    (port-count-lines! op)
    (when plus-input?
      (port-count-lines! ip)))
  (if plus-input?
      (values ip op)
      op))

(define/who (open-output-file path [mode1 none] [mode2 none])
  (do-open-output-file who path mode1 mode2))

(define/who (open-input-output-file path [mode1 none] [mode2 none])
  (do-open-output-file #:plus-input? #t who path mode1 mode2))

(define/who (call-with-input-file path proc [mode none])
  (check who path-string? path)
  (check who (procedure-arity-includes/c 1) proc)
  (define i (open-input-file path mode))
  (begin0
   (proc i)
   (close-input-port i)))

(define/who (call-with-output-file path proc [mode1 none] [mode2 none])
  (check who path-string? path)
  (check who (procedure-arity-includes/c 1) proc)
  (define o (open-output-file path mode1 mode2))
  (begin0
   (proc o)
   (close-output-port o)))

(define/who (with-input-from-file path proc [mode none])
  (check who path-string? path)
  (check who (procedure-arity-includes/c 0) proc)
  (define i (open-input-file path mode))
  (parameterize ([current-input-port i])
    (dynamic-wind
     void
     proc
     (lambda ()
       (close-input-port i)))))

(define/who (with-output-to-file path proc [mode1 none] [mode2 none])
  (check who path-string? path)
  (check who (procedure-arity-includes/c 0) proc)
  (define o (open-output-file path mode1 mode2))
  (parameterize ([current-output-port o])
    (dynamic-wind
     void
     proc
     (lambda ()
       (close-output-port o)))))
