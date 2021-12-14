#lang racket/base
(require "../common/check.rkt"
         "../path/path.rkt"
         "../path/split.rkt"
         "../format/main.rkt"
         "../file/host.rkt"
         "../file/error.rkt"
         "../file/main.rkt"
         "../host/rktio.rkt"
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "../sandman/main.rkt"
         "../sandman/ltps.rkt"
         "../error/message.rkt")

(provide filesystem-change-evt?
         filesystem-change-evt
         filesystem-change-evt-cancel)

(struct fs-change-evt ([rfc #:mutable]
                       [cust-ref #:mutable])
  #:reflection-name 'filesystem-change-evt
  #:property prop:evt (poller
                       ;; in atomic mode
                       (lambda (fc ctx)
                         (define rfc (fs-change-evt-rfc fc))
                         (cond
                           [(not rfc) (values (list fc) #f)]
                           [(eqv? (rktio_poll_fs_change_ready rktio rfc) RKTIO_POLL_READY)
                            (values (list fc) #f)]
                           [else
                            (sandman-poll-ctx-add-poll-set-adder!
                             ctx
                             (lambda (ps)
                               (rktio_poll_add_fs_change rktio rfc ps)))
                            (values #f fc)]))))

(define (filesystem-change-evt? v)
  (fs-change-evt? v))

(define/who (filesystem-change-evt p [fail #f])
  (check who path-string? p)
  (check who (procedure-arity-includes/c 0) #:or-false fail)
  (define fn (->host p who '(exists)))
  (start-atomic)
  (define file-rfc (rktio_fs_change rktio fn shared-ltps))
  (define rfc
    (cond
      [(rktio-error? file-rfc)
       (end-atomic)
       (cond
         [(and (zero? (bitwise-and (rktio_fs_change_properties rktio) RKTIO_FS_CHANGE_FILE_LEVEL))
               (rktio_file_exists rktio fn))
          ;; try directory containing the file
          (define-values (base name dir) (split-path (host-> fn)))
          (define base-fn (->host base who '(exists)))
          (start-atomic)
          (rktio_fs_change rktio base-fn shared-ltps)]
         [else
          (start-atomic)
          file-rfc])]
      [else file-rfc]))
  (cond
    [(rktio-error? rfc)
     (end-atomic)
     (cond
       [fail (fail)]
       [(racket-error? rfc RKTIO_ERROR_UNSUPPORTED)
        (raise (exn:fail:unsupported
                (error-message->string 'filesystem-change-evt "unsupported")
                (current-continuation-marks)))]
       [else
        (raise-filesystem-error who rfc (format "error generating event\n  path: ~a"
                                                (host-> fn)))])]
    [else
     (define fc (fs-change-evt rfc #f))
     (define cust-ref (unsafe-custodian-register (current-custodian)
                                                 fc
                                                 ;; in atomic mode
                                                 (lambda (fc) (close-fc fc))
                                                 #f
                                                 #t))
     (set-fs-change-evt-cust-ref! fc cust-ref)
     (unsafe-add-global-finalizer fc (lambda () (close-fc fc)))
     (end-atomic)
     fc]))

(define/who (filesystem-change-evt-cancel fc)
  (check who filesystem-change-evt? fc)
  (start-atomic)
  (close-fc fc)
  (end-atomic))

;; in atomic mode
(define (close-fc fc)
  (define rfc (fs-change-evt-rfc fc))
  (when rfc
    (unsafe-custodian-unregister fc (fs-change-evt-cust-ref fc))
    (set-fs-change-evt-cust-ref! fc #f)
    (set-fs-change-evt-rfc! fc #f)
    (rktio_fs_change_forget rktio rfc)))

(void (set-fs-change-properties!
       (let ([props (rktio_fs_change_properties rktio)])
         (define (set? a b) (not (eqv? 0 (bitwise-and a b))))
         (cond
           [(and (set? props RKTIO_FS_CHANGE_NEED_LTPS)
                 (eq? shared-ltps rktio_NULL))
            '#(#f #f #f #f)]
           [else
            (vector (and (set? props RKTIO_FS_CHANGE_SUPPORTED)
                         'supported)
                    (and (set? props RKTIO_FS_CHANGE_SCALABLE)
                         'scalable)
                    (and (set? props RKTIO_FS_CHANGE_LOW_LATENCY)
                         'low-latency)
                    (and (set? props RKTIO_FS_CHANGE_FILE_LEVEL)
                         'file-level))]))))
