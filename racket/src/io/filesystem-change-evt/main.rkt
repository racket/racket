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
         "../host/place-local.rkt"
         "../sandman/main.rkt"
         "../sandman/ltps.rkt"
         "../error/message.rkt")

(provide filesystem-change-evt?
         filesystem-change-evt
         filesystem-change-evt-cancel)

(module+ init
  (provide rktio-filesyste-change-evt-init!))

;; locked by atomic mode
(struct fs-change-evt ([rfc #:mutable]
                       [cust-ref #:mutable])
  #:reflection-name 'filesystem-change-evt
  #:property prop:evt (poller
                       ;; in atomic mode
                       (lambda (fc ctx)
                         (define rfc (fs-change-evt-rfc fc))
                         (cond
                           [(not rfc) (values (list fc) #f)]
                           [(eqv? (rktioly (rktio_poll_fs_change_ready rktio rfc)) RKTIO_POLL_READY)
                            (values (list fc) #f)]
                           [else
                            (sandman-poll-ctx-add-poll-set-adder!
                             ctx
                             ;; atomic and in rktio, must not start nested rktio
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
  (poll-filesystem-change-finalizations)
  (start-rktio)
  (define file-rfc (rktio_fs_change rktio fn shared-ltps))
  (define rfc
    (cond
      [(rktio-error? file-rfc)
       (cond
         [(and (zero? (bitwise-and (rktio_fs_change_properties rktio) RKTIO_FS_CHANGE_FILE_LEVEL))
               (rktio_file_exists rktio fn))
          ;; try directory containing the file
          (define-values (base name dir) (split-path (host-> fn)))
          (define base-fn (->host base who '(exists)))
          (rktio_fs_change rktio base-fn shared-ltps)]
         [else
          file-rfc])]
      [else file-rfc]))
  (end-rktio)
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
     (unless filesystem-change-evt-will-executor
       (set! filesystem-change-evt-will-executor (make-will-executor)))
     (will-register filesystem-change-evt-will-executor fc (lambda (fc) (close-fc fc)))
     (end-atomic)
     fc]))

(define/who (filesystem-change-evt-cancel fc)
  (check who filesystem-change-evt? fc)
  (atomically
   (close-fc fc)))

;; in atomic mode
(define (close-fc fc)
  (define rfc (fs-change-evt-rfc fc))
  (when rfc
    (unsafe-custodian-unregister fc (fs-change-evt-cust-ref fc))
    (set-fs-change-evt-cust-ref! fc #f)
    (set-fs-change-evt-rfc! fc #f)
    (rktioly
     (rktio_fs_change_forget rktio rfc))))

(define-place-local filesystem-change-evt-will-executor #f)

(define (poll-filesystem-change-finalizations)
  (when (and filesystem-change-evt-will-executor
             (will-try-execute filesystem-change-evt-will-executor))
    (poll-filesystem-change-finalizations)))

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

(define (rktio-filesyste-change-evt-init!)
  (set! filesystem-change-evt-will-executor #f))
