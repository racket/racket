#lang racket/base
(require "../host/rktio.rkt"
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "../host/place-local.rkt")

(provide shared-ltps
         shared-ltps-place-init!

         fd-semaphore-update!
         fd-semaphore-poll-ready?)

(define (make-ltps)
  (define ltps (rktio_ltps_open rktio))
  (unless (rktio-error? ltps)
    (unsafe-custodian-register (current-custodian)
                               ltps
                               ;; in atomic mode
                               (lambda (ltps)
                                 (rktio_ltps_remove_all rktio ltps)
                                 (rktio_ltps_close rktio ltps)
                                 (shared-ltps-reset!))
                               #f
                               #f))
  (if (rktio-error? ltps)
      rktio_NULL
      ltps))

(define-place-local shared-ltps (make-ltps))

(define (shared-ltps-place-init!)
  (set! shared-ltps (make-ltps)))

(define (shared-ltps-reset!)
  (set! shared-ltps rktio_NULL))

;; ----------------------------------------

;; in atomic mode
(define (fd-semaphore-update! fd mode)
  (cond
    [(eq? shared-ltps rktio_NULL) #f]
    [else
     (define h (rktio_ltps_add rktio shared-ltps fd (case mode
                                                      [(read) RKTIO_LTPS_CREATE_READ]
                                                      [(write) RKTIO_LTPS_CREATE_WRITE]
                                                      [(check-read) RKTIO_LTPS_CHECK_READ]
                                                      [(check-write) RKTIO_LTPS_CHECK_WRITE]
                                                      [else RKTIO_LTPS_REMOVE])))
     (cond
       [(rktio-error? h)
        ;; We could log failures that are not RKTIO_ERROR_LTPS_REMOVED or RKTIO_ERROR_LTPS_NOT_FOUND
        #f]
       [else
        (define ib (rktio_ltps_handle_get_data rktio h))
        (cond
          [(not (eq? ib rktio_NULL))
           (immobile-cell-ref (address->immobile-cell ib))]
          [else
           (define s (make-semaphore))
           (define ib (malloc-immobile-cell s))
           (rktio_ltps_handle_set_data rktio h (immobile-cell->address ib))
           s])])]))

;; in atomic mode
(define (fd-semaphore-poll-ready?)
  (cond
    [(eq? shared-ltps rktio_NULL) #f]
    [else
     (rktio_ltps_poll rktio shared-ltps)
     (let loop ([did? #f])
       (define h (rktio_ltps_get_signaled_handle rktio shared-ltps))
       (cond
         [(rktio-error? h)
          ;; Could log an error that isn't RKTIO_ERROR_LTPS_NOT_FOUND
          did?]
         [else
          (define ib (address->immobile-cell (rktio_ltps_handle_get_data rktio h)))
          (semaphore-post-all (immobile-cell-ref ib))
          (free-immobile-cell ib)
          (rktio_free h)
          (loop #t)]))]))
