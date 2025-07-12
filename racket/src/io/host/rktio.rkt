#lang racket/base
(require racket/include
         racket/fixnum
         (for-syntax racket/base)
         (only-in '#%linklet primitive-table)
         "place-local.rkt"
         "thread.rkt"
         "pthread.rkt")

(provide rktio
         rktio-error?
         rktio-errkind
         rktio-errno
         rktio-errstep
         racket-error?

         start-rktio
         end-rktio
         rktioly

         maybe-start-sleep-rktio
         end-sleep-rktio

         rktio-mutex+sleep
         start-some-rktio
         end-some-rktio

         end-rktio+atomic

         rktio-place-init!
         rktio-place-destroy!)
;; More `provide`s added by macros below

(define rktio-table
  (or (primitive-table '#%rktio)
      (error '#%rktio "rktio not supported by host")))

(define (lookup n)
  (hash-ref rktio-table n))

(define << arithmetic-shift)

(define-syntax-rule (define-constant n v)
  (begin
    (define n v)
    (provide n)))
  
(define-syntax-rule (define-type . _) (void))
(define-syntax-rule (define-struct-type . _) (void))

(define-syntax-rule (define-function _ _ name . _)
  (begin
    (define name (lookup 'name))
    (provide name)))

(define-syntax-rule (define-function/errno _ _ _ name . _)
  (define-function () #f name))
(define-syntax-rule (define-function/errno+step _ _ _ name . _)
  (define-function () #f name))

(include "../../rktio/rktio.rktl")

(define-function () #f rktio_filesize_ref)
(define-function () #f rktio_timestamp_ref)
(define-function () #f rktio_is_timestamp)
(define-function () #f rktio_recv_length_ref)
(define-function () #f rktio_recv_address_ref)
(define-function () #f rktio_stat_to_vector)
(define-function () #f rktio_identity_to_vector)
(define-function () #f rktio_seconds_to_date*)
(define-function () #f rktio_convert_result_to_vector)
(define-function () #f rktio_to_bytes)
(define-function () #f rktio_to_bytes_list)
(define-function () #f rktio_to_shorts)
(define-function () #f rktio_NULL)
(define-function () #f rktio_do_install_os_signal_handler)
(define-function () #f rktio_get_ctl_c_handler)
(define-function () #f rktio_from_bytes_list)
(define-function () #f rktio_free_bytes_list)
(define-function () #f rktio_make_sha1_ctx)
(define-function () #f rktio_make_sha2_ctx)
(define-function () #f rktio_process_result_stdin_fd)
(define-function () #f rktio_process_result_stdout_fd)
(define-function () #f rktio_process_result_stderr_fd)
(define-function () #f rktio_process_result_process)
(define-function () #f rktio_status_running)
(define-function () #f rktio_status_result)
(define-function () #f rktio_pipe_results)

;; Error results are represented as vectors:
(define rktio-error? vector?)
(define (rktio-errkind v) (vector-ref v 0))
(define (rktio-errno v) (vector-ref v 1))
(define (rktio-errstep v) (vector-ref v 2))

(define (racket-error? v errno)
  (and (eqv? (rktio-errkind v) RKTIO_ERROR_KIND_RACKET)
       (eqv? (rktio-errno v) errno)))

(define-place-local rktio (rktio_init))

;; rktio lock order:
;;
;;    - atomic/uninterruptible mode (reentrant)
;;    - port locks (*not* reentrant, implies uninterruptable mode)
;;    - rktio lock (reentrant, implies uninterruptable mode)
;;
;; The rktio lock needs to be used for any rktio operation,
;; unless "rktio.h" says that the operation is atomic.

(struct m+s (mutex sleep handle)
  #:authentic)

(define (make-rktio-mutex+sleep rktio)
  (m+s (make-mutex)
       (box #f)
       (rktio_get_signal_handle rktio)))

(define-place-local rktio-mutex+sleep (make-rktio-mutex+sleep rktio))
(define (start-rktio)
  (start-uninterruptible)
  (mutex-acquire/wakeup-sleep rktio-mutex+sleep))
(define (end-rktio)
  (mutex-release/allow-sleep rktio-mutex+sleep)
  (end-uninterruptible))
(define-syntax-rule (rktioly e ...)
  (begin
    (start-rktio)
    (begin0
      (let () e ...)
      (end-rktio))))
(define (maybe-start-sleep-rktio) ; in scheduler, so already uninterruptible
  (maybe-mutex-acquire/start-sleep rktio-mutex+sleep))
(define (end-sleep-rktio) ; in scheduler, so already uninterruptible
  (mutex-release/end-sleep rktio-mutex+sleep))
(define (start-some-rktio mutex+sleep) (mutex-acquire/wakeup-sleep mutex+sleep))
(define (end-some-rktio mutex+sleep) (mutex-release/allow-sleep mutex+sleep))
(define (end-rktio+atomic)
  (end-rktio)
  (end-atomic))

;; used by thread other than the scheduler, because a sleeping
;; scheduler may need to be woken up to release the rktio lock
(define (mutex-acquire/wakeup-sleep mutex+sleep)
  (define (maybe-increment-sleep-wakeup)
    (define n (unbox (m+s-sleep mutex+sleep)))
    (and (fixnum? n)
         (box-cas! (m+s-sleep mutex+sleep) n (fx+ n 1))))
  (define (decrement-sleep-wakeup)
    (define n (unbox (m+s-sleep mutex+sleep)))
    (unless (box-cas! (m+s-sleep mutex+sleep) n (if (fx= n 1)
                                                    #f
                                                    (fx- n 1)))
      (decrement-sleep-wakeup)))
  (cond
    [(box-cas! (m+s-sleep mutex+sleep) #f 1)
     ;; normal mode: a `rktio_sleep` is not in progress
     (mutex-acquire (m+s-mutex mutex+sleep))
     (decrement-sleep-wakeup)]
    [(box-cas! (m+s-sleep mutex+sleep) 'sleep 1)
     ;; wakeup mode: a `rktio_sleep` is in progress, so we
     ;; tell it to wake up to allow rktio
     (rktio_signal_received_at (m+s-handle mutex+sleep))
     (mutex-acquire (m+s-mutex mutex+sleep))
     (decrement-sleep-wakeup)]
    [(maybe-increment-sleep-wakeup)
     ;; hasn't woken up from sleep, but wakeup is already requested,
     ;; and now the scheduler knows that we're waiting, too; note that
     ;; it's ok to increment and decerement the count if we already
     ;; have the mutex and this is a reentrant acquire
     (mutex-acquire (m+s-mutex mutex+sleep))
     (decrement-sleep-wakeup)]
    [else
     ;; all CASes failed, so try again
     (mutex-acquire/wakeup-sleep mutex+sleep)]))
(define (mutex-release/allow-sleep mutex+sleep)
  ;; we were not sleeping, so leave sleep state as-is
  (mutex-release (m+s-mutex mutex+sleep)))

(define (maybe-mutex-acquire/start-sleep mutex+sleep)
  (cond
    [(box-cas! (m+s-sleep mutex+sleep) #f 'sleep)
     (mutex-acquire (m+s-mutex mutex+sleep))
     ;; It's possible that we set to 'sleep, but a non-sleep
     ;; thread managed to switch to 1 and then got the mutex
     ;; and then switched to #f; it would be bad to sleep
     ;; in that case, so make sure we're still supposed to sleep
     (let loop ()
       (cond
         [(box-cas! (m+s-sleep mutex+sleep) 'sleep 'sleep)
          ;; acquired and ok
          #t]
         [(or (box-cas! (m+s-sleep mutex+sleep) #f #f)
              (ping-sleep-wakeup (m+s-sleep mutex+sleep)))
          ;; release mutex, and assume others are waiting
          (mutex-release (m+s-mutex mutex+sleep))
          #f]
         [else
          ;; must be a spurious CAS failure, so try checking again
          (loop)]))]
    [(ping-sleep-wakeup (m+s-sleep mutex+sleep))
     ;; others are waiting, and we should not sleep
     #f]
    [else
     ;; CAS failed, try again
     (maybe-mutex-acquire/start-sleep mutex+sleep)]))
(define (mutex-release/end-sleep mutex+sleep)
  (cond
    [(box-cas! (m+s-sleep mutex+sleep) 'sleep #f)
     (mutex-release (m+s-mutex mutex+sleep))]
    [(ping-sleep-wakeup (m+s-sleep mutex+sleep))
     ;; others are waiting, and they'll take care of resetting the `sleep` value
     (mutex-release (m+s-mutex mutex+sleep))]
    [else
     ;; CAS failed, try again
     (mutex-release/end-sleep mutex+sleep)]))
(define (ping-sleep-wakeup sleep)
  (define n (unbox sleep))
  (and (fixnum? n) (box-cas! sleep n n)))

(define (rktio-place-init!)
  (set! rktio (rktio_init))
  (set! rktio-mutex+sleep (make-rktio-mutex+sleep rktio)))

(define (rktio-place-destroy!)
  (rktio_destroy rktio)
  (set! rktio #f))

;; Only in the main place:
(void (rktio_do_install_os_signal_handler rktio))
