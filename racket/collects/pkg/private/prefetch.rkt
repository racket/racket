#lang racket/base
(require racket/async-channel)

(provide make-prefetch-group
         prefetch-group-in-progress
         call-with-prefetch-cleanup
         make-prefetch-future
         make-prefetch-future/hash
         prefetch-future?
         prefetch-touch)

;; A prefetch is a kind of future for getting package info from
;; a catalog or getting a checksum from a remote source. We
;; run at most `MAX-PARALLEL` of them at once, and we avoid
;; creating any threads if only one needs to run.

;; Output via `download-printf` is pumped back to the original thread
;; so that it's not interleaved. Output is pumped while waiting on a
;; future or when creating one.

(define MAX-PARALLEL 32)

;; Only mutate a group or a future with the group's lock held

(struct prefetch-group (custodian 
                        [in-cleanup? #:mutable] 
                        in-progress
                        lock
                        [pending #:mutable]
                        [pending-rev #:mutable]
                        [tokens #:mutable]
                        output))

(struct prefetch-future ([proc #:mutable]
                         [get-result #:mutable]
                         [evt #:mutable]))

(struct output-msg (token fmt args))

(define (make-prefetch-group)
  (prefetch-group (make-custodian)
                  #f
                  (make-hash)
                  (make-semaphore 1)
                  null
                  null
                  (for/list ([i (in-range MAX-PARALLEL)]) 
                    i)
                  (make-async-channel)))


;; Ensure that `group` is terminated on exit from `thunk`.
;; Terminate only on the outer exit in case of nested calls.
(define (call-with-prefetch-cleanup group thunk)
  (if (prefetch-group-in-cleanup? group)
      (thunk)
      (dynamic-wind
       (lambda ()
         (set-prefetch-group-in-cleanup?! group #t))
       thunk
       (lambda ()
         (custodian-shutdown-all (prefetch-group-custodian group))))))

;; Put `proc` in a future to be potentially run in a thread.
(define (make-prefetch-future group download-printf proc)
  (pump-output group download-printf)
  (define f (prefetch-future proc #t #f))
  (call-with-semaphore (prefetch-group-lock group)
    (lambda ()
      (set-prefetch-group-pending-rev! group (cons f (prefetch-group-pending-rev group)))
      (maybe-start-future group download-printf)))
  (pump-output group download-printf)
  f)

;; Like `make-prefetch-future`, but ensures a single prefetch for a
;; given key with respect to the given table, and install the new future
;; in that table.
(define (make-prefetch-future/hash table key proc group download-printf)
  (define s (make-semaphore))
  (define f (make-prefetch-future
             group
             download-printf
             (lambda (download-printf)
               ;; Don't start until hash table has future:
               (semaphore-wait s)
               ;; Adjusts cache when it has a result:
               (proc download-printf))))
  (hash-set! table key f)
  (semaphore-post s)
  f)

;; Wait for a future to be ready
(define (prefetch-touch f group download-printf)
  (pump-output group download-printf)
  (cond
   [(prefetch-future-evt f)
    (pump-output group download-printf
                 #:evt (prefetch-future-evt f)
                 #:timeout #f)
    ((prefetch-future-get-result f))]
   [else
    ((call-with-semaphore (prefetch-group-lock group)
       (lambda ()
         (cond
          [(prefetch-future-evt f)
           ;; Got a thread meanwhile, so recur:
           (lambda ()
             (prefetch-touch f group download-printf))]
          [else
           ;; Mark this future as no longer pending, and
           ;; run its procedure directly:
           (make-thread-proc f download-printf)]))))]))

;; Wraps the procedure in `f` so that it's ready to run in a thread,
;; and replace the procured in `f` to eventually return the result
;; or propagate an exception.
(define (make-thread-proc f download-printf)
  (define s (make-semaphore))
  (define proc (prefetch-future-proc f))
  (set-prefetch-future-proc! f #f)
  (define result #f)
  (define result-is-exn? #f)
  (define (get-result) (if result-is-exn?
                           (raise result)
                           result))
  (set-prefetch-future-get-result! f get-result)
  (set-prefetch-future-evt! f (semaphore-peek-evt s))
  (lambda ()
    (with-handlers ([values (lambda (v)
                              (set! result-is-exn? #t)
                              (set! result v))])
      (set! result (proc download-printf)))
    (semaphore-post s)
    (get-result)))

;; Call with lock:
(define (maybe-start-future group download-printf)
  (cond
   [(null? (prefetch-group-pending group))
    (cond
     [(null? (prefetch-group-pending-rev group))
      ;; Nothing to do
      (void)]
     [else
      ;; Move reversed list to ready list
      (set-prefetch-group-pending! group (reverse (prefetch-group-pending-rev group)))
      (set-prefetch-group-pending-rev! group null)
      (maybe-start-future group  download-printf)])]
   [(not (prefetch-future-proc (car (prefetch-group-pending group))))
    ;; Discard no-longer-needed future
    (set-prefetch-group-pending! group (cdr (prefetch-group-pending group)))
    (maybe-start-future group  download-printf)]
   [(and (null? (cdr (prefetch-group-pending group)))
         (null? (prefetch-group-pending-rev group)))
    ;; Only one prefetch available, so don't start a thread
    (void)]
   [(null? (prefetch-group-tokens group))
    ;; Too many running already
    (void)]
   [else
    ;; Start a thread:
    (define token (car (prefetch-group-tokens group)))
    (set-prefetch-group-tokens! group (cdr (prefetch-group-tokens group)))
    (define proc
      (make-thread-proc (car (prefetch-group-pending group))
                        (lambda (fmt . args)
                          (async-channel-put (prefetch-group-output group)
                                             (output-msg token fmt args)))))
    (set-prefetch-group-pending! group (cdr (prefetch-group-pending group)))
    (parameterize ([current-custodian (prefetch-group-custodian group)])
      (thread
       (lambda ()
         (proc)
         ;; The future computation is done, so release the token and
         ;; maybe start a new future:
         (call-with-semaphore (prefetch-group-lock group)
           (lambda ()
             (set-prefetch-group-tokens! group (cons token (prefetch-group-tokens group)))
             (maybe-start-future group download-printf))))))
    (void)]))

;; Check for output from a future thread:
(define (pump-output group
                     download-printf
                     #:evt [evt never-evt]
                     #:timeout [timeout 0])
  (sync/timeout
   timeout
   evt
   (handle-evt (prefetch-group-output group)
               (lambda (msg)
                 (define token (output-msg-token msg))
                 (define fmt (output-msg-fmt msg))
                 (define args (output-msg-args msg))
                 (apply download-printf
                        (string-append
                         (format "~a~a: "
                                 (if (token . < . 10) "0" "")
                                 token)
                         fmt)
                        args)
                 (pump-output group
                               download-printf
                               #:evt evt
                               #:timeout timeout)))))
