#lang racket/base
(require "place-local.rkt"
         "check.rkt"
         "tree.rkt"
         "internal-error.rkt"
         "sandman-struct.rkt"
         "current-sandman.rkt"
         "host.rkt")

;; A "sandman" manages the set of all sleeping threads that may need
;; to be awoken in response to an external event, and it implements
;; the process-wide `sleep` that waits for an external event. Timeouts
;; are the only external events recognized by the initial sandman,
;; and that is supported by the host system's `sleep` function.

;; When a thread is registered with a sandman, the sandman provides a
;; handle representing the registration. The handle can be any value
;; except #f, and it is provided back to the sandman to unregister a
;; thread. A sandman doesn't unregister threads on its own, even when
;; it detects that an external event has happened.

;; When `sync` determines that a thread should sleep, it accumulates
;; external-event specifications to provide to the sandman along with
;; the thread. For the initial sandman, this information is just a
;; maximum wake-up time, but a more sophisticated sandman might
;; support file-descriptor activity. Event implementations expect a
;; sandman that provides specific functionality, so all sandman
;; implementations need to support time.

;; All sandman functions are called in atomic mode.

;; See also "sandman-struct.rkt".

(provide sandman-merge-timeout
         sandman-merge-exts
         sandman-add-sleeping-thread!
         sandman-remove-sleeping-thread!
         sandman-poll
         sandman-sleep
         sandman-get-wakeup-handle
         sandman-wakeup
         sandman-any-sleepers?
         sandman-sleepers-external-events

         current-sandman)

;; in atomic mode
(define (sandman-merge-timeout exts timeout)
  ((sandman-do-merge-timeout the-sandman) exts timeout))

;; in atomic mode
(define (sandman-merge-exts a-exts b-exts)
  ((sandman-do-merge-external-event-sets the-sandman) a-exts b-exts))

;; in atomic mode
(define (sandman-add-sleeping-thread! th exts)
  ((sandman-do-add-thread! the-sandman) th exts))

;; in atomic mode
(define (sandman-remove-sleeping-thread! th h)
  ((sandman-do-remove-thread! the-sandman) th h))

;; in atomic mode
;; The `thread-wakeup` callback can be called with #f
;; to indicate that a thread was potentially woken up
;; some other way, such as by a semaphore post
(define (sandman-poll thread-wakeup)
  ((sandman-do-poll the-sandman) thread-wakeup))

;; in atomic mode
(define (sandman-sleep exts)
  ((sandman-do-sleep the-sandman) exts))

;; potentially in atomic mode
(define (sandman-get-wakeup-handle)
  ((sandman-do-get-wakeup the-sandman)))

;; potentially in atomic mode
(define (sandman-wakeup h)
  ((sandman-do-wakeup the-sandman) h))

;; in atomic mode
(define (sandman-any-sleepers?)
  ((sandman-do-any-sleepers? the-sandman)))

;; in atomic mode
(define (sandman-sleepers-external-events)
  ((sandman-do-sleepers-external-events the-sandman)))

(define-place-local waiting-threads '())
(define-place-local awoken-threads '())

;; ----------------------------------------
;; Default sandman implementation

;; A tree mapping times (in milliseconds) to a hash table of threads
;; to wake up at that time
(define-place-local sleeping-threads empty-tree)

(define (min* a-sleep-until b-sleep-until)
  (if (and a-sleep-until b-sleep-until)
      (min a-sleep-until b-sleep-until)
      (or a-sleep-until b-sleep-until)))

;; Sandman should not have place-local state itself, but
;; it can access place-local state that's declared as such.
(define the-default-sandman
  (sandman
   ;; sleep
   (lambda (timeout-at)
     (host:sleep (max 0.0 (/ (- (or timeout-at (distant-future)) (current-inexact-monotonic-milliseconds)) 1000.0))))

   ;; poll
   (lambda (wakeup)
     (unless (tree-empty? sleeping-threads)
       (define-values (timeout-at threads) (tree-min sleeping-threads))
       (when (timeout-at . <= . (current-inexact-monotonic-milliseconds))
         (unless (null? threads)
           (for ([t (in-hash-keys threads)])
             (wakeup t))))))

   ;; get-wakeup-handle
   (lambda ()
     (host:get-wakeup-handle))

   ;; wakeup
   (lambda (h)
     (host:wakeup h))

   ;; any-sleepers?
   (lambda ()
     (not (tree-empty? sleeping-threads)))

   ;; sleepers-external-events
   (lambda ()
     (and (not (tree-empty? sleeping-threads))
          (let-values ([(timeout-at threads) (tree-min sleeping-threads)])
            timeout-at)))
     
   ;; add-thread!
   (lambda (t sleep-until)
     (set! sleeping-threads
           (tree-set sleeping-threads
                     sleep-until
                     (hash-set (or (tree-ref sleeping-threads sleep-until <)
                                   #hasheq())
                               t
                               #t)
                     <))
     sleep-until)
   ;; remove-thread!
   (lambda (t sleep-until)
     (define threads (tree-ref sleeping-threads sleep-until <))
     (unless threads (internal-error "thread not found among sleeping threads"))
     (define new-threads (hash-remove threads t))
     (set! sleeping-threads
           (if (zero? (hash-count new-threads))
               (tree-remove sleeping-threads sleep-until <)
               (tree-set sleeping-threads sleep-until new-threads <))))

   ;; merge-exts
   (lambda (a-sleep-until b-sleep-until)
     (min* a-sleep-until b-sleep-until))
   
   ;; merge-timeout
   (lambda (sleep-until timeout-at)
     (if sleep-until
         (min sleep-until timeout-at)
         timeout-at))
   ;; extract-timeout
   (lambda (sleep-until) sleep-until)))

(void (current-sandman the-default-sandman))

;; Compute an approximation to infinity:
(define (distant-future)
  (+ (current-inexact-monotonic-milliseconds)
     (* 1000.0 60 60 24 365)))
