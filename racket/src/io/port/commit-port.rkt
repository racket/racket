#lang racket/base
(require racket/fixnum
         "../common/class.rkt"
         "port.rkt"
         "input-port.rkt"
         "lock.rkt"
         "commit-manager.rkt")

(provide commit-input-port)

(class commit-input-port #:extends core-input-port
  #:field
  [progress-sema #f]
  [commit-manager #f]

  #:static
  ;; with lock held, and in atomic mode if a progress-evt
  ;; was created for the port
  [progress!
   (lambda ()
     (when progress-sema
       (semaphore-post progress-sema)
       (set! progress-sema #f)
       (send commit-input-port this no-more-atomic-for-progress)))]

  #:public
  ;; with lock held
  [no-more-atomic-for-progress
   (lambda ()
     (unless closed-sema
       (port-lock-require-atomic! this #f)))]

  #:static
  ;; with lock held [can leave locked mode temporarily]
  ;; and in atomic mode if there's a progress-evt for
  ;; the port --- and there's only a commit manager
  ;; if there's a progress-evt.
  ;; After this function returns, complete any commit-changing work
  ;; before leaving atomic mode again.
  [pause-waiting-commit
   (lambda ()
     (when commit-manager
       (merely-atomically
        this
        (commit-manager-pause commit-manager))))]

  ;; with locked held [can leave locked mode temporarily]
  ;; and in atomic mode, since there's a progress-evt
  [wait-commit
   (lambda (progress-evt ext-evt finish)
     (cond
       [(and (not commit-manager)
             ;; Try shortcut:
             (not (sync/timeout 0 progress-evt))
             (sync/timeout 0 ext-evt))
        (merely-atomically
         this
         (finish))
        #t]
       [else
        ;; General case to support blocking and potentially multiple
        ;; committing threads:
        (unless commit-manager
          (set! commit-manager (make-commit-manager)))
        (merely-atomically
         this
         (commit-manager-wait commit-manager progress-evt ext-evt finish))]))]

  ;; with lock held
  [make-progress-evt
   (lambda ()
     (unless progress-sema
       (set! progress-sema (make-semaphore))
       (port-lock-require-atomic! this #t))
     (semaphore-peek-evt progress-sema))])
