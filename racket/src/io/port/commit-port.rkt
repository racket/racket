#lang racket/base
(require racket/fixnum
         "../common/class.rkt"
         "port.rkt"
         "input-port.rkt"
         "commit-manager.rkt")

(provide commit-input-port)

(class commit-input-port #:extends core-input-port
  #:field
  [progress-sema #f]
  [commit-manager #f]

  #:static
  ;; in atomic mode
  [progress!
   (lambda ()
     (when progress-sema
       (semaphore-post progress-sema)
       (set! progress-sema #f)))]

  ;; in atomic mode [can leave atomic mode temporarily]
  ;; After this function returns, complete any commit-changing work
  ;; before leaving atomic mode again.
  [pause-waiting-commit
   (lambda ()
     (when commit-manager
       (commit-manager-pause commit-manager)))]

  ;; in atomic mode [can leave atomic mode temporarily]
  [wait-commit
   (lambda (progress-evt ext-evt finish)
     (cond
       [(and (not commit-manager)
             ;; Try shortcut:
             (not (sync/timeout 0 progress-evt))
             (sync/timeout 0 ext-evt))
        (finish)
        #t]
       [else
        ;; General case to support blocking and potentially multiple
        ;; commiting threads:
        (unless commit-manager
          (set! commit-manager (make-commit-manager)))
        (commit-manager-wait commit-manager progress-evt ext-evt finish)]))]

  ;; in atomic mode
  [make-progress-evt
   (lambda ()
     (unless progress-sema
       (set! progress-sema (make-semaphore))) 
     (semaphore-peek-evt progress-sema))])
