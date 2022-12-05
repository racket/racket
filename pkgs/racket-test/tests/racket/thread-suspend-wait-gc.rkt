#lang racket/base

;; Make sure that when an otherwise inaccessible thread is created
;; with `thread/suspend-to-kill` and cannot make further progress,
;; then threads waiting on that thread can be GCed --- on the grounds
;; that a custodian cannot trigger termination in the same way that
;; it can with `thread`.

(for ([replace-evt (if (eq? 'racket (system-type 'vm))
                       ;; BC: no GC for threads blocked on `replace-evt`
                       (list (lambda (e p) e))
                       ;; CS: try with and without `replace-evt`
                       (list replace-evt (lambda (e p) e)))])
  (for ([mode '(single multi)])
    (let loop ([max-use 0] [same-count 0] [tries 0])
      (collect-garbage)
      (define use (current-memory-use))
      (printf "~s: ~s ~s\n" mode use (<= use max-use))
      (define new-same-count (if (<= use max-use)
                                 (+ same-count 1)
                                 0))
      (unless (= new-same-count 3)
        (when (= tries 30)
          (error "didn't reach memory-use fixpoint"))
        (for ([i 300])
          (thread/suspend-to-kill
           (lambda ()
             ;; see "thread-kill-wait-gc.rkt"
             (define outer-t (current-thread))
             (define sub-t (and (not (eq? mode 'single))
                                (thread/suspend-to-kill
                                 (lambda () (thread-wait outer-t)))))
             (sync (replace-evt (choice-evt
                                 (thread-dead-evt (if (eq? mode 'single)
                                                      (current-thread)
                                                      sub-t))
                                 (make-semaphore))
                                void)
                   (make-semaphore))))
          (sync (system-idle-evt)))
        (loop (max use max-use) new-same-count (+ tries 1))))))

;; Symmetric for a thread created with `thread` and waiting
;; via `thread-suspend-evt`:

(for ([replace-evt (if (eq? 'racket (system-type 'vm))
                       ;; BC: no GC for threads blocked on `replace-evt`
                       (list (lambda (e p) e))
                       ;; CS: try with and without `replace-evt`
                       (list replace-evt (lambda (e p) e)))])
  (for ([mode '(single multi)])
    (let loop ([max-use 0] [same-count 0] [tries 0])
      (collect-garbage)
      (define use (current-memory-use))
      (printf "~s: ~s ~s\n" mode use (<= use max-use))
      (define new-same-count (if (<= use max-use)
                                 (+ same-count 1)
                                 0))
      (unless (= new-same-count 3)
        (when (= tries 30)
          (error "didn't reach memory-use fixpoint"))
        (for ([i 300])
          (thread
           (lambda ()
             ;; see "thread-kill-wait-gc.rkt"
             (define outer-t (current-thread))
             (define sub-t (and (not (eq? mode 'single))
                                (thread
                                 (lambda () (sync (thread-suspend-evt outer-t))))))
             (sync (replace-evt (choice-evt
                                 (thread-suspend-evt (if (eq? mode 'single)
                                                         (current-thread)
                                                         sub-t))
                                 (make-semaphore))
                                void)
                   (make-semaphore))))
          (sync (system-idle-evt)))
        (loop (max use max-use) new-same-count (+ tries 1))))))
