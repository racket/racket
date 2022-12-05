#lang racket/base

;; Make sure that a thread killed while blocked on a
;; thread-dead-evt (which means that an internal
;; semaphore is being retained strongly) does not
;; end up leaking memory (i.e., the semaphore goes back
;; to a weak reference in its custodian)

(for ([mode '(single multi custodian multi-custodian nonkill)])
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
      (for ([i 3000])
        (define c (if (memq mode '(custodian multi-custodian))
                      (make-custodian)
                      (current-custodian)))
        (define cs (if (eq? mode 'multi-custodian)
                       (list (make-custodian) (make-custodian))
                       null))
        (define (add-custodians t)
          (when (eq? mode 'multi-custodian)
            (for/list ([c (in-list (cons (make-custodian) cs))])
              (thread-resume t c)))
          t)
        (define other-t #f)
        (define t
          (parameterize ([current-custodian c])
            (add-custodians
             (thread (lambda ()
                       ;; intent is to include a pattern that is unlikely
                       ;; to be detected as impossible to make progress
                       ;; (even if that detection is improved after the
                       ;; time of writing):
                       (define outer-t (current-thread))
                       (define sub-t (and (not (eq? mode 'single))
                                          (add-custodians
                                           (thread (lambda ()
                                                     (if (eq? mode 'nonkill)
                                                         (void)
                                                         (thread-wait outer-t)))))))
                       (set! other-t sub-t)
                       (sync (replace-evt (choice-evt
                                           (thread-dead-evt (if (eq? mode 'single)
                                                                (current-thread)
                                                                sub-t))
                                           (make-semaphore))
                                          void)
                             (make-semaphore)))))))
        (sync (system-idle-evt))
        (cond
          [(eq? mode 'nonkill) (sync t)]
          [(eq? mode 'custodian)
           (custodian-shutdown-all c)]
          [else
           (kill-thread t)
           (unless (eq? mode 'single)
             (kill-thread other-t))]))
      (loop (max use max-use) new-same-count (+ tries 1)))))
