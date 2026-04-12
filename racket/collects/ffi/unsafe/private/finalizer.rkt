#lang racket/base
(require (only-in '#%foreign
                  make-late-will-executor)
         (only-in '#%unsafe
                  unsafe-thread-at-root
                  unsafe-start-uninterruptible
                  unsafe-end-uninterruptible))

(provide register-finalizer)

(define killer-thread #f)

(define register-finalizer
  ;; We bind `killer-executor' as a location variable, instead of a module
  ;; variable, so that the loop for `killer-thread' doesn't have a namespace
  ;; (via a prefix) in its continuation:
  (let ([killer-executor (make-late-will-executor)])
    ;; The "late" kind of will executor (for `killer-executor') is
    ;; provided by '#%foreign, and it doesn't get GC'ed if any
    ;; finalizers are attached to it (while the normal kind can get
    ;; GCed even if a thread that is otherwise inaccessible is blocked
    ;; on the executor).  Also it registers level-2 finalizers (which
    ;; are run after non-late weak boxes are cleared).
    (lambda (obj finalizer)
      (unless killer-thread
        (unsafe-start-uninterruptible)
        (unless killer-thread
          ;; We need to make a thread that runs in a privildged custodian and
          ;; that doesn't retain the current namespace --- either directly
          ;; or indirectly through some parameter setting in the current thread.
          ;; There's a race if multiple parallel threads try to register the
          ;; first finalizer, but that's ok, because multiple finalizer threads
          ;; should be fine.
          (let ([cweh #f]) ; <- avoids a reference to a module-level binding
            (set! cweh call-with-exception-handler)
            (set! killer-thread
                  (unsafe-thread-at-root
                   (lambda ()
                     (define logger (current-logger))
                     (let retry-loop ()
                       (call-with-continuation-prompt
                        (lambda ()
                          (cweh
                           (lambda (exn)
                             (log-message logger
                                          'error
                                          (if (exn? exn)
                                              (exn-message exn)
                                              (format "~s" exn))
                                          #f)
                             (abort-current-continuation
                              (default-continuation-prompt-tag)
                              void))
                           (lambda ()
                             (let loop () (will-execute killer-executor) (loop))))))
                       (retry-loop)))))))
        (unsafe-end-uninterruptible))
      (will-register killer-executor obj finalizer))))
