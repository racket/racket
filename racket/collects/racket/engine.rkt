#lang racket/base

;; Library for engines: preemptable processes

(require racket/contract/base)

(provide
 engine?
 (contract-out (engine (((any/c . -> . any) . -> . any) . -> . engine?))
               (engine-run ((or/c evt? real?) engine? . -> . boolean?))
               (engine-result (engine? . -> . any))
               (engine-kill (engine? . -> . any))))

;; An X-engine-object is
;; (make-engine-object thread semaphore channel channel X)
(define-struct engine-object (worker can-stop-lock done-ch ex-ch result name)
  #:mutable)

;; engine : ((bool ->) -> X) -> X-engine-object
(define (engine f)
  ;;(printf "2. new engine\n")
  (let* ([can-stop-lock (make-semaphore 1)]
         [done-ch (make-channel)]
         [ex-ch (make-channel)]
         [proceed-sema (make-semaphore)]
         [stop-enabled? #t]
         [enable-stop
          (lambda (enable?)
            ;;(printf "3. enabling ~a\n" enable?)
            (cond
             [(and enable? (not stop-enabled?))
              (semaphore-post can-stop-lock)
              (set! stop-enabled? #t)]
             [(and (not enable?) stop-enabled?)
              (semaphore-wait can-stop-lock)
              (set! stop-enabled? #f)])
            ;;(printf "3. finished enabling\n")
            )]
         [tid (thread (lambda ()
                        (semaphore-wait proceed-sema)
                        ;;(printf "3. creating engine thread\n")
                        (with-handlers ([(lambda (exn) #t)
                                         (lambda (exn)
                                           (enable-stop #t)
                                           (channel-put ex-ch exn))])
                          (let ([v (f enable-stop)])
                            (enable-stop #t)
                            (channel-put done-ch v)))))])
    (begin0 (make-engine-object tid can-stop-lock done-ch ex-ch #f 
                                (and (object-name f)
                                     (symbol->string (object-name f))))
      (thread-suspend tid)
      (semaphore-post proceed-sema))))

;; engine : real-number X-engine-object -> bool
(define (engine-run timeout w)
  (log "engine-run called" w)
  (if (engine-object-worker w)
    (let ([can-stop-lock (engine-object-can-stop-lock w)]
          [worker (engine-object-worker w)])
      (thread-resume worker)
      (dynamic-wind
        void
        ;; Let the co-routine run...
        (lambda ()
          (sync (choice-evt (wrap-evt (if (evt? timeout)
                                          timeout
                                          (alarm-evt (+ timeout (current-inexact-milliseconds))))
                                      (lambda (x)
                                        (log "alarm woke up, waiting to suspend engine" w)
                                        (semaphore-wait can-stop-lock)
                                        (log "suspending engine" w)
                                        (thread-suspend worker)
                                        (semaphore-post can-stop-lock)
                                        #f))
                            (wrap-evt (engine-object-done-ch w)
                                      (lambda (res)
                                        (log "engine done" w)
                                        (set-engine-object-result! w res)
                                        (engine-kill w)
                                        #t))
                            (wrap-evt (engine-object-ex-ch w)
                                      (lambda (exn)
                                        (log "engine raised exn" w)
                                        (engine-kill w)
                                        (raise exn))))))
        ;; In case we escape through a break:
        (lambda ()
          (when (thread-running? worker)
            (log "engine escape via break" w)
            (semaphore-wait can-stop-lock)
            (thread-suspend worker)
            (semaphore-post can-stop-lock)))))
    #t))

;; engine-result : X-engine-object -> X
(define (engine-result w)
  (engine-object-result w))

;; engine-kill : X-engine-object ->
(define (engine-kill w)
  (set-engine-object-can-stop-lock! w #f)
  (set-engine-object-done-ch! w #f)
  (set-engine-object-ex-ch! w #f)
  (when (engine-object-worker w)
    (kill-thread (engine-object-worker w))
    (set-engine-object-worker! w #f)))

(define (engine? x)
  (engine-object? x))

(define engine-logger (make-logger 'racket/engine (current-logger)))
(define-syntax-rule
  (log msg w)
  (when (log-level? engine-logger 'debug)
    (do-log msg w)))
(define (do-log msg w)
  (define name (engine-object-name w))
  (log-message engine-logger 'debug 
               (if name
                   (string-append "racket/engine: " name ": " msg)
                   (string-append "racket/engine: " msg))
               (engine-info (current-inexact-milliseconds)
                            name)))
(struct engine-info (msec name) #:prefab)

