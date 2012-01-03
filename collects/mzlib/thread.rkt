
(module thread mzscheme
  (require "kw.rkt" "contract.rkt")

  (provide run-server
           consumer-thread)

  #|
  t accepts a function, f, and creates a thread. It returns the thread and a
  function, g. When g is applied it passes its argument to f, and evaluates
  the call of f in the time of the thread that was created. Calls to g do not
  block.
  |#

  (define/kw (consumer-thread f #:optional [init void])
    (unless (procedure? f) (raise-type-error 'consumer-thread "procedure" f))
    (let ([sema (make-semaphore 0)]
          [protect (make-semaphore 1)]
          [front-state null]
          [back-state null])
      (values
       (thread
        (letrec ([loop
                  (lambda ()
                    (semaphore-wait sema)
                    (let ([local-state
                           (begin
                             (semaphore-wait protect)
                             (if (null? back-state)
                               (let ([new-front (reverse front-state)])
                                 (set! back-state (cdr new-front))
                                 (set! front-state null)
                                 (semaphore-post protect)
                                 (car new-front))
                               (begin0
                                   (car back-state)
                                 (set! back-state (cdr back-state))
                                 (semaphore-post protect))))])
                      (apply f local-state))
                    (loop))])
          (lambda ()
            (init)
            (loop))))
       (procedure-reduce-arity
        (lambda new-state
          (semaphore-wait protect)
          (set! front-state (cons new-state front-state))
          (semaphore-post protect)
          (semaphore-post sema))
        (procedure-arity f)))))

  (define/kw (run-server port-number handler connection-timeout
                         #:optional
                         [handle-exn void]
                         [tcp-listen tcp-listen]
                         [tcp-close tcp-close]
                         [tcp-accept tcp-accept]
                         [tcp-accept/enable-break tcp-accept/enable-break])
    (let ([l (tcp-listen port-number 5 #t)]
          [can-break? (break-enabled)])
      (dynamic-wind
          void
          (lambda ()
            ;; All connections should use the same parameterization,
            ;;  to facilitate transferring continuations from one
            ;;  connection to another:
            (let ([paramz (current-parameterization)])
              ;; Loop to handle connections:
              (let loop ()
                ;; Introducing this thread causes PR12443 to no longer fail.
                
                ;; The Web Server will definitely kill the custodian
                ;; associated with the resources of the connection. I
                ;; think what is going on is that the loop here is
                ;; attached to one of these custodians (eventually)
                ;; and then the listening loop thread gets killed
                ;; too. This patch basically just disconnects the loop
                ;; from the new custodian. The error reported in the
                ;; PR still shows up, but it has no effect on the
                ;; response time/etc, whereas before it would stop
                ;; listening and 'ab' would fail.
                (thread-wait
                 (thread
                  (λ ()
                     (with-handlers 
                      ([exn:fail:network? handle-exn])
                      ;; Make a custodian for the next session:
                      (let ([c (make-custodian)])
                        (parameterize
                         ([current-custodian c])
                         ;; disable breaks during session set-up...
                         (parameterize-break 
                          #f
                          ;; ... but enable breaks while blocked on an accept:
                          (let-values ([(r w) ((if can-break?
                                                   tcp-accept/enable-break
                                                   tcp-accept)
                                               l)])
                            ;; Handler thread:
                            (let ([t 
                                   (thread 
                                    (lambda ()
                                      ;; First, install the parameterization
                                      ;;  used for all connections:
                                      (call-with-parameterization
                                       paramz
                                       (lambda ()
                                         ;; Install this connection's custodian
                                         ;;  for this thread in the shared
                                         ;;  parameterization:
                                         (current-custodian c)
                                         ;; Enable breaking:
                                         (when can-break?
                                               (break-enabled #t))
                                         ;; Call the handler
                                         (handler r w)))))])
                              ;; Clean-up and timeout thread:
                              (thread 
                               (lambda ()
                                 (sync/timeout connection-timeout t)
                                 (when (thread-running? t)
                                       ;; Only happens if connection-timeout is not #f
                                       (break-thread t))
                                 (sync/timeout connection-timeout t)
                                 (custodian-shutdown-all c))))))))))))
                (loop))))
          (lambda () (tcp-close l)))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Couroutine
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; An X-coroutine-object is
  ;; (make-coroutine-object thread semaphore channel channel X)
  (define-struct coroutine-object (worker can-stop-lock done-ch ex-ch result))

  ;; coroutine : ((bool ->) -> X) -> X-coroutine-object
  (define (coroutine f)
    ;;(printf "2. new coroutine\n")
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
                          ;;(printf "3. creating coroutine thread\n")
                          (with-handlers ([(lambda (exn) #t)
                                           (lambda (exn)
                                             (enable-stop #t)
                                             (channel-put ex-ch exn))])
                            (let ([v (f enable-stop)])
                              (enable-stop #t)
                              (channel-put done-ch v)))))])
      (begin0 (make-coroutine-object tid can-stop-lock done-ch ex-ch #f)
        (thread-suspend tid)
        (semaphore-post proceed-sema))))

  ;; coroutine : real-number X-coroutine-object -> bool
  (define (coroutine-run timeout w)
    (if (coroutine-object-worker w)
      (let ([can-stop-lock (coroutine-object-can-stop-lock w)]
            [worker (coroutine-object-worker w)])
        #;(printf "2. starting coroutine\n")
        (thread-resume worker)
        (dynamic-wind
          void
          ;; Let the co-routine run...
          (lambda ()
            (sync (choice-evt (wrap-evt (if (evt? timeout)
                                            timeout
                                            (alarm-evt (+ timeout (current-inexact-milliseconds))))
                                        (lambda (x)
                                          #;(printf "2. alarm-evt\n")
                                          (semaphore-wait can-stop-lock)
                                          (thread-suspend worker)
                                          (semaphore-post can-stop-lock)
                                          #f))
                              (wrap-evt (coroutine-object-done-ch w)
                                        (lambda (res)
                                          #;(printf "2. coroutine-done-evt\n")
                                          (set-coroutine-object-result! w res)
                                          (coroutine-kill w)
                                          #t))
                              (wrap-evt (coroutine-object-ex-ch w)
                                        (lambda (exn)
                                          #;(printf "2. ex-evt\n")
                                          (coroutine-kill w)
                                          (raise exn))))))
          ;; In case we escape through a break:
          (lambda ()
            (when (thread-running? worker)
              (semaphore-wait can-stop-lock)
              (thread-suspend worker)
              (semaphore-post can-stop-lock)))))
      #t))

  ;; coroutine-result : X-coroutine-object -> X
  (define (coroutine-result w)
    (coroutine-object-result w))

  ;; coroutine-kill : X-coroutine-object ->
  (define (coroutine-kill w)
    (set-coroutine-object-can-stop-lock! w #f)
    (set-coroutine-object-done-ch! w #f)
    (set-coroutine-object-ex-ch! w #f)
    (when (coroutine-object-worker w)
      (kill-thread (coroutine-object-worker w))
      (set-coroutine-object-worker! w #f)))

  (define (coroutine? x)
    (coroutine-object? x))

  (provide coroutine?)
  (provide/contract
   (coroutine (((any/c . -> . any) . -> . any) . -> . coroutine?))
   (coroutine-run ((or/c evt? real?) coroutine? . -> . boolean?))
   (coroutine-result (coroutine? . -> . any))
   (coroutine-kill (coroutine? . -> . any))))
