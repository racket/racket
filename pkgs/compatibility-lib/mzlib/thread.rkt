
(module thread mzscheme
  (require mzlib/kw mzlib/contract racket/engine)

  (provide run-server
           consumer-thread

           (rename engine? coroutine?)
           (rename engine coroutine)
           (rename engine-run coroutine-run)
           (rename engine-result coroutine-result)
           (rename engine-kill coroutine-kill))

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
                                      ;; Prevent the handler from
                                      ;; killing this custodian, by
                                      ;; creating an intermediary,
                                      ;; but child custodian
                                      (parameterize ([current-custodian 
                                                      (make-custodian)])
                                        ;; Call the handler
                                        (handler r w))))))])
                           ;; Clean-up and timeout thread:
                           (thread 
                            (lambda ()
                              (sync/timeout connection-timeout t)
                              (when (thread-running? t)
                                ;; Only happens if connection-timeout is not #f
                                (break-thread t))
                              (sync/timeout connection-timeout t)
                              (custodian-shutdown-all c)))))))))
                (loop))))
          (lambda () (tcp-close l))))))
