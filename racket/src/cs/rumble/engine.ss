;; Inspried by Chez's engine API, but
;;   - works with delimited-continuations extensions in "control.ss"
;;   - doesn't run winders when suspending or resuming an engine
;;   - accepts an extra "prefix" argument to run code within an engine
;;     just before resuming the engine's continuation
;;   - supports direct engine-to-engine transition instead of a
;;     back-and-forth between an engine scheduler

;; Don't mix Chez engines with this implementation, because we take
;; over the timer.

(define-record-type engine-cell-state
  (fields thread-cell-values init-break-enabled-cell))
(define empty-engine-cell-state (make-engine-cell-state #f #f))

(define-virtual-register current-engine-complete-or-expire #f)
(define-virtual-register current-engine-cell-state empty-engine-cell-state)

(define (set-ctl-c-handler! proc)
  (keyboard-interrupt-handler (case-lambda
                               [() (proc 'break)]
                               [(kind) (proc kind)])))
(define (get-ctl-c-handler)
  (keyboard-interrupt-handler))

(define (engine-exit v)
  (place-exit v))

(define (set-engine-exit-handler! proc)
  (set! engine-exit proc))

(define (currently-in-engine?)
  (current-engine-complete-or-expire))

;; An engine is represented by a procedure that takes three arguments, where the
;; procedure must be tail-called either within `call-with-engine-completion` or
;; in an engine call's `complete-or-expire` callback:
;;   * ticks: number of ticks to run before exire
;;   * prefix: thunk to invoke just before continuing (counts toward ticks)
;;   * complete-or-expire: callback that accepts 3 arguments,
;;      - engine or #f: an engine if the original `thunk` has not yet returned
;;      - list or #f: a list if the original `thunk` has returned values
;;      - remining-ticks: a number of ticks leftover due to complete or `(engine-block)`
;;     where the callback must end by tail-calling another engine procedure or
;;     the procedure provided by `call-with-engine-completion`
(define (make-engine thunk          ; can return any number of values
                     prompt-tag     ; prompt to wrap around call to `thunk`
                     abort-handler  ; handler for that prompt
                     init-break-enabled-cell ; default break-enable cell
                     empty-config?) ; whether to clone the current parameterization
  (let ([paramz (if empty-config?
                    empty-parameterization
                    (current-parameterization))])
    (create-engine empty-metacontinuation
                   (lambda (prefix)
                     ;; Set parameterize for `prefix` to use:
                     (with-continuation-mark
                         parameterization-key paramz
                       (begin
                         (prefix)
                         (call-with-values (lambda ()
                                             (call-with-continuation-prompt
                                              (lambda ()
                                                ;; Set parameterization again inside
                                                ;; the prompt tag, so it goes along with
                                                ;; a captured continuation:
                                                (with-continuation-mark
                                                    parameterization-key paramz
                                                  (|#%app| thunk)))
                                              prompt-tag
                                              abort-handler))
                           engine-return))))
                   (make-engine-cell-state
                    (if empty-config?
                        (make-empty-thread-cell-values)
                        (new-engine-thread-cell-values))
                    init-break-enabled-cell))))

;; Internal: creates an engine procedure to be called within `call-with-engine-completion`
;; or from an engine procedure's `complete-or-expire` callback
(define (create-engine to-saves proc cell-state)
  (case-lambda
   ;; For `continuation-marks` and `engine-roots`:
   [() to-saves]
   ;; Normal engine case:
   [(ticks prefix complete-or-expire)
    (start-implicit-uninterrupted 'create)
    (apply-meta-continuation
     to-saves
     (lambda ()
       (current-engine-complete-or-expire complete-or-expire)
       (current-engine-cell-state cell-state)
       (timer-interrupt-handler engine-block-via-timer)
       (end-implicit-uninterrupted 'create)
       (set-timer ticks)
       (proc prefix)))]))

;; Captures the current metacontinuation as an engine runner, and calls `proc`
;; with a procedure to be tail-called from an engine procedure's `complete-or-expire`
;; callback to return to the metacontinuation
(define (call-with-engine-completion proc)
  (start-implicit-uninterrupted 'call-with-engine-completion)
  (call-with-current-metacontinuation
   (lambda (saves)
     (end-implicit-uninterrupted 'call-with-engine-completion)
     (let ([rh (reset-handler)]
           [ws (#%$current-winders)]
           [exns (current-exception-state)])
       (reset-handler engine-reset-handler)
       (#%$current-winders '())
       (current-exception-state (create-exception-state))
       (proc (lambda args
               (current-engine-complete-or-expire #f)
               (current-engine-cell-state empty-engine-cell-state)
               (apply-meta-continuation
                saves
                (lambda ()
                  (reset-handler rh)
                  (#%$current-winders ws)
                  (current-exception-state exns)
                  (#%apply values args)))))))))

(define (engine-reset-handler)
  (end-uninterrupted 'reset)
  (if (currently-in-engine?)
      (engine-return (void))
      (#%exit 1)))

(define (engine-block-via-timer)
  (cond
   [(current-in-uninterrupted)
    (pending-interrupt-callback engine-block/timeout)]
   [else
    (engine-block/timeout)]))

(define engine-block
  (case-lambda
   [(timeout?)
    (assert-not-in-uninterrupted 'engine-block)
    (timer-interrupt-handler void)
    (let ([complete-or-expire (current-engine-complete-or-expire)]
          [cell-state (current-engine-cell-state)]
          [remain-ticks (let ([n (set-timer 0)])
                          (if timeout? 0 n))])
      (unless complete-or-expire
        (error 'engine-block "not currently running an engine"))
      (start-implicit-uninterrupted 'engine-block)
      (call-with-current-metacontinuation
       (lambda (saves)
         (end-implicit-uninterrupted 'engine-block)
         (current-engine-complete-or-expire #f)
         (current-engine-cell-state empty-engine-cell-state)
         (complete-or-expire (create-engine saves
                                            (lambda (prefix) (prefix))
                                            cell-state)
                             #f
                             remain-ticks))))]
   [() (engine-block #f)]))

(define (engine-block/timeout)
  (engine-block #t))

(define (engine-timeout)
  (let ([can-block? (fx= 1 (disable-interrupts))])
    (enable-interrupts)
    (cond
     [can-block?
      (engine-block/timeout)]
     [else
      ;; Cause the timer to fire as soon as possible (i.e., as soon
      ;; as interrupts are enabled)
      (set-timer 1)])))

(define (engine-return . results)
  (assert-not-in-uninterrupted 'engine-return)
  (timer-interrupt-handler void)
  (let ([complete-or-expire (current-engine-complete-or-expire)])
    (unless complete-or-expire
      (error 'engine-return "not currently running an engine"))
    (let ([remain-ticks (set-timer 0)])
      (start-implicit-uninterrupted 'block)
      (call-with-current-metacontinuation
       (lambda (ignored-saves)
         (end-implicit-uninterrupted 'block)
         (current-engine-complete-or-expire #f)
         (current-engine-cell-state empty-engine-cell-state)
         (complete-or-expire #f results remain-ticks))))))

(define (engine-roots e)
  (let ([mc (e)])
    (cons e mc)))

(define (make-empty-thread-cell-values)
  (make-ephemeron-eq-hashtable))

(define-virtual-register root-thread-cell-values (make-empty-thread-cell-values))

(define original-thread-id (get-thread-id))

(define (current-engine-thread-cell-values)
  (or (engine-cell-state-thread-cell-values (current-engine-cell-state))
      (root-thread-cell-values)))

(define (set-current-engine-thread-cell-values! new-t)
  (let ([current-t (current-engine-thread-cell-values)])
    (with-interrupts-disabled
     (hash-table-for-each
      current-t
      (lambda (c v)
        (when (thread-cell-preserved? c)
          (hashtable-delete! current-t c))))
     (hash-table-for-each
      new-t
      (lambda (c v)
        (hashtable-set! current-t c v))))))

(define (new-engine-thread-cell-values)
  (let ([current-t (current-engine-thread-cell-values)]
        [new-t (make-ephemeron-eq-hashtable)])
    (when current-t
      (hash-table-for-each
       current-t
       (lambda (c v)
         (when (thread-cell-preserved? c)
           (hashtable-set! new-t c v)))))
    new-t))

(define (current-engine-init-break-enabled-cell none-v)
  (or (engine-cell-state-init-break-enabled-cell (current-engine-cell-state))
      none-v))
