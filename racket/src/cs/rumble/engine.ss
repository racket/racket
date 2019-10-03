;; Like Chez's engine API, but
;;   - works with delimited-continuations extensions in "control.ss"
;;   - doesn't run winders when suspending or resuming an engine
;;   - accepts an extra "prefix" argument to run code within an engine
;;     just before resuming the engine's continuation

;; Don't mix Chez engines with this implementation, because we take
;; over the timer.

(define-record engine-state (mc complete-or-expire thread-cell-values init-break-enabled-cell reset-handler))

(define-virtual-register current-engine-state #f)

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

;; An engine is repesented by a procedure that takes thee arguments:
;;   * ticks: number of ticks to run before exire
;;   * prefix: thunk to invoke just before continuing (counts toward ticks)
;;   * complete-or-expire: callback that accepts 3 arguments:
;;      - engine or #f: an engine if the original `thunk` has not yet returned
;;      - list or #f: a list if the original `thunk` has returned values
;;      - remining-ticks: a number of ticks leftover due to complete or `(engine-block)`
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
                   (if empty-config?
                       (make-empty-thread-cell-values)
                       (new-engine-thread-cell-values))
                   init-break-enabled-cell)))

(define (create-engine to-saves proc thread-cell-values init-break-enabled-cell)
  (case-lambda
   ;; For `continuation-marks`:
   [() to-saves]
   ;; Normal engine case:
   [(ticks prefix complete-or-expire)
    (start-implicit-uninterrupted 'create)
    ((swap-metacontinuation
      to-saves
      (lambda (saves)
        (current-engine-state (make-engine-state saves complete-or-expire thread-cell-values
                                                 init-break-enabled-cell (reset-handler)))
        (reset-handler engine-reset-handler)
        (timer-interrupt-handler engine-block-via-timer)
        (end-implicit-uninterrupted 'create)
        (set-timer ticks)
        (proc prefix))))]))

(define (engine-reset-handler)
  (end-uninterrupted 'reset)
  (if (current-engine-state)
      (engine-return (void))
      (chez:exit)))

(define (engine-block-via-timer)
  (cond
   [(current-in-uninterrupted)
    (pending-interrupt-callback engine-block/timeout)]
   [else
    (engine-block/timeout)]))
    
(define engine-block
  (case-lambda
   [(timeout?)
    (assert-not-in-uninterrupted)
    (timer-interrupt-handler void)
    (let ([es (current-engine-state)]
          [remain-ticks (if timeout?
                            0
                            (set-timer 0))])
      (unless es
        (error 'engine-block "not currently running an engine"))
      (reset-handler (engine-state-reset-handler es))
      (start-implicit-uninterrupted 'block)
      ;; Extra pair of parens around swap is to apply a prefix
      ;; function on swapping back in:
      ((swap-metacontinuation
        (engine-state-mc es)
        (lambda (saves)
          (end-implicit-uninterrupted 'block)
          (current-engine-state #f)
          (lambda () ; returned to the `swap-continuation` in `create-engine`
            ((engine-state-complete-or-expire es)
             (create-engine
              saves
              (lambda (prefix) prefix) ; returns `prefix` to the above "(("
              (engine-state-thread-cell-values es)
              (engine-state-init-break-enabled-cell es))
             #f
             remain-ticks))))))]
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
  (assert-not-in-uninterrupted)
  (timer-interrupt-handler void)
  (let ([es (current-engine-state)])
    (unless es
      (error 'engine-return "not currently running an engine"))
    (reset-handler (engine-state-reset-handler es))
    (let ([remain-ticks (set-timer 0)])
      (start-implicit-uninterrupted 'return)
      (swap-metacontinuation
       (engine-state-mc es)
       (lambda (saves)
         (current-engine-state #f)
         (end-implicit-uninterrupted 'return)
         (lambda () ; returned to the `swap-continuation` in `create-engine`
           ((engine-state-complete-or-expire es) #f results remain-ticks)))))))

(define (make-empty-thread-cell-values)
  (make-ephemeron-eq-hashtable))

(define-virtual-register root-thread-cell-values (make-empty-thread-cell-values))

(define original-thread-id (get-thread-id))

(define (current-engine-thread-cell-values)
  (let ([es (current-engine-state)])
    (if es
        (engine-state-thread-cell-values es)
        (root-thread-cell-values))))

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
  (let ([es (current-engine-state)])
    (if es
        (engine-state-init-break-enabled-cell es)
        none-v)))
