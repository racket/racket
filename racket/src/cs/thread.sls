(library (thread)
  (export)
  (import (rename (chezpart)
                  [define chez:define])
          (rename (rumble)
                  [rumble:break-enabled-key break-enabled-key]
                  ;; Remapped to place-local register operations:
                  [unsafe-make-place-local rumble:unsafe-make-place-local]
                  [unsafe-place-local-ref rumble:unsafe-place-local-ref]
                  [unsafe-place-local-set! rumble:unsafe-place-local-set!]
                  ;; These are extracted via `#%linklet`:
                  [make-engine rumble:make-engine]
                  [make-engine-thread-cell-state rumble:make-engine-thread-cell-state]
                  [set-engine-thread-cell-state! rumble:set-engine-thread-cell-state!]
                  [engine-timeout rumble:engine-timeout]
                  [engine-return rumble:engine-return]
                  [engine-roots rumble:engine-roots]
                  [call-with-engine-completion rumble:call-with-engine-completion]
                  [call-with-current-continuation-roots rumble:call-with-current-continuation-roots]
                  [pthread? rumble:thread?]
                  [fork-place rumble:fork-place]
                  [place-get-inherit rumble:place-get-inherit]
                  [start-place rumble:start-place]
                  [fork-pthread rumble:fork-thread]
                  [get-initial-pthread rumble:get-initial-pthread]
                  [current-place-roots rumble:current-place-roots]
                  [call-as-asynchronous-callback rumble:call-as-asynchronous-callback]
                  [post-as-asynchronous-callback rumble:post-as-asynchronous-callback]
                  [set-ctl-c-handler! rumble:set-ctl-c-handler!]
                  [set-break-enabled-transition-hook! rumble:set-break-enabled-transition-hook!]
                  [set-reachable-size-increments-callback! rumble:set-reachable-size-increments-callback!]
                  [set-custodian-memory-use-proc! rumble:set-custodian-memory-use-proc!]
                  [set-immediate-allocation-check-proc! rumble:set-immediate-allocation-check-proc!]
                  [continuation-current-primitive rumble:continuation-current-primitive]))

  (include "place-register.ss")
  (define-place-register-define place:define thread-register-start thread-register-count)
  
  ;; Special handling of `current-atomic` to use the last virtual register, and
  ;; similarr for other. We rely on the fact that the register's default value is 0
  ;; or the rumble layer installs a suitable default. Also, force inline a few
  ;; functions and handle other special cases. Note that the implementation of
  ;; `start-atomic` and `end-atomic` rely on some specific parameters being thread
  ;; registers so that the functions can be safely called from any Scheme thread.
  (define-syntax (define stx)
    (let ([define-as-virtual-register
            (lambda (stx n)
              (with-syntax ([(_ id _) stx]
                            [n (datum->syntax #'here n)])
                #'(define-syntax id
                    (syntax-rules ()
                      [(_) (virtual-register n)]
                      [(_ v) (set-virtual-register! n v)]))))])
      (syntax-case stx (current-atomic end-atomic-callback 1/current-future
                                       lambda make-pthread-parameter unsafe-make-place-local)
        ;; Recognize definition of `current-atomic`:
        [(_ current-atomic (make-pthread-parameter 0))
         (define-as-virtual-register stx current-atomic-virtual-register)]
        ;; Recognize definition of `end-atomic-callback`:
        [(_ end-atomic-callback (make-pthread-parameter 0))
         (define-as-virtual-register stx end-atomic-virtual-register)]
        ;; Recognize definition of `current-future`:
        [(_ 1/current-future (make-pthread-parameter #f))
         (define-as-virtual-register stx current-future-virtual-register)]
        ;; Force-inline atomicity-managing functions, at least within the core layers:
        [(_ id (lambda () expr ...))
         (#%memq (syntax->datum #'id) '(start-atomic end-atomic end-atomic/no-barrier-exit
                                                     future-barrier future-barrier-exit
                                                     start-uninterruptable end-uninterruptable
                                                     in-atomic-mode? not-atomic-mode?))
         #'(begin
             (define proc (let ([id (lambda () expr ...)]) id))
             (define-syntax (id stx)
               (syntax-case stx ()
                 [(_) #'(let () expr ...)]
                 [_ #'proc])))]
        ;; Workaround for redirected access of `unsafe-make-place-local` from #%pthread:
        [(_ alias-id unsafe-make-place-local) #'(begin)]
        ;; Chain to place-register handling:
        [(_ . rest) #'(place:define . rest)])))

  ;; This implementation of `sleep`, `get-wakeup-handle`, and `wakeup` is relevant
  ;; only for running the places part of the thread demo. The relevant callbacks get
  ;; replaced by the "io" layer to use rktio-based functions.
  (define sleep-interrupted (rumble:unsafe-make-place-local #f))
  (define (sleep secs)
    (let ([isecs (inexact->exact (floor secs))]
          [zero-secs (make-time 'time-duration 0 0)]
          [pause-secs (make-time 'time-duration 100000 0)])
      (let loop ([all-secs (make-time 'time-duration
                                      (inexact->exact (floor (* (- secs isecs) 1e9)))
                                      isecs)])
        (unless (or (time<=? all-secs zero-secs)
                    (let ([b (rumble:unsafe-place-local-ref sleep-interrupted)])
                      (and b (unbox b))))
          (#%sleep pause-secs)
          (loop (subtract-duration all-secs pause-secs))))
      (let ([b (rumble:unsafe-place-local-ref sleep-interrupted)])
        (when b
          (set-box! b #f)))))
  (define (get-wakeup-handle)
    (let ([b (rumble:unsafe-place-local-ref sleep-interrupted)])
      (or b
          (begin
            ;; There's a race condition here.. Avoid triggering it
            ;; in the thread demo.
            (rumble:unsafe-place-local-set! sleep-interrupted (box #f))
            (get-wakeup-handle)))))
  (define (wakeup b)
    (set-box! b #t))

  (define (get-system-stats)
    (values (collections)))

  (define (internal-error s)
    (#%printf "internal-error: ~a\n" s)
    (#%call/cc
     (lambda (k)
       (let loop ([k k] [offset #f] [n 0])
         (cond
           [(or (not (#%$continuation? k))
                (eq? k #%$null-continuation))
            (void)]
           [(fx= n 100) (void)]
           [else
            (let* ([name (let* ([c (if offset
                                       (#%$continuation-stack-return-code k offset)
                                       (#%$continuation-return-code k))]
                                [n (#%$code-name c)])
                           n)])
              (#%printf " at ~s\n" name)
              (let* ([offset (if offset
                                 (fx- offset (#%$continuation-stack-return-frame-words k offset))
                                 (fx- (#%$continuation-stack-clength k)
                                      (#%$continuation-return-frame-words k)))]
                     [offset (if (fx= offset 0) #f offset)])
                (loop (if offset k (#%$continuation-link k))
                      offset
                      (fx+ n 1))))]))))
    (#%exit 1))

  ;; Lock-order assertion --- using a vrtual register that is otherwise used
  ;; for a check in the foreign interface, so enable assertion checking by
  ;; flipping the mode in "rumble/pthread.ss".
  ;; The lock order checking here spans the "thread" and "io" layers.
  (meta-if-foreign-checking
   (begin
     (define-syntax (assert-push-lock-level! stx)
       #'(void))
     (define-syntax (assert-pop-lock-level! stx)
       #'(void)))
   (begin
     (define (assert-push-lock-level! v)
       (let ([status (current-lock-status)])
         (when (case (and (pair? status) (car status))
                 [(rktio) (memq v '(rktio port))] ; rktio is non-reentrant
                 [(rktio-sleep-relevant) (memq v '(rktio-sleep-relevant rktio port))] ; also non-reentrant
                 [(custodian) (and (memq v '(logger rktio-sleep-relevant rktio port))
                                   ;; ok to retake lock, even after others:
                                   (not (memq 'custodian status)))]
                 [(logger) (memq v '(custodian rktio-sleep-relevant rktio port))]
                 [else #f])
           (#%display (#%format "<< ~s >>\n" (cons v status)))
           (internal-error "misordered lock"))
         #;(#%display (#%format "+ ~s take ~s\n" (#%get-thread-id)  (cons v status)))
         (current-lock-status (cons v status))))
     (define (assert-pop-lock-level! v)
       #;(#%display (#%format "- ~s drop ~s\n" (#%get-thread-id)  (current-lock-status)))
       (current-lock-status (let loop ([status (current-lock-status)])
                              (cond
                                [(null? status)
                                 (#%display (#%format "<< ~s ~s >>\n" v (current-lock-status)))
                                 (internal-error "releasing unheld lock")]
                                [(eq? v (car status)) (cdr status)]
                                [else (cons (car status) (loop (cdr status)))]))))))
  (export assert-push-lock-level!
          assert-pop-lock-level!)

  (define (primitive-table key)
    (case key
      [(|#%pthread|)
       ;; Entries in the `#%pthread` table are referenced more
       ;; directly in "compiled/thread.scm". To make that work, the
       ;; entries need to be either primitives in all Racket
       ;; implementations or registered as built-in names with the
       ;; expander and listed in "primitive/internal.ss". And since
       ;; the table isn't going to be used, we skip creating it.
       (hasheq)
       #;
       (hasheq
        'make-pthread-parameter make-pthread-parameter
        'unsafe-root-continuation-prompt-tag unsafe-root-continuation-prompt-tag
        'break-enabled-key break-enabled-key
        'engine-block engine-block
        'make-mutex make-mutex
        'mutex-acquire mutex-acquire
        'mutex-release mutex-release
        'make-condition make-condition
        'condition-wait condition-wait
        'condition-signal condition-signal
        'condition-broadcast condition-broadcast
        'threaded? threaded?
        'get-thread-id get-thread-id
        ;; These are actually redirected by "place-register.ss", but
        ;; we list them here for compatibility with the bootstrapping
        ;; variant of `#%pthread`
        'unsafe-make-place-local rumble:unsafe-make-place-local
        'unsafe-place-local-ref rumble:unsafe-place-local-ref
        'unsafe-place-local-set! rumble:unsafe-place-local-set!)]
      [(|#%engine|)
       (hasheq
        'make-engine rumble:make-engine
        'make-engine-thread-cell-state rumble:make-engine-thread-cell-state
        'set-engine-thread-cell-state! rumble:set-engine-thread-cell-state!
        'engine-timeout rumble:engine-timeout
        'engine-return rumble:engine-return
        'engine-roots rumble:engine-roots
        'call-with-engine-completion rumble:call-with-engine-completion
        'set-ctl-c-handler! rumble:set-ctl-c-handler!
        'poll-will-executors poll-will-executors
        'make-will-executor rumble:make-will-executor
        'make-late-will-executor rumble:make-late-will-executor
        'will-executor? rumble:will-executor?
        'will-register rumble:will-register
        'will-try-execute rumble:will-try-execute
        'unsafe-make-hasheq unsafe-make-hasheq
        'unsafe-make-weak-hasheq unsafe-make-weak-hasheq
        'set-break-enabled-transition-hook! rumble:set-break-enabled-transition-hook!
        'continuation-marks rumble:continuation-marks
        'set-reachable-size-increments-callback! rumble:set-reachable-size-increments-callback!
        'set-custodian-memory-use-proc! rumble:set-custodian-memory-use-proc!
        'set-immediate-allocation-check-proc! rumble:set-immediate-allocation-check-proc!
        'exn:break/non-engine exn:break
        'exn:break:hang-up/non-engine exn:break:hang-up
        'exn:break:terminate/non-engine exn:break:terminate
        'current-process-milliseconds cpu-time
        'poll-async-callbacks poll-async-callbacks
        'disable-interrupts disable-interrupts
        'enable-interrupts enable-interrupts
        'sleep sleep
        'get-wakeup-handle get-wakeup-handle
        'wakeup wakeup
        'fork-place rumble:fork-place
        'place-get-inherit rumble:place-get-inherit
        'start-place rumble:start-place
        'fork-pthread rumble:fork-thread
        'get-initial-place rumble:get-initial-pthread
        'current-place-roots rumble:current-place-roots
        'call-with-current-continuation-roots rumble:call-with-current-continuation-roots 
        'exit place-exit
        'pthread? rumble:thread?
        'call-as-asynchronous-callback rumble:call-as-asynchronous-callback
        'post-as-asynchronous-callback rumble:post-as-asynchronous-callback
        'continuation-current-primitive rumble:continuation-current-primitive
        'prop:unsafe-authentic-override prop:unsafe-authentic-override
        'get-system-stats get-system-stats
        'internal-error internal-error)]
      [else #f]))

  ;; Tie knots:
  (define (check-for-break) (1/check-for-break))
  (define (break-enabled) (1/break-enabled))

  (include "include.ss")
  (include-generated "thread.scm")

  (set-engine-exit-handler!
   (lambda (v)
     (|#%app| (|#%app| 1/exit-handler) v)))

  (set-scheduler-lock-callbacks! (lambda () (1/make-semaphore 1))
                                 unsafe-semaphore-wait
                                 unsafe-semaphore-post)

  (set-scheduler-atomicity-callbacks! (lambda ()
                                        (start-atomic))
                                      (lambda ()
                                        (end-atomic))
                                      (lambda ()
                                        (current-atomic (fx+ (current-atomic) 1)))
                                      (lambda ()
                                        (current-atomic (fx- (current-atomic) 1))))

  (set-future-callbacks! future-block future-unblock current-future-prompt))
