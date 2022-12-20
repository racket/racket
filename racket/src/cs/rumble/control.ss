;; The full continuation is a chain of metacontinuations. Each
;; metacontinuation contains a host Scheme continuation, and
;; every prompt is on a boundary between metacontinuations. When
;; a composable continuation is applied, the composition boundary
;; is also a metacontinuation boundary.

;; "Continuation" as exported from Rumble is "metacontinuation"
;; here. So, `call-with-current-continuation` defined here and
;; exported captures the current metacontinuation (up to a prompt).
;; The `call/cc` function is the host's notion of continuation, which
;; corresponds to a single metacontinuation frame.

;; A picture where the continuation grows down:

;;                   [root empty continuation]
;;                    --- empty-k: 'empty attachment
;; metacontinuation  |
;;     frame         |
;;                   |--- resume-k
;;                   |<-- tag represents this point
;;                    --- empty-k: 'empty attachment
;; metacontinuation  |
;;     frame         |
;;                   |
;;                   |--- resume-k
;;                   |<-- tag represents this point
;;                    --- empty-k: 'empty attachment
;;   current host    |
;;   continuation    |
;;                   v

;; Concretely, the metacontinuation is the current host continuation
;; plus the frames in the list `(current-metacontinuation)`, where the
;; shallowest (= lowest in the picture above) frame is first in the
;; list. The `empty-k` continuation is recognized by having an
;; 'empty continuation attachment.

;; The conceptual `empty-k` continuation is used to detect when the
;; current host continuation is empty. When it's empty, then calling a
;; composable continuation doesn't need to add a new metacontinuation
;; frame, and the application gets the right "tail" behavior.

;; Any continuation marks for the `empty-k` continuation are kept
;; separate in `current-mark-splice`, instead of being kept as an
;; attachment. That way, the continuation's marks (if any) can be
;; spliced into a new context when captured in a composable
;; continuation. See also `current-mark-splice` below.

;; A metacontinuation frame's `resume-k` is called when control
;; returns or aborts to the frame. When aborting to a prompt tag,
;; metacontinuation frames between the abort and prompt are removed
;; one-by-one, running any winders in each frame. Finally, the
;; `resume-k` continuation of the target prompt's metacontinuation is
;; called; the `resume-k` is called using `#%$call-in-continuation` to
;; run a thunk in the restored continuation to apply the prompt's
;; handler.
;;
;; Calling a non-composable continuation is similar to aborting,
;; except that the target prompt's abort handler is not called. In
;; fact, the metacontinuation-frame unwinding process stops before the
;; frame with the target prompt tag (since that prompt is meant to be
;; preserved).

;; The `dynamic-wind` winders for the frame represented by the current
;; host continuation are kept in `current-winders`. Each winder has a
;; continuation for the point where the winder applies, and when
;; winding or unwinding, control is oved to that continuation, which
;; is needed especially for space-safe unwinding to avoid retaining
;; the continuation where the jump starts.

;; The continuation marks for the frame represented by the current
;; host continuation are implemented by the host's
;; continuation-attachment support. The `current-mark-stack` function
;; is just an alias for the host's `$current-attachments` function.
;; When a metacontinuation frame is created, it takes the current
;; `current-mark-stack` value and `current-mark-stack` is set back to
;; empty. To keep winders and the mark stack in sync, a `dynamic-wind`
;; pre or post thunk resets the mark stack on entry.

;; When a composable continuation is applied in a continuation frame
;; that has marks, then the marks are moved into
;; `current-mark-splice`, which is conceptually merged into the tail
;; of `current-mark-stack`. Having a separate `current-mark-splice`
;; enables `dynamic-wind` pre and post thunks to adapt correctly to
;; the splicing while jumping into or out of the continuation.

;; A metacontinuation frame has an extra cache slot to contain a "mark
;; chain", which is a cached/caching list of mark-stack lists down to
;; the root continuation. When a delimited sequence of
;; metacontinuation frames are copied out of or into the
;; metacontinuation, the slot is flushed and will be reset on demand.

;; Continuations are used to implement engines, but it's important
;; that an engine doesn't get swapped out (or, more generally,
;; asynchronous signals are handled at the Racket level) while we're
;; manipulating the continuation representation. A bad time for a swap
;; is an "interrupted" region. The `begin-uninterrupted` and
;; `end-uninterrupted` functions bracket such regions dynamically. See
;; also "rumble/engine.ss" and "rumble/interrupt.ss"

(define-virtual-register current-metacontinuation '())

(define-record metacontinuation-frame (tag           ; continuation prompt tag or #f
                                       resume-k      ; delivers values to the prompt
                                       handler       ; prompt handler
                                       marks         ; marks of `resume-k` plus immediate mark (if any)
                                       winders       ; `dynamic-wind` winders
                                       mark-splice   ; extra part of mark stack to restore
                                       mark-chain    ; #f or a cached list of mark-chain-frame or elem+cache
                                       traces        ; #f or a cached list of traces
                                       cc-guard      ; for impersonated tag, initially #f
                                       avail-cache)) ; cache for `continuation-prompt-available?`

(define-record-type (continuation-prompt-tag create-continuation-prompt-tag authentic-continuation-prompt-tag?)
  (fields (mutable name))) ; mutable => constructor generates fresh instances

(define the-default-continuation-prompt-tag (create-continuation-prompt-tag 'default))

;; Not actually set, but allows access to the full continuation:
(define the-root-continuation-prompt-tag (create-continuation-prompt-tag 'root))

;; Tag for a metacontinuation created for composing a continuation
(define the-compose-prompt-tag (create-continuation-prompt-tag 'compose))

;; Detected to prevent some jumps:
(define the-barrier-prompt-tag (create-continuation-prompt-tag 'barrier))

(define/who make-continuation-prompt-tag
  (case-lambda
    [() (create-continuation-prompt-tag #f)]
    [(name)
     (check who symbol? name)
     (create-continuation-prompt-tag name)]))

(define (default-continuation-prompt-tag) the-default-continuation-prompt-tag)
(define (unsafe-root-continuation-prompt-tag) the-root-continuation-prompt-tag)

;; To support special treatment of break parameterizations, and also
;; to initialize disabled breaks for `dynamic-wind` pre and post
;; thunks:
(define break-enabled-key '#{break-enabled n1kcvqw4c9hh8t3fi3659ci94-2})

(define/who continuation-prompt-available?
  (case-lambda
   [(tag)
    (check who continuation-prompt-tag? tag)
    (is-continuation-prompt-available? tag (current-metacontinuation))]
   [(tag k)
    (check who continuation-prompt-tag? tag)
    (check who continuation? k)
    (when (escape-continuation? k)
      (unless (is-continuation-prompt-available? (escape-continuation-tag k) #f)
        (raise-continuation-error who
                                  "escape continuation not in the current thread's continuation")))
    (or (is-continuation-prompt-available? tag (continuation-mc k))
        (and (non-composable-continuation? k)
             (eq? (strip-impersonator tag) (strip-impersonator (full-continuation-tag k)))))]))

(define (is-continuation-prompt-available? tag mc)
  (maybe-future-barricade tag)
  (let ([tag (strip-impersonator tag)])
    (or (and (not mc)
             (or (eq? tag the-default-continuation-prompt-tag)
                 (eq? tag the-root-continuation-prompt-tag)))
        ;; Look through metacontinuation cache, but cache a search result
        ;; half-way up if the chain is deep enough
        (let ([mc (or mc (current-metacontinuation))])
          (let loop ([mc mc] [slow-mc mc] [slow-step? #f] [steps 0])
            (cond
             [(null? mc)
              (cache-prompt-available-conclusion tag #f slow-mc steps)]
             [else
              (let ([mf (car mc)])
                (cond
                 [(eq? tag (strip-impersonator (metacontinuation-frame-tag mf)))
                  (cache-prompt-available-conclusion tag #t slow-mc steps)]
                 [else
                  (let* ([avail-cache (metacontinuation-frame-avail-cache mf)]
                         [avail (and avail-cache (eq-hashtable-ref avail-cache tag #f))])
                    (cond
                     [avail
                      (cache-prompt-available-conclusion tag (eq? avail 'yes) slow-mc steps)]
                     [else
                      (loop (cdr mc)
                            (if slow-step? (cdr slow-mc) slow-mc)
                            (not slow-step?)
                            (add1 steps))]))]))]))))))

(define (cache-prompt-available-conclusion tag avail? slow-mc steps)
  (when (> steps 32)
    ;; cache conclusion halfway
    (let* ([mf (car slow-mc)]
           [avail-cache (or (metacontinuation-frame-avail-cache mf)
                            (let ([ht (make-weak-eq-hashtable)])
                              (set-metacontinuation-frame-avail-cache! mf ht)
                              ht))])
      (eq-hashtable-set! avail-cache tag (if avail? 'yes 'no))))
  avail?)

(define (maybe-future-barricade tag)
  (when (current-future)
    (#%$app/no-inline future-barricade tag)))

(define (future-barricade tag)
  (let ([fp (strip-impersonator (current-future-prompt))]
        [tag (strip-impersonator tag)])
    (cond
     [(eq? fp tag)
      ;; shortcut: boundary is the future prompt
      (void)]
     [(eq? tag the-root-continuation-prompt-tag)
      (block-future)]
     [else
      (let loop ([mc (current-metacontinuation)])
        (cond
         [(null? mc)
          ;; Won't happen normally, since every thread starts with a explicit prompt
          (block-future)]
         [(eq? tag (strip-impersonator (metacontinuation-frame-tag (car mc))))
          (void)]
         [(eq? (metacontinuation-frame-tag (car mc)) fp)
          ;; tag must be above future prompt
          (block-future)]
         [else
          (loop (cdr mc))]))])))

(define/who call-with-continuation-prompt
  (case-lambda
    [(proc) (call-with-continuation-prompt proc the-default-continuation-prompt-tag #f)]
    [(proc tag) (call-with-continuation-prompt proc tag #f)]
    [(proc tag handler . args)
     (check who procedure? proc)
     (check who continuation-prompt-tag? tag)
     (check who :or-false procedure? handler)
     (start-uninterrupted 'prompt)
     (call-in-empty-metacontinuation-frame
      tag
      (wrap-handler-for-impersonator
       tag
       (or handler (make-default-abort-handler tag)))
      empty-mark-frame ; new splice
      (lambda ()
        (end-uninterrupted 'prompt)
        ;; Finally, apply the given function:
        (apply proc args)))]))

(define (make-default-abort-handler tag)
  (case-lambda
   [(abort-thunk)
    (check 'default-continuation-prompt-handler (procedure-arity-includes/c 0) abort-thunk)
    (call-with-continuation-prompt abort-thunk tag #f)]
   [args
    ;; report arity error as result-arity error
    (apply raise-result-arity-error
           'call-with-continuation-prompt
           1
           "\n  in: application of default prompt handler"
           args)]))

(define (pop-metacontinuation-frame)
  (let ([mf (car (current-metacontinuation))])
    (current-metacontinuation (cdr (current-metacontinuation)))
    (current-winders (metacontinuation-frame-winders mf))
    (current-mark-splice (metacontinuation-frame-mark-splice mf))
    mf))

(define (call-in-empty-metacontinuation-frame tag handler new-splice proc)
  ;; Call `proc` in an empty metacontinuation frame, reifying the
  ;; current metacontinuation as a new frame on `current-metacontinuation`
  (assert-in-uninterrupted 'call-in-empty-metacontinuation-frame)
  (assert-not-in-system-wind 'call-in-empty-metacontinuation-frame)
  (call/cc
   (lambda (resume-k)
     (let ([marks (current-mark-stack)]) ; grab marks before `#%$call-in-continuation`
       (#%$call-in-continuation
        #%$null-continuation
        '()
        (lambda ()
          (let-values ([results
                        ;; mark the "empty" continuation frame
                        ;; that just continues the metacontinuation:
                        (#%$call-setting-continuation-attachment
                         'empty
                         (lambda ()
                           (let ([mf (make-metacontinuation-frame tag
                                                                  resume-k
                                                                  handler
                                                                  marks
                                                                  (current-winders)
                                                                  (current-mark-splice)
                                                                  #f
                                                                  #f
                                                                  #f
                                                                  #f)])
                             (current-winders '())
                             (current-mark-splice new-splice)
                             ;; push the metacontinuation:
                             (current-metacontinuation (cons mf (current-metacontinuation)))
                             ;; ready:
                             (proc))))])
            ;; Continue normally; the metacontinuation could be different
            ;; than when we captured this metafunction frame, though:
            (cond
             [(null? (current-metacontinuation)) (engine-return)]
             [else
              (start-uninterrupted 'resume-mc)
              (let ([mf (pop-metacontinuation-frame)])
                (#%$call-in-continuation
                 (metacontinuation-frame-resume-k mf)
                 (metacontinuation-frame-marks mf)
                 (lambda ()
                   (end-uninterrupted 'resume)
                   (let ([cc-guard (metacontinuation-frame-cc-guard mf)])
                     ;; Apply the cc-guard, if any, outside of the prompt:
                     (cond
                      [cc-guard
                       (apply cc-guard results)]
                      [else
                       (if (and (pair? results) (null? (cdr results)))
                           (car results)
                           (#%apply values results))])))))]))))))))

;; Simplified `call-in-empty-metacontinuation-frame` suitable for swapping engines:
(define (call-with-empty-metacontinuation-frame-for-swap proc)
  (assert-in-uninterrupted 'call-with-empty-metacontinuation-frame-for-swap)
  (assert-not-in-system-wind 'call-with-empty-metacontinuation-frame-for-swap)
  (call/cc
   (lambda (resume-k)
     (let ([marks (current-mark-stack)])
       (#%$call-in-continuation
        #%$null-continuation
        '()
        (lambda ()
          (let ([mf (make-metacontinuation-frame #f
                                                 resume-k
                                                 void
                                                 marks
                                                 (current-winders)
                                                 (current-mark-splice)
                                                 #f
                                                 #f
                                                 #f
                                                 #f)])
            (current-winders '())
            (current-mark-splice empty-mark-frame)
            (let ([mc (cons mf (current-metacontinuation))])
              (current-metacontinuation '())
              (let ([r (proc mc)])
                (let ([mf (pop-metacontinuation-frame)])
                  (#%$call-in-continuation
                   (metacontinuation-frame-resume-k mf)
                   (metacontinuation-frame-marks mf)
                   (lambda () r))))))))))))

(define (call-in-empty-metacontinuation-frame-for-compose proc)
  (#%$call-getting-continuation-attachment
   'none
   (lambda (at)
     (cond
      [(eq? at 'empty)
       ;; empty continuation in the current frame; don't push a new
       ;; metacontinuation frame
       (proc)]
      [else
       ;; Consume attachment to move it (if there is one) to the new
       ;; metacontinuation frame's splice:
       (#%$call-consuming-continuation-attachment
        empty-mark-frame
        (lambda (new-splice)
          (call-in-empty-metacontinuation-frame
           the-compose-prompt-tag
           fail-abort-to-delimit-continuation
           new-splice
           proc)))]))))

(define (metacontinuation-frame-update-mark-splice current-mf mark-splice)
  (make-metacontinuation-frame (metacontinuation-frame-tag current-mf)
                               (metacontinuation-frame-resume-k current-mf)
                               (metacontinuation-frame-handler current-mf)
                               (metacontinuation-frame-marks current-mf)
                               (metacontinuation-frame-winders current-mf)
                               mark-splice
                               #f
                               #f
                               (metacontinuation-frame-cc-guard current-mf)
                               #f))

(define (metacontinuation-frame-update-cc-guard current-mf cc-guard)
  ;; Ok to keep caches, since the cc-guard doesn't affect them
  (make-metacontinuation-frame (metacontinuation-frame-tag current-mf)
                               (metacontinuation-frame-resume-k current-mf)
                               (metacontinuation-frame-handler current-mf)
                               (metacontinuation-frame-marks current-mf)
                               (metacontinuation-frame-winders current-mf)
                               (metacontinuation-frame-mark-splice current-mf)
                               (metacontinuation-frame-mark-chain current-mf)
                               (metacontinuation-frame-traces current-mf)
                               cc-guard
                               (metacontinuation-frame-avail-cache current-mf)))
 
;; ----------------------------------------

(define/who (abort-current-continuation tag . args)
  (check who continuation-prompt-tag? tag)
  (maybe-future-barricade tag)
  (check-prompt-tag-available who (strip-impersonator tag))
  (let ([args (apply-impersonator-abort-wrapper tag args)]
        [tag (strip-impersonator tag)])
    (start-uninterrupted 'abort)
    (do-abort-current-continuation who tag args #t)))

(define/who (unsafe-abort-current-continuation/no-wind tag arg)
  (let ([args (apply-impersonator-abort-wrapper tag (list arg))]
        [tag (strip-impersonator tag)])
    (start-uninterrupted 'abort)
    (do-abort-current-continuation who tag args #f)))

;; `args` can be a thunk if `do-abort-current-continuation` is
;; called via `apply-continuation`
(define (do-abort-current-continuation who tag args wind?)
  (assert-in-uninterrupted 'do-abort-current-continuation)
  (cond
   [(null? (current-metacontinuation))
    ;; A reset handler must end the uninterrupted region:
    ((reset-handler))]
   [(or (not wind?)
        (null? (current-winders)))
    (let ([mf (car (current-metacontinuation))])
      (cond
       [(eq? tag (strip-impersonator (metacontinuation-frame-tag mf)))
        ;; Remove the prompt and resume its continuation
        ;; as we call the handler:
        (let ([mf (pop-metacontinuation-frame)])
          (#%$call-in-continuation
           (metacontinuation-frame-resume-k mf)
           (metacontinuation-frame-marks mf)
           (lambda ()
             (end-uninterrupted/call-hook 'handle)
             (if (#%procedure? args)
                 (args) ; assuming that handler is `values`
                 (apply (metacontinuation-frame-handler mf)
                        args)))))]
       [else
        ;; Aborting to an enclosing prompt, so keep going:
        (pop-metacontinuation-frame)
        (do-abort-current-continuation who tag args wind?)]))]
   [else
    (wind-to
     (winders-length (current-winders)) 0 '()
     ;; No winders left:
     (lambda ()
       (do-abort-current-continuation who tag args #t))
     ;; If the metacontinuation changes, check target before retrying:
     (lambda ()
       (check-prompt-still-available who tag)
       (do-abort-current-continuation who tag args #t)))]))

(define (check-prompt-still-available who tag)
  (unless (is-continuation-prompt-available? tag #f)
    (end-uninterrupted 'escape-fail)
    (raise-continuation-error who
                              (string-append
                               "lost target;\n"
                               (if (eq? who 'abort-current-continuation)
                                   (string-append
                                    " abort in progress, but the current continuation includes no prompt with\n"
                                    " the given tag after a `dynamic-wind` post-thunk return")
                                   (string-append
                                    " jump to escape continuation in progress, and the target is not in the\n"
                                    " current continuation after a `dynamic-wind` post-thunk return"))))))

;; ----------------------------------------

(define/who (call-with-continuation-barrier p)
  (check who (procedure-arity-includes/c 0) p)
  (start-uninterrupted 'barrier)
  (call-with-continuation-barrier* p))

(define/who (call-with-continuation-barrier* p)
  (assert-in-uninterrupted 'call-with-continuation-barrier*)
  (call-in-empty-metacontinuation-frame
   the-barrier-prompt-tag ; <- recognized as a barrier by continuation capture or call
   #f
   empty-mark-frame ; new splice
   (lambda ()
     (end-uninterrupted 'barrier)
     (|#%app| p))))

;; ----------------------------------------
;; Capturing and applying continuations

(define-record continuation (mc))
(define-record full-continuation continuation (k winders mark-stack mark-splice tag))
(define-record composable-continuation full-continuation (wind?))
(define-record non-composable-continuation full-continuation ())
(define-record escape-continuation continuation (tag))

(define/who call-with-current-continuation
  (case-lambda
    [(proc) (call-with-current-continuation proc
                                            the-default-continuation-prompt-tag)]
    [(proc tag)
     (check who (procedure-arity-includes/c 1) proc)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (call/cc
      (lambda (k)
        (|#%app|
         proc
         (make-non-composable-continuation
          (extract-metacontinuation who (strip-impersonator tag) #t)
          k
          (current-winders)
          (current-mark-stack)
          (current-mark-splice)
          tag))))]))

(define/who call-with-composable-continuation
  (case-lambda
    [(p) (call-with-composable-continuation p the-default-continuation-prompt-tag)]
    [(p tag)
     (check who (procedure-arity-includes/c 1) p)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (call-with-composable-continuation* p tag #t)]))

(define (call-with-composable-continuation* p tag wind?)
  (call/cc
   (lambda (k)
     (|#%app|
      p
      (make-composable-continuation
       (extract-metacontinuation 'call-with-composable-continuation (strip-impersonator tag) (not wind?))
       k
       (current-winders)
       (current-mark-stack)
       (current-mark-splice)
       tag
       wind?)))))

(define (unsafe-call-with-composable-continuation/no-wind p tag)
  (call-with-composable-continuation* p tag #f))

(define/who (call-with-escape-continuation p)
  (check who (procedure-arity-includes/c 1) p)
  (let ([tag (make-continuation-prompt-tag)])
    (call-with-continuation-prompt
     (lambda ()
       (|#%app| p (make-escape-continuation (current-metacontinuation) tag)))
     tag
     values)))

(define/who (call-in-continuation c proc)
  (check who continuation? c)
  (cond
   [(and (#%procedure? proc)
         (chez:procedure-arity-includes? proc 0))
    (apply-continuation c proc)]
   [else
    (check who (procedure-arity-includes/c 0) proc)
    (apply-continuation c (lambda () (proc)))]))

;; `args` is either a list or a procedure for which `#%procedure?` is true
(define (apply-continuation c args)
  (cond
   [(composable-continuation? c)
    (apply-composable-continuation c args)]
   [(non-composable-continuation? c)
    (apply-non-composable-continuation c args)]
   [(escape-continuation? c)
    (apply-escape-continuation c args)]
   [else
    (raise-argument-error 'apply-continuation "continuation?" c)]))

;; Applying a composable continuation calls this internal function:
(define (apply-composable-continuation c args)
  (start-uninterrupted 'continue)
  ;; To compose the metacontinuation, first make sure the current
  ;; continuation is reified in `(current-metacontinuation)`:
  (call-in-empty-metacontinuation-frame-for-compose
   (lambda ()
     ;; The current metacontinuation frame has an
     ;; empty continuation, so we can "replace" that
     ;; with the composable one:
     (cond
      [(and (null? (continuation-mc c))
            (null? (full-continuation-winders c))
            (eq? (current-mark-splice) (full-continuation-mark-splice c))
            (let ([marks (#%$continuation-attachments (full-continuation-k c))])
              (or (null? marks)
                  (and (null? (cdr marks))
                       (eq? (car marks) 'empty)))))
       ;; Shortcut for no winds and no change to break status:
       (end-uninterrupted 'cc)
       (if (#%procedure? args)
           (#%$call-in-continuation (full-continuation-k c) (full-continuation-mark-stack c)
                                    (lambda () (args)))
           (#%apply (full-continuation-k c) args))]
      [(not (composable-continuation-wind? c))
       (apply-immediate-continuation/no-wind c args)]
      [else
       (apply-immediate-continuation c (reverse (continuation-mc c)) args 0 0)]))))

;; Applying an escape continuation calls this internal function:
(define (apply-escape-continuation c args)
  (start-uninterrupted 'continue)
  (let ([tag (escape-continuation-tag c)])
    (unless (is-continuation-prompt-available? tag #f)
      (end-uninterrupted 'escape-fail)
      (raise-continuation-error '|continuation application|
                                "attempt to jump into an escape continuation"))
    (do-abort-current-continuation '|continuation application| tag args #t)))

;; Applying a non-composable continuation calls this internal function:
(define (apply-non-composable-continuation c args)
  (start-uninterrupted 'continue)
  (apply-non-composable-continuation* c args))

(define (apply-non-composable-continuation* c args)
  (assert-in-uninterrupted 'apply-non-composable-continuation*)
  (let ([mc (current-metacontinuation)]
        [c-mc (continuation-mc c)]
        [tag (full-continuation-tag c)])
    (cond
     [(and (null? c-mc)
           (pair? mc)
           (not (impersonator? tag))
           (eq? tag (metacontinuation-frame-tag (car mc)))
           (same-winders? (current-winders) (full-continuation-winders c))
           (eq? (current-mark-splice) (full-continuation-mark-splice c))
           (eq? (#%$continuation-attachments (full-continuation-k c))
                (current-mark-stack)))
      ;; Short cut: jump within the same metacontinuation, no winder
      ;; changes or changes to marks (so no break-enabled changes),
      ;; and no tag impersonators to deal with
      (end-uninterrupted 'cc)
      (if (#%procedure? args)
          (#%$call-in-continuation (full-continuation-k c) (full-continuation-mark-stack c)
                                   (lambda () (args)))
          (#%apply (full-continuation-k c) args))]
     [else
      (let-values ([(common-mc   ; shared part of the current metacontinuation
                     rmc-append) ; non-shared part of the destination metacontinuation
                    ;; We check every time, just in case control operations
                    ;; change the current continuation out from under us.
                    (find-common-metacontinuation c-mc
                                                  mc
                                                  (strip-impersonator tag))])
        (let-values ([(exit-winder-n     ; number of source winders we need to exit
                       entered-winder-n) ; number of destination winders we *don't* need to enter
                      (count-exit+entered-continuation-winders common-mc c rmc-append)])
          (let loop ([exit-winder-n exit-winder-n])
            (cond
              [(eq? common-mc (current-metacontinuation))
               ;; Replace the current metacontinuation frame's continuation
               ;; with the saved one; this replacement will take care of any
               ;; shared winders within the frame.
               (apply-immediate-continuation c rmc-append args exit-winder-n entered-winder-n)]
              [else
               ;; Unwind this metacontinuation frame:
               (let ([exit-winder-now-n (fxmin exit-winder-n
                                               (winders-length (current-winders)))])
                 (wind-to
                  exit-winder-now-n 0 '()
                  ;; If all winders complete simply:
                  (lambda ()
                    (pop-metacontinuation-frame)
                    (loop (fx- exit-winder-n exit-winder-now-n)))
                  ;; If a winder changes the metacontinuation, then
                  ;; start again:
                  (lambda ()
                    (apply-non-composable-continuation* c args))))]))))])))

;; Apply a continuation within the current metacontinuation frame:
(define (apply-immediate-continuation c rmc args exit-winder-n entered-winder-n)
  (assert-in-uninterrupted 'apply-immediate-continuation)
  (apply-continuation-with-appended-metacontinuation rmc c args exit-winder-n entered-winder-n))

(define (apply-continuation-within-metacontinuation c args exit-winder-n entered-winder-n)
  (let ([mark-stack (full-continuation-mark-stack c)])
    (current-mark-splice (let ([mark-splice (full-continuation-mark-splice c)])
                           (if (composable-continuation? c)
                               (merge-mark-splice mark-splice (current-mark-splice))
                               mark-splice)))
    (wind-to
     exit-winder-n entered-winder-n (full-continuation-winders c)
     ;; When no winders are left:
     (lambda ()
       (when (non-composable-continuation? c)
         ;; Activate/add cc-guards in target prompt; any user-level
         ;; callbacks here are run with a continuation barrier, so
         ;; the metacontinuation won't change (except by escaping):
         (activate-and-wrap-cc-guard-for-impersonator!
          (full-continuation-tag c)))
       (end-uninterrupted 'cc)
       (apply-with-break-transition (full-continuation-k c) mark-stack args))
     ;; If a winder changed the meta-continuation, try again for a
     ;; non-composable continuation:
     (and (non-composable-continuation? c)
          (lambda ()
            (apply-non-composable-continuation* c args))))))

;; Like `apply-immediate-continuation`, but don't run winders
(define (apply-immediate-continuation/no-wind c args)
  (current-metacontinuation (append
                             (map metacontinuation-frame-clear-cache (continuation-mc c))
                             (current-metacontinuation)))
  (current-winders (full-continuation-winders c))
  (current-mark-splice (full-continuation-mark-splice c))
  (end-uninterrupted 'cc)
  (apply-with-break-transition (full-continuation-k c) (full-continuation-mark-stack c) args))

;; Used as a "handler" for a prompt without a tag, which is used for
;; composable continuations
(define (fail-abort-to-delimit-continuation . args)
  (error 'abort "trying to abort to a delimiter continuation frame"))

;; Find common metacontinuation to keep due to a combination of:
;; the metacontinuation is beyond the relevant prompt, or the
;; metacontinuation fragment before the prompt is also shared
;; with the composable continuation's metacontinuation (so we
;; should not unwind and rewind those metacontinuation frames)
(define (find-common-metacontinuation mc current-mc tag)
  (assert-in-uninterrupted 'find-common-metacontinuation)
  (let-values ([(rev-current ; (list (cons mf mc) ...)
                 base-current-mc)
                ;; Get the reversed prefix of `current-mc` that is to be
                ;; replaced by `mc`:
                (let loop ([current-mc current-mc] [accum null])
                  (cond
                   [(null? current-mc)
                    (unless (or (eq? tag the-default-continuation-prompt-tag)
                                (eq? tag the-root-continuation-prompt-tag))
                      (end-uninterrupted 'tag-error)
                      (raise-no-prompt-tag '|continuation application| tag))
                    (values accum null)]
                   [(eq? tag (strip-impersonator (metacontinuation-frame-tag (car current-mc))))
                    (values accum current-mc)]
                   [else
                    (loop (cdr current-mc)
                          ;; Accumulate this frame plus the chain that
                          ;; we should keep if this frame is shared:
                          (cons (cons (car current-mc) current-mc)
                                accum))]))])
    (let ([rev-mc (reverse mc)])
      ;; Work from the tail backwards (which is forward in the reverse
      ;; lists): If the continuations are the same for the two frames,
      ;; then the metacontinuation frame should not be unwound
      (let loop ([rev-current rev-current]
                 [rev-mc rev-mc]
                 [base-current-mc base-current-mc])
        (cond
         [(null? rev-mc) (values base-current-mc '())]
         [(null? rev-current)
          (check-for-barriers rev-mc)
          ;; Return the shared part plus the unshared-to-append part
          (values base-current-mc rev-mc)]
         [(eq? (metacontinuation-frame-resume-k (car rev-mc))
               (metacontinuation-frame-resume-k (caar rev-current)))
          ;; Matches, so update base and look shallower
          (loop (cdr rev-current)
                (cdr rev-mc)
                (cdar rev-current))]
         [else
          ;; Doesn't match, so we've found the shared part;
          ;; check for barriers that we'd have to reintroduce
          (check-for-barriers rev-mc)
          ;; Return the shared part plus the unshared-to-append part
          (values (cdr (cdar rev-current)) rev-mc)])))))

(define (check-for-barriers rev-mc)
  (assert-in-uninterrupted 'check-for-barriers)
  (unless (null? rev-mc)
    (when (eq? (metacontinuation-frame-tag (car rev-mc))
               the-barrier-prompt-tag)
      (raise-barrier-error))
    (check-for-barriers (cdr rev-mc))))

(define (raise-barrier-error)
  (end-uninterrupted 'hit-barrier)
  (raise-continuation-error '|continuation application|
                            "attempt to cross a continuation barrier"))

(define (set-continuation-applicables!)
  ;; These procedure registrations may be short-circuited by a special
  ;; case that dispatches directly to `apply-continuation`
  (struct-property-set! prop:procedure
                        (record-type-descriptor composable-continuation)
                        (lambda (c . args) (apply-composable-continuation c args)))
  (struct-property-set! prop:procedure
                        (record-type-descriptor non-composable-continuation)
                        (lambda (c . args) (apply-non-composable-continuation c args)))
  (struct-property-set! prop:procedure
                        (record-type-descriptor escape-continuation)
                        (lambda (c . args) (apply-escape-continuation c args)))
  (struct-property-set! prop:object-name
                        (record-type-descriptor continuation-prompt-tag)
                        0))

;; ----------------------------------------
;; Metacontinuation operations for continutions

;; Extract a prefix of `(current-metacontinuation)` up to `tag`
(define (extract-metacontinuation who tag barrier-ok?)
  (let ([check-barrier-ok
         (lambda (saw-barrier?)
           (when (and saw-barrier? (not barrier-ok?))
             (raise-continuation-error who "cannot capture past continuation barrier")))])
    (let loop ([mc (current-metacontinuation)] [saw-barrier? #f])
      (cond
       [(null? mc)
        (unless (or (eq? tag the-root-continuation-prompt-tag)
                    (eq? tag the-default-continuation-prompt-tag))
          (raise-no-prompt-tag who tag))
        (check-barrier-ok saw-barrier?)
        '()]
       [else
        (let ([a-tag (strip-impersonator (metacontinuation-frame-tag (car mc)))])
          (cond
           [(eq? a-tag tag)
            (check-barrier-ok saw-barrier?)
            '()]
           [else
            (cons (metacontinuation-frame-clear-cache (car mc))
                  (loop (cdr mc) (or saw-barrier?
                                     (eq? a-tag the-barrier-prompt-tag))))]))]))))

(define (check-prompt-tag-available who tag)
  (unless (is-continuation-prompt-available? tag #f)
    (raise-no-prompt-tag who tag)))

(define (raise-no-prompt-tag who tag)
  (do-raise-arguments-error 'internal
                            who
                            primitive-realm
                            "no corresponding prompt in the continuation"
                            exn:fail:contract:continuation
                            (list "tag" tag)))

(define (apply-continuation-with-appended-metacontinuation rmc dest-c dest-args exit-winder-n entered-winder-n)
  ;; Assumes that the current metacontinuation frame is ready to be
  ;; replaced with `mc` (reversed as `rmc`) plus `proc`.
  ;; In the simple case of no winders and an empty immediate
  ;; metacontinuation frame, we could just
  ;;  (current-metacontinuation (append mc (current-metacontinuation)))
  ;; But, to run winders and replace anything in the current frame,
  ;; we proceed frame-by-frame in `mc`.
  (assert-in-uninterrupted 'apply-continuation-with-appended-metacontinuation)
  (let loop ([rmc rmc] [exit-winder-n exit-winder-n] [entered-winder-n entered-winder-n])
    (cond
      [(null? rmc)
       (apply-continuation-within-metacontinuation dest-c dest-args exit-winder-n entered-winder-n)]
      [else
       (let ([mf (maybe-merge-splice (composable-continuation? dest-c)
                                     (metacontinuation-frame-clear-cache (car rmc)))]
             [rmc (cdr rmc)])
         ;; Set splice before jumping, so it can be used by winders
         (current-mark-splice (metacontinuation-frame-mark-splice mf))
         ;; Run "in" winders for the metacontinuation
         (let ([entered-winder-now-n (fxmin entered-winder-n
                                            (winders-length (metacontinuation-frame-winders mf)))])
           (wind-to
            exit-winder-n entered-winder-now-n (metacontinuation-frame-winders mf)
            ;; When all winders done for this frame:
            (lambda ()
              (current-metacontinuation (cons mf (current-metacontinuation)))
              (loop rmc 0 (fx- entered-winder-n entered-winder-now-n)))
            ;; When a winder changes the metacontinuation, try again
            ;; for a non-composable continuation:
            (and (non-composable-continuation? dest-c)
                 (lambda ()
                   (apply-non-composable-continuation* dest-c dest-args))))))])))

(define (metacontinuation-frame-clear-cache mf)
  (metacontinuation-frame-update-mark-splice mf
                                             (metacontinuation-frame-mark-splice mf)))

;; Get/cache a converted list of marks for a metacontinuation
(define (metacontinuation-marks mc)
  (cond
   [(null? mc) null]
   [else (let ([mf (car mc)])
           (or (metacontinuation-frame-mark-chain mf)
               (let* ([r (metacontinuation-marks (cdr mc))]
                      ;; Add splice if non-empty
                      [m (let ([mark-splice (metacontinuation-frame-mark-splice mf)])
                           (if (empty-mark-frame? mark-splice)
                               r
                               (cons (make-mark-chain-frame
                                      (strip-impersonator (metacontinuation-frame-tag mf))
                                      (list mark-splice))
                                     r)))]
                      ;; Get marks shallower than the splice
                      [marks (let ([marks (metacontinuation-frame-marks mf)])
                               (if (and (pair? marks)
                                        (let ([mark (car marks)])
                                          (or (eq? mark 'empty)
                                              (empty-mark-frame? mark))))
                                   (cdr marks)
                                   marks))]
                      ;; If marks is empty and splice is non-empty, we don't need
                      ;; to keep the empty list; the splice frame represents the tag
                      [l (if (and (null? marks) (not (eq? m r)))
                             m
                             (cons (make-mark-chain-frame
                                    (strip-impersonator (metacontinuation-frame-tag mf))
                                    marks)
                                   m))])
                 (set-metacontinuation-frame-mark-chain! mf l)
                 l)))]))

(define (maybe-merge-splice splice? mf)
  (cond
   [(and splice? (current-mark-splice))
    => (lambda (mark-splice)
         (current-mark-splice empty-mark-frame)
         (metacontinuation-frame-update-mark-splice mf
                                                   (merge-mark-splice (metacontinuation-frame-mark-splice mf)
                                                                      mark-splice)))]
   [else mf]))

;; ----------------------------------------
;; Continuation-mark table

;; A continuation-mark table within a frame is just an association
;; list. This works well as long as the number of marks per frame is
;; small.

(define empty-mark-table '())
(define empty-mark-table? null?)

(define (mark-table-add mt k v)
  (cons (cons k v) mt))

(define (pair->mark-table k+v)
  (list k+v))

(define (mark-table-remove mt k)
  (cond
   [(null? mt) mt]
   [(eq? k (caar mt)) (cdr mt)]
   [else (let ([mt-rest (mark-table-remove (cdr mt) k)])
           (if (eq? mt-rest (cdr mt))
               mt
               (cons (car mt) mt-rest)))]))

(define (mark-table-add/replace mt k v)
  (mark-table-add (mark-table-remove mt k) k v))

(define (mark-table-ref mt k default wrapper)
  (let ([a (#%assq k mt)])
    (if a
        (if wrapper
            (wrapper (cdr a))
            (cdr a))
        default)))

(define (mark-table-merge a b)
  (cond
   [(null? a) b]
   [(null? b) a]
   [else
    (let loop ([b b] [a a])
      (cond
       [(null? a) b]
       [else (let ([p (car a)])
               (loop (mark-table-add/replace b (car p) (cdr p))
                     (cdr a)))]))]))

;; ----------------------------------------
;; Continuation marks

;; A "mark frame" in the mark stack is one of
;;
;;   - #f = empty-mark-frame = (make-mark-frame empty-mark-table #f #f)
;;   - (cons key val) = (make-mark-frame (pair->mark-table (cons key val)) #f #f)
;;   - a mark frame
;;   - a elem+cache containing one of the others
;;
;; The shorthand forms are promoted to `make-mark-frame` as needed to
;; hold mappings for multiple key. The shorthand forms are also promoted
;; on capture by `current-continuation-marks`; in that case, the mark-stack
;; list is mutated to substitute the promoted form.
;;
;; The list can also be mutated to substitute an `elem+cache` in place
;; of other elements; that substitution happens when searching for a
;; mark in the list.

(define-record continuation-mark-set (mark-chain traces))
(define-record mark-frame (table  ; maps keys to values; use `mark-hash-ref`
                           end-uninterupted?)) ; whether an "in interrupted?" check has been added

(define empty-mark-frame #f)

(define (empty-mark-frame? mf)
  (or (not mf)
      (and (mark-frame? mf)
           (empty-mark-table? (mark-frame-table mf)))))

(define current-mark-stack
  (case-lambda
   [() (#%$current-attachments)]
   [(l) (#%$current-attachments l)]))

;; An extra mark stack of size 0 or 1 that is conceptually appended to
;; the end of `current-mark-stack`, which supports composable
;; continuations.
(define-virtual-register current-mark-splice empty-mark-frame)

(define (mark-frame-update a key val)
  (cond
   [(not a) (if (impersonator? key)
                (mark-frame-update (make-mark-frame '() #f) key val)
                (cons key val))]
   [(pair? a)
    (if (eq? key (car a))
        (cons key val)
        (make-mark-frame (mark-table-add/replace* (pair->mark-table a) a key val)
                         #f))]
   [(eq? a 'empty)
    ;; The current frame is the mark-splice frame, so update
    ;; `current-mark-splice`.
    (current-mark-splice (mark-frame-update (current-mark-splice) key val))
    'empty]
   [(mark-frame? a)
    (make-mark-frame (mark-table-add/replace* (mark-frame-table a) a key val)
                     (mark-frame-end-uninterupted? a))]
   [else
    ;; assert: (elem+cache? a)
    (mark-frame-update (elem+cache-elem a) key val)]))

(define (coerce-to-mark-frame a)
  (cond
   [(mark-frame? a) a]
   [(not a) (make-mark-frame '() #f)]
   [else (make-mark-frame (list a) #f)]))

(define (extract-mark-from-frame a key default-v)
  (let-values ([(key wrapper) (extract-continuation-mark-key-and-wrapper 'call-with-immediate-continuation-mark key)])
    (cond
     [(eq? a 'empty)
      (let ([a (current-mark-splice)])
        ;; `a` is never 'empty or an `elem+cache`
        (cond
         [(pair? a) (if (eq? key (car a)) (if wrapper (wrapper (cdr a)) (cdr a)) default-v)]
         [(mark-frame? a) (mark-table-ref (mark-frame-table a) key default-v wrapper)]
         [else default-v]))]
     [else
      (extract-mark-from-frame* a key default-v wrapper)])))

(define (extract-mark-from-frame* a key default-v wrapper)
  (cond
   [(pair? a) (if (eq? key (car a)) (if wrapper (wrapper (cdr a)) (cdr a)) default-v)]
   [(mark-frame? a) (mark-table-ref (mark-frame-table a) key default-v wrapper)]
   [(elem+cache? a) (extract-mark-from-frame* (elem+cache-elem a) key default-v wrapper)]
   [else default-v]))

(define-syntax with-continuation-mark
  (syntax-rules ()
    [(_ key val body)
     (let* ([k key]
            [v val])
       (#%$call-consuming-continuation-attachment
        empty-mark-frame
        (lambda (a)
          (#%$call-setting-continuation-attachment
           (mark-frame-update a k v)
           (lambda ()
             body)))))]))

;; Specializations of `with-continuation-mark*` as determined by a mode:
(define-syntax with-continuation-mark*
  (lambda (stx)
    (syntax-case stx ()
      [(_ mode key val body)
       (case (syntax->datum #'mode)
         [(general)
          #'(with-continuation-mark key val body)]
         [(push)
          ;; Assume no mark in place for this frame
          #'(let* ([k key]
                   [v val])
              (#%$call-setting-continuation-attachment
               (if (impersonator? k)
                   (mark-frame-update empty-mark-frame k v)
                   (cons k v))
               (lambda ()
                 body)))]
         [(authentic)
          ;; Assume `key` produces an authentic value
          #'(let* ([k key]
                   [v val])
              (#%$call-consuming-continuation-attachment
               empty-mark-frame
               (lambda (a)
                 (#%$call-setting-continuation-attachment
                  (if a
                      (mark-frame-update a k v)
                      (cons k v))
                  (lambda ()
                    body)))))]
         [(push-authentic)
          ;; Assume no mark in place, and `key` produces an authentic value
          #'(let* ([k key]
                   [v val])
              (#%$call-setting-continuation-attachment
               (cons k v)
               (lambda ()
                 body)))]
         [else
          (#%error 'with-continuation-mark* "unrecognized mode: ~s" #'mode)])])))

(define (current-mark-chain)
  (get-mark-chain (current-mark-stack) (current-mark-splice) (current-metacontinuation)))

(define-record mark-chain-frame (tag marks))

(define (get-rest-mark-chain mark-splice mc)
  (let ([mid (and (not (empty-mark-frame? mark-splice))
                  (make-mark-chain-frame
                   #f ; no tag
                   (list mark-splice)))]
        [tl (metacontinuation-marks mc)])
    (if mid
        (cons mid tl)
        tl)))

(define (get-mark-chain mark-stack mark-splice mc)
  (cons (make-mark-chain-frame
         #f ; no tag
         mark-stack)
        (get-rest-mark-chain mark-splice mc)))

(define (prune-mark-chain-prefix tag mark-chain)
  (cond
   [(eq? tag (mark-chain-frame-tag (elem+cache-strip (car mark-chain))))
    mark-chain]
   [else
    (prune-mark-chain-prefix tag (cdr mark-chain))]))

(define (prune-mark-chain-suffix who tag must-find-tag mark-chain)
  (cond
   [(null? mark-chain)
    (when must-find-tag
      (unless (or (eq? tag the-default-continuation-prompt-tag)
                  (eq? tag the-root-continuation-prompt-tag))
        (raise-no-prompt-tag who must-find-tag)))
    null]
   [(eq? tag (mark-chain-frame-tag (elem+cache-strip (car mark-chain))))
    null]
   [else
    (let ([rest-mark-chain (prune-mark-chain-suffix who tag must-find-tag (cdr mark-chain))])
      (if (eq? rest-mark-chain (cdr mark-chain))
          mark-chain
          (cons (car mark-chain)
                rest-mark-chain)))]))

;; Merge immediate frame of `mark-splice` into immediate frame of
;; `mark-stack`, where `mark-stack` takes precedence. We expect that
;; each argument is a stack of length 0 or 1, since that's when
;; merging makes sense.
(define (merge-mark-splice mark-stack mark-splice)
  (cond
   [(empty-mark-frame? mark-stack) mark-splice]
   [(empty-mark-frame? mark-splice) mark-stack]
   [else
    (make-mark-frame (mark-table-merge (mark-frame-table (coerce-to-mark-frame mark-stack))
                                       (mark-frame-table (coerce-to-mark-frame mark-splice)))
                     #f)]))

;; ----------------------------------------
;; Continuation-mark caching

;; A `elem+cache` can replace a plain table in a mark stack within a
;; metacontinuation frame or in a mark-stack chain for a
;; metacontinuation. The cache is a table that records results found
;; later in the list, which allows `continuation-mark-set-first` to
;; take amortized constant time.
(define-record elem+cache (elem cache))
(define (elem+cache-strip t) (if (elem+cache? t) (elem+cache-elem t) t))

;; Export this form renamed to `call-with-immediate-continuation-mark`.
;; It's a macro to ensure that the underlying `call-getting-continuation-attachment`
;; is exposed.
(define-syntax (call-with-immediate-continuation-mark/inline stx)
  (syntax-case stx (lambda |#%name|)
    [(_ key-expr proc-expr)
     #'(call-with-immediate-continuation-mark/inline key-expr proc-expr #f)]
    [(_ key-expr (|#%name| _ (lambda (arg) body ...)) default-v-expr)
     #'(call-with-immediate-continuation-mark/inline key-expr (lambda (arg) body ...) default-v-expr)]
    [(_ key-expr (lambda (arg) body ...) default-v-expr)
     #'(#%$call-getting-continuation-attachment
        empty-mark-frame
        (lambda (a)
          (let* ([key key-expr]
                 [default-v default-v-expr]
                 [arg (extract-mark-from-frame a key default-v)])
            body ...)))]
    [(_ arg ...)
     #'(call-with-immediate-continuation-mark arg ...)]
    [_
     #'call-with-immediate-continuation-mark]))

(define/who call-with-immediate-continuation-mark
  (case-lambda
    [(key proc) (call-with-immediate-continuation-mark key proc #f)]
    [(key proc default-v)
     (check who (procedure-arity-includes/c 1) proc)
     (call-with-immediate-continuation-mark/inline key (lambda (arg) (proc arg)) default-v)]))

(define/who continuation-mark-set-first
  (case-lambda
    [(marks key) (continuation-mark-set-first marks key #f)]
    [(marks key none-v)
     (let ([prompt-tag
            ;; Treat `break-enabled-key` and `parameterization-key`, specially
            ;; so that things like `current-break-parameterization` work without
            ;; referencing the root continuation prompt tag
            (if (or (eq? key break-enabled-key)
                    (eq? key parameterization-key))
                the-root-continuation-prompt-tag
                the-default-continuation-prompt-tag)])
       (cond
         [(not (or marks
                   (impersonator? key)
                   (current-future)))
          ;; Fast path for simple and common case:
          (let ([v (marks-search (current-mark-stack)
                                 key
                                 #f ; at-outer?
                                 prompt-tag
                                 #f)])
            (if (eq? v none)
                (let ([v (marks-search (get-rest-mark-chain (current-mark-splice) (current-metacontinuation))
                                       key
                                       #t ; at-outer?
                                       prompt-tag
                                       #f)])
                  (if (eq? v none)
                      (cond
                        [(eq? key parameterization-key)
                         empty-parameterization]
                        [(eq? key break-enabled-key)
                         (current-engine-init-break-enabled-cell none-v)]
                        [else
                         none-v])
                      v))
                v))]
         [else
          ;; General path:
          (continuation-mark-set-first marks key none-v prompt-tag)]))]
    [(marks key none-v orig-prompt-tag)
     (check who continuation-mark-set? :or-false marks)
     (check who continuation-prompt-tag? orig-prompt-tag)
     (unless marks (maybe-future-barricade orig-prompt-tag))
     (let ([prompt-tag (strip-impersonator orig-prompt-tag)])
       (let-values ([(key wrapper) (extract-continuation-mark-key-and-wrapper 'continuation-mark-set-first key)])
         (let* ([v0 (if marks
                        none
                        ;; Avoid allocating a frame for the immediate marks:
                        (marks-search (current-mark-stack)
                                      key
                                      #f ; at-outer?
                                      prompt-tag
                                      #f))]
                [v (if (eq? v0 none)
                       (marks-search (or (and marks
                                              (continuation-mark-set-mark-chain marks))
                                         ;; We've already checked `(current-mark-stack)`, so get the rest
                                         (get-rest-mark-chain (current-mark-splice) (current-metacontinuation)))
                                     key
                                     #t ; at-outer?
                                     prompt-tag
                                     (not (or marks
                                              (or (eq? prompt-tag the-default-continuation-prompt-tag)
                                                  (eq? prompt-tag the-root-continuation-prompt-tag)))))
                       v0)])
           (cond
            [(eq? v none)
             ;; More special treatment of built-in keys
             (cond
              [(eq? key parameterization-key)
               empty-parameterization]
              [(eq? key break-enabled-key)
               (current-engine-init-break-enabled-cell none-v)]
              [else
               none-v])]
            [(eq? v none2)
             ;; Didn't find prompt tag when searching the current continuation
             (raise-no-prompt-tag who orig-prompt-tag)]
            [wrapper (wrapper v)]
            [else v]))))]))

;; To make `continuation-mark-set-first` constant-time, if we traverse
;; N elements to get an answer, then cache the answer at N/2 elements.
;; The result is `none` is not found.
;; The result is `none2` if `need-tag?` and the prompt tag is never found.
(define (marks-search elems key at-outer? prompt-tag need-tag?)
  (let loop ([elems elems] [elems/cache-pos elems] [depth 0])
    (cond
     [(or (null? elems)
          (and at-outer?
               (not (eq? prompt-tag the-root-continuation-prompt-tag))
               (eq? (mark-chain-frame-tag (elem+cache-strip (car elems))) prompt-tag)))
      ;; Not found
      (cond
       [(and need-tag? (null? elems)) none2]
       [else
        (cache-result! elems/cache-pos depth key none at-outer? prompt-tag)
        none])]
     [else
      (let t-loop ([t (car elems)])
        (cond
         [(elem+cache? t)
          (let ([v (intmap-ref (elem+cache-cache t) key none2)])
            (cond
             [(eq? v none2)
              ;; No mapping in cache, so try the element and continue:
              (t-loop (elem+cache-elem t))]
             [else
              (let ([v (if at-outer?
                           ;; strip & combine --- cache results at the metacontinuation
                           ;; level should depend on the prompt tag, so make the cache
                           ;; value another table level mapping the prompt tag to the value:
                           (hash-ref v prompt-tag none2)
                           v)])
                (cond
                 [(eq? v none2)
                  ;; Strip filtered this cache entry away, so try the element:
                  (t-loop (elem+cache-elem t))]
                 [(eq? v none)
                  ;; The cache records that it's not in the rest:
                  (cache-result! elems/cache-pos depth key none at-outer? prompt-tag)
                  none]
                 [else
                  ;; The cache provides a value from the rest:
                  (cache-result! elems/cache-pos depth key v at-outer? prompt-tag)
                  v]))]))]
         [else
          ;; Try the element:
          (let ([v (if at-outer?
                       ;; Search within the metacontinuation frame:
                       (let ([marks (mark-chain-frame-marks t)])
                         (if (null? marks)
                             none
                             (marks-search marks key #f #f #f)))
                       ;; We're looking at just one frame:
                       (cond
                         ;; Inline common cases:
                         [(pair? t)
                          (if (eq? (car t) key)
                              (cdr t)
                              none)]
                         [(eq? t 'empty) none]
                         [else
                          (extract-mark-from-frame* t key none #f)]))])
            (cond
             [(eq? v none)
              ;; Not found at this point; keep looking
              (loop (cdr elems)
                    (if (fxodd? depth) (cdr elems/cache-pos) elems/cache-pos)
                    (fx+ 1 depth))]
             [else
              ;; Found it
              (cache-result! elems/cache-pos depth key v at-outer? prompt-tag)
              v]))]))])))

;; To make `continuation-mark-set-first` constant-time, cache
;; a key--value mapping at a point that's half-way in
(define (cache-result! marks/cache-pos depth key v at-outer? prompt-tag)
  (unless (< depth 8)
    (let* ([t (car marks/cache-pos)]
           [new-t (if (elem+cache? t)
                      t
                      (make-elem+cache t empty-hasheq))])
      (unless (eq? t new-t)
        (set-car! marks/cache-pos new-t))
      (set-elem+cache-cache! new-t (intmap-set (elem+cache-cache new-t)
                                               key
                                               (if at-outer?
                                                   ;; At the metacontinuation level, cache depends on the
                                                   ;; prompt tag:
                                                   (let ([old (intmap-ref (elem+cache-cache new-t) key empty-hasheq)])
                                                     (intmap-set old prompt-tag v))
                                                   v))))))

(define/who continuation-mark-set->list
  (case-lambda
    [(marks key) (continuation-mark-set->list marks key the-default-continuation-prompt-tag)]
    [(marks key prompt-tag-in)
     (check who continuation-mark-set? :or-false marks)
     (check who continuation-prompt-tag? prompt-tag-in)
     (maybe-future-barricade prompt-tag-in)
     (let ([prompt-tag (strip-impersonator prompt-tag-in)])
       (let-values ([(key wrapper) (extract-continuation-mark-key-and-wrapper 'continuation-mark-set->list key)])
         (let chain-loop ([mark-chain (or (and marks
                                               (continuation-mark-set-mark-chain marks))
                                          (prune-mark-chain-suffix who prompt-tag prompt-tag-in (current-mark-chain)))])
           (cond
            [(null? mark-chain)
             null]
            [else
             (let* ([mcf (elem+cache-strip (car mark-chain))])
               (cond
                [(eq? (mark-chain-frame-tag mcf) prompt-tag)
                 null]
                [else
                 (let loop ([marks (mark-chain-frame-marks mcf)])
                   (cond
                    [(null? marks)
                     (chain-loop (cdr mark-chain))]
                    [else
                     (let* ([v (extract-mark-from-frame* (elem+cache-strip (car marks)) key none wrapper)])
                       (if (eq? v none)
                           (loop (cdr marks))
                           (cons v (loop (cdr marks)))))]))]))]))))]))

(define/who continuation-mark-set->list*
  (case-lambda
    [(marks keys) (continuation-mark-set->list* marks keys #f the-default-continuation-prompt-tag)]
    [(marks keys none-v) (continuation-mark-set->list* marks keys none-v the-default-continuation-prompt-tag)]
    [(marks keys none-v prompt-tag)
     ((do-continuation-mark-set->list* who #f marks keys none-v prompt-tag))]))

(define/who continuation-mark-set->iterator
  (case-lambda
    [(marks keys) (continuation-mark-set->iterator marks keys #f the-default-continuation-prompt-tag)]
    [(marks keys none-v) (continuation-mark-set->iterator marks keys none-v the-default-continuation-prompt-tag)]
    [(marks keys none-v prompt-tag)
     (let ([next (do-continuation-mark-set->list* who #t marks keys none-v prompt-tag)])
       ;; Each `next` call returns `null` when no more values are
       ;; available, otherwise a vector and a new next
       (lambda ()
         (let loop ([next next])
           (call-with-values next
             (case-lambda
              [(done)
               (values #f (lambda () (loop (lambda () null))))]
              [(v new-next)
               (values v (lambda () (loop new-next)))])))))]))

(define (do-continuation-mark-set->list* who iterator? marks keys none-v prompt-tag-in)
  (check who continuation-mark-set? :or-false marks)
  (check who list? keys)
  (check who continuation-prompt-tag? prompt-tag-in)
  (maybe-future-barricade prompt-tag-in)
  (let* ([prompt-tag (strip-impersonator prompt-tag-in)]
         [mark-chain (or (and marks
                              (continuation-mark-set-mark-chain marks))
                         (prune-mark-chain-suffix who prompt-tag prompt-tag-in (current-mark-chain)))])
    (let-values ([(all-keys all-wrappers)
                  (map/2-values (lambda (k)
                                  (extract-continuation-mark-key-and-wrapper who k))
                                keys)])
      (lambda ()
        (let* ([n (length all-keys)]
               [tmp (#%make-vector n)])
          (let chain-loop ([mark-chain mark-chain])
            (cond
             [(null? mark-chain)
              null]
             [else
              (let* ([mcf (elem+cache-strip (car mark-chain))])
                (cond
                 [(eq? (mark-chain-frame-tag mcf) prompt-tag)
                  null]
                 [else
                  (let loop ([marks (mark-chain-frame-marks mcf)])
                    (cond
                     [(null? marks)
                      (chain-loop (cdr mark-chain))]
                     [else
                      (let ([t (elem+cache-strip (car marks))])
                        (let key-loop ([keys all-keys] [wrappers all-wrappers] [i 0] [found? #f])
                          (cond
                           [(null? keys)
                            (if found?
                                (let ([vec (vector-copy tmp)])
                                  (if iterator?
                                      (values vec (lambda () (loop (cdr marks))))
                                      (cons vec (loop (cdr marks)))))
                                (loop (cdr marks)))]
                           [else
                            (let ([v (extract-mark-from-frame* t (car keys) none (car wrappers))])
                              (cond
                               [(eq? v none)
                                (vector-set! tmp i none-v)
                                (key-loop (cdr keys) (cdr wrappers) (add1 i) found?)]
                               [else
                                (vector-set! tmp i v)
                                (key-loop (cdr keys) (cdr wrappers) (add1 i) #t)]))])))]))]))])))))))

(define/who continuation-mark-set->context
  (case-lambda
   [(marks realms?)
    (check who continuation-mark-set? marks)
    (traces->context (continuation-mark-set-traces marks) realms?)]
   [(marks)
    (continuation-mark-set->context marks #f)]))

(define/who current-continuation-marks
  (case-lambda
    [() (current-continuation-marks the-default-continuation-prompt-tag)]
    [(tag)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (call/cc
      (lambda (k)
        (make-continuation-mark-set (prune-mark-chain-suffix who (strip-impersonator tag) tag (current-mark-chain))
                                    (cons (continuation->trace k)
                                          (get-metacontinuation-traces (current-metacontinuation))))))]))

(define/who (current-continuation-marks/no-trace)
  (make-continuation-mark-set (current-mark-chain) null))

;; Wrapped a threads layer to handle thread arguments:
(define/who continuation-marks
  (case-lambda
    [(k) (continuation-marks k (default-continuation-prompt-tag))]
    [(k orig-tag)
     ;; If `k` is a procedure, we assume that it's an engine
     (check who (lambda (p) (or (not p)
                                (continuation? p)
                                (and (#%procedure? p) (procedure-arity-includes? p 0))))
            :contract "(or/c continuation? engine-procedure? #f)"
            k)
     (check who continuation-prompt-tag? orig-tag)
     (maybe-future-barricade orig-tag)
     (let ([tag (strip-impersonator orig-tag)])
       (cond
        [(#%procedure? k) ; => an engine
         (let ([mc (k)])
           (make-continuation-mark-set
            (prune-mark-chain-suffix
             who
             tag
             orig-tag
             (get-mark-chain '() #f mc))
            (get-metacontinuation-traces mc)))]
        [(full-continuation? k)
         (make-continuation-mark-set
          (prune-mark-chain-suffix
           who
           tag
           (and (not (eq? tag (strip-impersonator (full-continuation-tag k))))
                orig-tag)
           (get-mark-chain (full-continuation-mark-stack k)
                           (full-continuation-mark-splice k)
                           (continuation-mc k)))
          (cons (continuation->trace (full-continuation-k k))
                (get-metacontinuation-traces (continuation-mc k))))]
        [(escape-continuation? k)
         (unless (is-continuation-prompt-available? (escape-continuation-tag k) #f)
           (raise-continuation-error '|continuation application|
                                     "escape continuation not in the current continuation"))
         (make-continuation-mark-set
          (prune-mark-chain-suffix
           who
           tag
           orig-tag
           (prune-mark-chain-prefix (escape-continuation-tag k) (current-mark-chain)))
          null)]
        [else
         ;; A `#f` is used to get the marks for a completed thread.
         ;; It would make sense to raise an error for any prompt,
         ;; since the continuaiton is empty, but `continuation-marks`
         ;; is defined to return empty marks in this case.
         (make-continuation-mark-set null null)]))]))

(define (get-metacontinuation-traces mc)
  (cond
   [(null? mc) '()]
   [(metacontinuation-frame-traces (car mc))
    => (lambda (traces) traces)]
   [else
    (let ([traces
           (cons (continuation->trace (metacontinuation-frame-resume-k (car mc)))
                 (get-metacontinuation-traces (cdr mc)))])
      (set-metacontinuation-frame-traces! (car mc) traces)
      traces)]))

;; ----------------------------------------
;; Continuation-mark keys: impersonators, and chaperones

(define-record-type (continuation-mark-key create-continuation-mark-key authentic-continuation-mark-key?)
  (fields (mutable name))) ; `mutable` ensures that `create-...` allocates

(define-record continuation-mark-key-impersonator impersonator (get set))
(define-record continuation-mark-key-chaperone chaperone (get set))

(define make-continuation-mark-key
  (case-lambda
   [() (make-continuation-mark-key (#%gensym))]
   [(name) (create-continuation-mark-key name)]))

(define (continuation-mark-key? v)
  (or (authentic-continuation-mark-key? v)
      (and (impersonator? v)
           (authentic-continuation-mark-key? (impersonator-val v)))))

;; Like `mark-table-add/replace`, but handles continuation-mark-key impersonators
(define (mark-table-add/replace* ht old-a k v)
  (cond
   [(and (impersonator? k)
         (authentic-continuation-mark-key? (impersonator-val k)))
    (let loop ([k k] [v v])
      (cond
       [(or (continuation-mark-key-impersonator? k)
            (continuation-mark-key-chaperone? k))
        (let ([new-v
               ;; Restore attachment while interposing
               (#%$call-setting-continuation-attachment
                old-a
                (lambda ()
                  (let ([new-v (|#%app|
                                (if (continuation-mark-key-impersonator? k)
                                    (continuation-mark-key-impersonator-set k)
                                    (continuation-mark-key-chaperone-set k))
                                v)])
                    (unless (or (continuation-mark-key-impersonator? k)
                                (chaperone-of? new-v v))
                      (raise-chaperone-error 'with-continuation-mark "value" v new-v))
                    new-v)))])
          (loop (impersonator-next k) new-v))]
       [(impersonator? k)
        (loop (impersonator-next k) v)]
       [else
        (mark-table-add/replace ht k v)]))]
   [else (mark-table-add/replace ht k v)]))

;; Extracts the key and converts the wrapper functions into
;; a single function or #f:
(define (extract-continuation-mark-key-and-wrapper who k)
  (cond
   [(and (impersonator? k)
         (authentic-continuation-mark-key? (impersonator-val k)))
    (values
     (impersonator-val k)
     (let loop ([k k])
       (cond
        [(or (continuation-mark-key-impersonator? k)
             (continuation-mark-key-chaperone? k))
         (let ([get (if (continuation-mark-key-impersonator? k)
                        (continuation-mark-key-impersonator-get k)
                        (continuation-mark-key-chaperone-get k))]
               [get-rest (loop (impersonator-next k))])
           (lambda (v)
             (let* ([v (get-rest v)]
                    [new-v (|#%app| get v)])
               (unless (or (continuation-mark-key-impersonator? k)
                           (chaperone-of? new-v v))
                 (raise-chaperone-error who "value" v new-v))
               new-v)))]
        [(impersonator? k)
         (loop (impersonator-next k))]
        [else
         (lambda (v) v)])))]
   [else
    (values k #f)]))

(define (map/2-values f l)
  (cond
   [(null? l) (values '() '())]
   [else
    (let-values ([(a b) (f (car l))])
      (let-values ([(a-r b-r) (map/2-values f (cdr l))])
        (values (cons a a-r) (cons b b-r))))]))

(define (impersonate-continuation-mark-key key get set . props)
  (do-impersonate-continuation-mark-key 'impersonate-continuation-mark-key
                                        key get set props
                                        make-continuation-mark-key-impersonator))

(define (chaperone-continuation-mark-key key get set . props)
  (do-impersonate-continuation-mark-key 'chaperone-continuation-mark-key
                                        key get set props
                                        make-continuation-mark-key-chaperone))

(define (do-impersonate-continuation-mark-key who
                                              key get set props
                                              make-continuation-mark-key-impersonator)
  (check who continuation-mark-key? key)
  (check who (procedure-arity-includes/c 1) get)
  (check who (procedure-arity-includes/c 1) set)
  (make-continuation-mark-key-impersonator (strip-impersonator key)
                                           key
                                           (add-impersonator-properties who
                                                                        props
                                                                        (if (impersonator? key)
                                                                            (impersonator-props key)
                                                                            empty-hasheq))
                                           get
                                           set))

;; ----------------------------------------
;; Continuation prompt tags: impersonators, and chaperones

(define (continuation-prompt-tag? v)
  (or (authentic-continuation-prompt-tag? v)
      (and (impersonator? v)
           (authentic-continuation-prompt-tag? (impersonator-val v)))))

(define-record continuation-prompt-tag-impersonator impersonator (procs))
(define-record continuation-prompt-tag-chaperone chaperone (procs))

(define-record continuation-prompt-tag-procs (handler abort cc-guard cc-impersonate))

(define (continuation-prompt-tag-impersonator-or-chaperone? tag)
  (or (continuation-prompt-tag-impersonator? tag)
      (continuation-prompt-tag-chaperone? tag)))

(define (continuation-prompt-tag-impersonator-or-chaperone-procs tag)
  (if (continuation-prompt-tag-impersonator? tag)
      (continuation-prompt-tag-impersonator-procs tag)
      (continuation-prompt-tag-chaperone-procs tag)))

(define (impersonate-prompt-tag tag handler abort . args)
  (do-impersonate-prompt-tag 'impersonate-prompt-tag tag handler abort args
                             make-continuation-prompt-tag-impersonator))

(define (chaperone-prompt-tag tag handler abort . args)
  (do-impersonate-prompt-tag 'chaperone-prompt-tag tag handler abort args
                             make-continuation-prompt-tag-chaperone))


(define (do-impersonate-prompt-tag who tag handler abort args
                                   make-continuation-prompt-tag-impersonator)
  (check who continuation-prompt-tag? tag)
  (check who procedure? handler)
  (check who procedure? abort)
  (let* ([cc-guard (and (pair? args)
                        (procedure? (car args))
                        (car args))]
         [args (if cc-guard (cdr args) args)]
         [callcc-impersonate (and (pair? args)
                                  (procedure? (car args))
                                  (car args))]
         [args (if callcc-impersonate (cdr args) args)])
    (when callcc-impersonate
      (check who (procedure-arity-includes/c 1) callcc-impersonate))
    (make-continuation-prompt-tag-impersonator
     (strip-impersonator tag)
     tag
     (add-impersonator-properties who
                                  args
                                  (if (impersonator? tag)
                                      (impersonator-props tag)
                                      empty-hasheq))
     (make-continuation-prompt-tag-procs handler abort cc-guard (or callcc-impersonate values)))))

(define (apply-prompt-tag-interposition who at-when what
                                        wrapper args chaperone?)
  (assert-not-in-uninterrupted 'apply-prompt-tag-interposition)
  (call-with-values (lambda () (apply wrapper args))
    (lambda new-args
      (unless (= (length args) (length new-args))
        (raise-result-arity-error #f (length args) (string-append "\n  at: " at-when) new-args))
      (when chaperone?
        (for-each (lambda (arg new-arg)
                    (unless (chaperone-of? new-arg arg)
                      (raise-chaperone-error who what arg new-arg)))
                  args new-args))
      new-args)))

(define (wrap-handler-for-impersonator tag handler)
  (let loop ([tag tag])
    (cond
     [(continuation-prompt-tag-impersonator-or-chaperone? tag)
      (let ([handler (loop (impersonator-next tag))]
            [h (continuation-prompt-tag-procs-handler
                (continuation-prompt-tag-impersonator-or-chaperone-procs tag))]
            [chaperone? (continuation-prompt-tag-chaperone? tag)])
        (lambda args
          (apply handler
                 (apply-prompt-tag-interposition 'call-with-continuation-prompt
                                                 "use of prompt-handler redirecting procedure"
                                                 "prompt-handler argument"
                                                 h args chaperone?))))]
     [(impersonator? tag)
      (loop (impersonator-next tag))]
     [else handler])))

(define (apply-impersonator-abort-wrapper tag args)
  (assert-not-in-uninterrupted 'apply-impersonator-abort-wrapper)
  (let loop ([tag tag] [args args])
    (cond
     [(continuation-prompt-tag-impersonator-or-chaperone? tag)
      (let ([a (continuation-prompt-tag-procs-abort
                (continuation-prompt-tag-impersonator-or-chaperone-procs tag))]
            [chaperone? (continuation-prompt-tag-chaperone? tag)])
        (loop (impersonator-next tag)
              (apply-prompt-tag-interposition 'abort-current-continuation
                                               "use of prompt-abort redirecting procedure"
                                               "prompt-abort argument"
                                               a args chaperone?)))]
     [(impersonator? tag)
      (loop (impersonator-next tag) args)]
     [else args])))

(define (activate-and-wrap-cc-guard-for-impersonator! tag)
  (assert-in-uninterrupted 'activate-and-wrap-cc-guard-for-impersonator!)
  (current-metacontinuation
   (let loop ([mc (current-metacontinuation)])
     (cond
      [(null? mc) mc]
      [(eq? (strip-impersonator tag)
            (strip-impersonator (metacontinuation-frame-tag (car mc))))
       (let* ([mf (car mc)]
              [mf-tag (metacontinuation-frame-tag mf)]
              [mf-cc-guard (metacontinuation-frame-cc-guard mf)])
         (cond
          [(or (continuation-prompt-tag-impersonator-or-chaperone? tag)
               (and (continuation-prompt-tag-impersonator-or-chaperone? mf-tag)
                    (not mf-cc-guard)))
           (cons (metacontinuation-frame-update-cc-guard
                  mf
                  (wrap-cc-guard-for-impersonator tag
                                                  (or mf-cc-guard
                                                      (compose-cc-guard-for-impersonator mf-tag values))))
                 (cdr mc))]
          [else mc]))]
      [else
       (let ([r (loop (cdr mc))])
         (if (eq? r (cdr mc))
             mc
             (cons (car mc) r)))]))))

(define (compose-cc-guard-for-impersonator tag guard)
  (cond
   [(continuation-prompt-tag-impersonator-or-chaperone? tag)
    (let ([cc-guard (continuation-prompt-tag-procs-cc-guard
                     (continuation-prompt-tag-impersonator-or-chaperone-procs tag))]
          [chaperone? (continuation-prompt-tag-chaperone? tag)])
      (let ([guard (compose-cc-guard-for-impersonator (impersonator-next tag)
                                                      guard)])
        (cond
         [cc-guard
          (lambda args
            (apply guard
                   (apply-prompt-tag-interposition 'call-with-continuation-prompt
                                                   "use of `call/cc` result guard"
                                                   "prompt-result argument"
                                                   cc-guard args chaperone?)))]
         [else guard])))]
   [(impersonator? tag)
    (compose-cc-guard-for-impersonator (impersonator-next tag) guard)]
   [else guard]))

(define (wrap-cc-guard-for-impersonator tag cc-guard)
  (assert-in-uninterrupted 'wrap-cc-guard-for-impersonator)
  (cond
   [(continuation-prompt-tag-impersonator-or-chaperone? tag)
    (let ([cc-impersonate (continuation-prompt-tag-procs-cc-impersonate
                           (continuation-prompt-tag-impersonator-or-chaperone-procs tag))]
          [chaperone? (continuation-prompt-tag-chaperone? tag)])
      (let ([cc-guard (wrap-cc-guard-for-impersonator (impersonator-next tag) cc-guard)])
        (let ([new-cc-guard (call-with-continuation-barrier*
                             (lambda ()
                               (assert-not-in-uninterrupted 'cc-guard)
                               (|#%app| cc-impersonate cc-guard)))])
          (start-uninterrupted 'post-cc-guard)
          (when chaperone?
            (unless (chaperone-of? new-cc-guard cc-guard)
              (end-uninterrupted 'cc-guard-fail)
              (raise-chaperone-error 'call-with-current-continuation
                                     "continuation-result guard"
                                     cc-guard
                                     new-cc-guard)))
          new-cc-guard)))]
   [(impersonator? tag)
    (wrap-cc-guard-for-impersonator (impersonator-next tag) cc-guard)]
   [else cc-guard]))

;; ----------------------------------------

(define-virtual-register current-winders '())

(define-record winder (length k marks pre post))

(define (winders-length winders)
  (if (null? winders)
      0
      (winder-length (car winders))))

;; Jobs for `dynamic-wind`:

;; 1. Set the mark stack on entry and exit to the saved mark stack.
;;    The saved mark stack is confined to the current metacontinuation
;;    frame, so it's ok to use it if the current continuation is later
;;    applied to a different metacontinuation.

;; 2. Start and end uninterrupted regions on the boundaries of
;;    transitions between thunks.

;; 3. Perform a built-in `(parameterize-break #f ...)` around the pre
;;    and post thunks. This break parameterization needs to be built
;;    in so that it's put in place before exiting the uninterrupted region,
;;    but it assumes a particular implementation of break
;;    parameterizations.

(define (dynamic-wind pre thunk post)
  (call/cc
   (lambda (k) ; continuation to restore while running pre/post thunk to unwind/rewind
     (let* ([winders (current-winders)]
            [winder (make-winder (if (null? winders)
                                     1
                                     (fx+ 1 (winder-length (car winders))))
                                 k
                                 (current-mark-stack)
                                 pre
                                 post)])
       (start-uninterrupted 'dw)
       (begin
         (call-winder-thunk 'dw-pre pre)
         (current-winders (cons winder winders))
         (end-uninterrupted/call-hook 'dw-body)
         (call-with-values (if (#%procedure? thunk)
                               thunk
                               (lambda () (|#%app| thunk)))
           (lambda args
             (start-uninterrupted 'dw-body)
             (current-winders winders)
             (call-winder-thunk 'dw-post post)
             (end-uninterrupted/call-hook 'dw)
             (if (and (pair? args)
                      (null? (cdr args)))
                 (car args)
                 (#%apply values args)))))))))

(define (call-winder-thunk who thunk)
  (with-continuation-mark
   break-enabled-key (make-thread-cell #f #t)
   (begin
     (end-uninterrupted who)
     (|#%app| thunk)
     (start-uninterrupted who))))

(define (wind-in winders k)
  (do-wind 'dw-pre winders winder-pre k))

(define (wind-out k)
  (do-wind 'dw-post (current-winders) winder-post k))

(define (do-wind who winders winder-thunk k)
  (assert-in-uninterrupted 'do-wind)
  (let ([winder (car winders)]
        [winders (cdr winders)])
    (current-winders winders)
    (let ([thunk (winder-thunk winder)])
      (#%$call-in-continuation
       (winder-k winder)
       (winder-marks winder)
       (lambda ()
         (call-winder-thunk who thunk)
         (k))))))

(define (wind-to exit-winder-n entered-winder-n dest-winders done-k retry-k)
  (let ([starting-metacontinuation (current-metacontinuation)])
    (let loop ([exit-winder-n exit-winder-n])
      (cond
        [(and retry-k
              (not (eq? starting-metacontinuation (current-metacontinuation))))
         (retry-k)]
        [(fx> exit-winder-n 0)
         ;; Go out by one winder
         (wind-out (lambda () (loop (fx- exit-winder-n 1))))]
        [else
         (let loop ([rev-dest-winders '()]
                    [dest-winders dest-winders])
           (cond
             [(fx< entered-winder-n (winders-length dest-winders))
              ;; move winder to `rev-dest-winders`
              (loop (cons (car dest-winders) rev-dest-winders) (cdr dest-winders))]
             [else
              (current-winders dest-winders)
              (let loop ([rev-dest-winders rev-dest-winders])
                (cond
                  [(and retry-k
                        (not (eq? starting-metacontinuation (current-metacontinuation))))
                   (retry-k)]
                  [(null? rev-dest-winders)
                   (done-k)]
                  [else
                   ;; Go in one winder
                   (let ([dest-winders (cons (car rev-dest-winders) (current-winders))]
                         [rev-dest-winders (cdr rev-dest-winders)])
                     (wind-in dest-winders
                              (lambda ()
                                (current-winders dest-winders)
                                (loop rev-dest-winders))))]))]))]))))

(define (same-winders? winders dest-winders-tail)
  (or (and (null? winders)
           (null? dest-winders-tail))
      (and (pair? winders)
           (pair? dest-winders-tail)
           ;; a winder record can exist at most once within a metacontinuation
           ;; frame, and then the tail must be the same, so it's enough to check
           ;; the first item's identity
           (eq? (car winders) (car dest-winders-tail)))))

(define (count-exit+entered-continuation-winders common-mc c rmc-append)
  ;; Find the common tail of
  ;;  * winders of the current metacontinuation up to `common-mc`
  ;;  * immediate winders of `c` plus those of `rmc-append`,
  ;;    which corresponds to the target metacontinuation up to `common-mc`
  ;; The difference of the length of the first input minus the common
  ;; tail length is the number of winders that we'll need to exit.
  ;; The length of the common tail is the number that we do not
  ;; need to enter.
  (cond
    [(and (null? rmc-append)
          (eq? (current-metacontinuation) common-mc))
     ;; Simpler case: the source and destination winders are
     ;; each in a single list, and we can use the `length` field
     ;; to find a common part without reversing anything
     (let loop ([src-winders (current-winders)]
                [dest-winders (full-continuation-winders c)]
                [exit-n 0])
       (cond
         [(null? src-winders) (values exit-n 0)]
         [(null? dest-winders) (values (fx+ exit-n (winder-length (car src-winders))) 0)]
         [else
          (let ([src-i (winder-length (car src-winders))]
                [dest-i (winder-length (car dest-winders))])
            (cond
              [(fx= src-i dest-i)
               (let loop ([src-winders src-winders]
                          [dest-winders dest-winders]
                          [exit-n exit-n])
                 (cond
                   [(null? src-winders) (values exit-n 0)]
                   [(eq? (car src-winders) (car dest-winders))
                    (values exit-n (winder-length (car dest-winders)))]
                   [else
                    (loop (cdr src-winders) (cdr dest-winders) (fx+ exit-n 1))]))]
              [(fx< src-i dest-i)
               (loop src-winders (list-tail dest-winders (fx- dest-i src-i)) exit-n)]
              [else
               (let ([delta (fx- src-i dest-i)])
                 (loop (list-tail src-winders delta) dest-winders (fx+ exit-n delta)))]))]))]
    [else
     ;; This could be made faster, perhaps, by taking advantage of depth
     ;; information instead of flatting out both winder lists. But at least
     ;; it's linear time in the length of two winder lists.
     (let loop ([mc (current-metacontinuation)]
                [rev-src-winders (reverse (current-winders))])
       (cond
         [(eq? mc common-mc)
          (cond
            [(null? rev-src-winders) (values 0 0)]
            [else
             (let loop ([rev-src-winders rev-src-winders]
                        [rev-dest-winders
                         (let loop ([rmc-append rmc-append])
                           (cond
                             [(null? rmc-append) (reverse (full-continuation-winders c))]
                             [else (append (reverse (metacontinuation-frame-winders (car rmc-append)))
                                           (loop (cdr rmc-append)))]))]
                        [n (length rev-src-winders)]
                        [common-n 0])
               (if (or (null? rev-src-winders)
                       (null? rev-dest-winders)
                       (not (eq? (car rev-src-winders)
                                 (car rev-dest-winders))))
                   (values n common-n)
                   (loop (cdr rev-src-winders) (cdr rev-dest-winders) (fx- n 1) (fx+ common-n 1))))])]
         [else
          (let ([mf (car mc)])
            (loop (cdr mc)
                  (append (reverse (metacontinuation-frame-winders mf))
                          rev-src-winders)))]))]))

;; ----------------------------------------

(define (raise-continuation-error who msg)
  (raise
   (|#%app|
    exn:fail:contract:continuation
    (string-append (symbol->string who) ": " msg)
    (current-continuation-marks))))

;; ----------------------------------------
;; Breaks

(define (call-with-break-disabled thunk)
  (with-continuation-mark
    break-enabled-key (make-thread-cell #f #t)
    (thunk)))

;; Some points where we jump out of uninterrupted mode are also points
;; where we might jump to a context where breaks are allowed. The
;; `continuation-mark-change-hook` function allows a thread scheduler to
;; inject a check at those points.
(define (end-uninterrupted/call-hook who)
  (end-uninterrupted who)
  (break-enabled-transition-hook))

(define break-enabled-transition-hook void)

(define (set-break-enabled-transition-hook! proc)
  (set! break-enabled-transition-hook proc))

(define (apply-with-break-transition k all-marks args)
  (#%$call-in-continuation
   k
   all-marks
   (lambda ()
     (break-enabled-transition-hook)
     (if (#%procedure? args)
         (args)
         (#%apply values args)))))

;; ----------------------------------------
;; Metacontinuation swapping for engines

(define empty-metacontinuation '())

;; Similar to `call-with-current-continuation` plus
;; applying an old continuation, but does not run winders;
;; this operation makes sense for thread or engine context
;; switches
(define (call-with-current-metacontinuation proc)
  (cond
   [(current-system-wind-start-k)
    => (lambda (k) (call-with-current-metacontinuation-with-system-wind proc k))]
   [else
    (call-with-empty-metacontinuation-frame-for-swap proc)]))

(define (apply-meta-continuation saved k)
  (current-metacontinuation saved)
  (k))

;; ----------------------------------------

;; In "system-wind" mode for the current metacontinuation frame, run
;; the frame's winders when jumping out of the frame or back in,
;; because the frame uses host-Scheme parameters and/or `fluid-let`.
;; For example, jumping out/in the host compiler needs to save/restore
;; compiler state.
(define-virtual-register current-system-wind-start-k #f)

;; During `call-with-system-wind`, the current metacontinuation frame
;; must remain as the most recent one, so that `swap-metacontinuation`
;; can capture the system-wind part
(define (call-with-system-wind proc)
  ((call/cc
    (lambda (k)
      (current-system-wind-start-k k)
      (#%dynamic-wind
       void
       (lambda ()
         (call-with-values
             proc
           (lambda args
             (lambda ()
               (#%apply values args)))))
       (lambda () (current-system-wind-start-k #f)))))))

(define (call-with-current-metacontinuation-with-system-wind proc start-k)
  (current-system-wind-start-k #f)
  (call/cc
   (lambda (system-wind-k) ; continuation with system `dynamic-wind` behavior
     ;; escape to starting point, running winders, before
     ;; capturing the rest of the metacontinuation:
     (start-k (lambda ()
                (let ([prefix (call-with-current-metacontinuation proc)])
                  (current-system-wind-start-k start-k)
                  (system-wind-k prefix)))))))

(define (assert-not-in-system-wind who)
  (CHECK-uninterrupted
   (when (current-system-wind-start-k)
     (internal-error 'not-in-system-wind (format "~a: assertion failed" who)))))

;; ----------------------------------------

(define (call-with-current-continuation-roots proc)
  (call/cc
   (lambda (k)
     (proc (cons k (current-metacontinuation))))))
