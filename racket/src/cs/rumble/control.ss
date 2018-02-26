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
;;                    --- empty-k
;; metacontinuation  |
;;     frame         |
;;                   |--- resume-k
;;                   |<-- tag represents this point
;;                    --- empty-k
;; metacontinuation  |
;;     frame         |
;;                   |
;;                   |--- resume-k
;;                   |<-- tag represents this point
;;                    --- empty-k
;;   current host    |
;;   continuation    |
;;                   v

;; Concretely, the metacontinuation is the current host continuation
;; plus the frames in the list `(current-metacontinuation)`, where the
;; shallowest (= lowest in the picture above) frame is first in the
;; list. The `empty-k` value of the current host continuation is
;; in `current-empty-k`.

;; The shallowest metacontinuation frame's `empty-k` continuation is
;; used to detect when the current host continuation is empty (i.e.,
;; when it matches the `current-empty-k` value). When it's empty, then
;; calling a composable continuation doesn't need to add a new
;; metacontinuation frame, and the application gets the right "tail"
;; behavior.

;; The shallowest metacontinuation frame's `empty-k` continuation also
;; indicates which continuation's marks (if any) should be spliced
;; into a new context when captured in a composable continuation. See
;; also `current-mark-splice` below.

;; A metacontinuation frame's `resume-k` is called when control
;; returns or aborts to the frame:
;;
;; * When returning normally to a metacontinuation frame, the
;;   `resume-k` continuation receives a function for values returned
;;   to the frame.
;;
;; * When aborting to a prompt tag, the `resume-k` continination
;;   receives a special value that indicates an abort with arguments.
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
;; host continuation are kept in `current-mark-stack`. When a
;; metacontinuation frame is created, it takes the current
;; `current-mark-stack` value and `current-mark-stack` is set back to
;; empty. To keep winders and the mark stack in sync, a `dynamic-wind`
;; pre or post thunk resets the mark stack on entry.

;; When a composable continuation is applied in a continuation frame
;; that has marks, then the marks are moved into `current-mark-splice`,
;; which is conceptually merged into the tai of `current-mark-stack`.
;; Having a separate `current-mark-splice` enables `dynamic-wind`
;; pre and post thunks adapt correctly to the splicing while jumping
;; into or out of the continuation.

;; A metacontinuation frame has an extra cache slot to contain a list
;; of mark-stack lists down to the root continuation. When a delimited
;; sequence of metacontinuation frames are copied out of or into the
;; metacontinuation, the slot is flushed and will be reset on demand.

;; Continuations are used to implement engines, but it's important
;; that an engine doesn't get swapped out (or, more generally,
;; asynchronous signals are handled at the Racket level) while we're
;; manipulating the continuation representation. A bad time for a swap
;; is an "interrupted" region. The `begin-uninterrupted` and
;; `end-uninterrupted` functions bracket such regions dynamically. See
;; also "rumble/engine.ss" and "rumble/interrupt.ss"

(define-virtual-register current-metacontinuation '())

(define-virtual-register current-empty-k #f)

(define-record metacontinuation-frame (tag          ; continuation prompt tag or #f
                                       resume-k     ; delivers values to the prompt
                                       empty-k      ; deepest end of this frame
                                       winders      ; `dynamic-wind` winders
                                       mark-stack   ; mark stack to restore
                                       mark-splice  ; extra part of mark stack to restore
                                       mark-chain   ; #f or a cached list of mark-chain-frame or elem+cache
                                       traces       ; #f or a cached list of traces
                                       cc-guard))   ; for impersonated tag, initially #f

;; Messages to `resume-k[/no-wind]`:
(define-record aborting (args))

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
(define (root-continuation-prompt-tag) the-root-continuation-prompt-tag)

;; To support special treatment of break parameterizations, and also
;; to initialize disabled breaks for `dynamic-wind` pre and post
;; thunks:
(define break-enabled-key (gensym 'break-enabled))

;; FIXME: add caching to avoid full traversal
(define/who (continuation-prompt-available? tag)
  (check who continuation-prompt-tag? tag)
  (let ([tag (strip-impersonator tag)])
    (or (eq? tag the-default-continuation-prompt-tag)
        (eq? tag the-root-continuation-prompt-tag)
        (let loop ([mc (current-metacontinuation)])
          (cond
           [(null? mc)
            (eq? tag the-default-continuation-prompt-tag)]
           [(eq? tag (strip-impersonator (metacontinuation-frame-tag (car mc))))
            #t]
           [else (loop (cdr mc))])))))

(define/who (maybe-future-barricade tag)
  (when (future? (current-future)) ;; running in a future
    (check who continuation-prompt-tag? tag)
    (let ([fp (strip-impersonator (current-future-prompt))]
          [tag (strip-impersonator tag)])
      (cond
       [(eq? tag the-root-continuation-prompt-tag)
        (block)]
       [else
        (let loop ([mc (current-metacontinuation)])
          (cond
           [(null? mc)
            ;; Won't happen normally, since every thread starts with a explicit prompt
            (block)]
           [(eq? tag (strip-impersonator (metacontinuation-frame-tag (car mc))))
            (void)]
           [(eq? (metacontinuation-frame-tag (car mc)) fp)
            ;; tag must be above future prompt
            (block)]
           [else
            (loop (cdr mc))]))]))))

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
      #f ; not a tail call
      (lambda ()
        (end-uninterrupted 'prompt)
        ;; Make room for a slicing continuation-mark frame, in case this
        ;; metacontinuation frame is capture and composed in a context
        ;; that already has marks:
        (call-with-splice-k
         (lambda ()
           ;; Finally, apply the given function:
           (apply proc args)))))]))

(define (make-default-abort-handler tag)
  (lambda (abort-thunk)
    (check 'default-continuation-prompt-handler (procedure-arity-includes/c 0) abort-thunk)
    (call-with-continuation-prompt abort-thunk tag #f)))

(define (resume-metacontinuation results)
  ;; pop a metacontinuation frame
  (cond
   [(null? (current-metacontinuation)) (engine-return)]
   [else
    (start-uninterrupted 'resume-mc)
    (let ([mf (car (current-metacontinuation))])
      (pop-metacontinuation-frame)
      ;; resume
      ((metacontinuation-frame-resume-k mf) results))]))

(define (pop-metacontinuation-frame)
  (let ([mf (car (current-metacontinuation))])
    (current-metacontinuation (cdr (current-metacontinuation)))
    (current-winders (metacontinuation-frame-winders mf))
    (current-mark-stack (metacontinuation-frame-mark-stack mf))
    (current-mark-splice (metacontinuation-frame-mark-splice mf))
    (current-empty-k (metacontinuation-frame-empty-k mf))))

(define (call-in-empty-metacontinuation-frame tag handler tail? proc)
  ;; Call `proc` in an empty metacontinuation frame, reifying the
  ;; current metacontinuation as needed (i.e., if non-empty) as a new
  ;; frame on `*metacontinuations*`; if the tag is #f and the
  ;; current metacontinuation frame is already empty, don't push more
  (assert-in-uninterrupted)
  (assert-not-in-system-wind)
  (call/cc
   (lambda (tail-k)
     (cond
      [(and (eq? tag the-compose-prompt-tag)
            (eq? tail-k (current-empty-k)))
       ;; empty continuation in the current frame; don't push a new
       ;; metacontinuation frame; if the mark stack is non-empty,
       ;; merge it into the mark splice
       (current-mark-splice (merge-mark-splice (current-mark-stack) (current-mark-splice)))
       (current-mark-stack '())
       (proc)]
      [else
       (let ([r ; a list of results, or a non-list for special handling
              (call/cc
               (lambda (k)
                 ;; Push another continuation frame so we can drop its `next`
                 (call-as-non-tail
                  (lambda ()
                    ;; drop the rest of the current continuation from the
                    ;; new metacontinuation frame:
                    (#%$current-stack-link #%$null-continuation)
                    (let-values ([results
                                  (call/cc
                                   ;; remember the "empty" continuation frame
                                   ;; that just continues the metacontinuation:
                                   (lambda (empty-k)
                                     (let ([mf (make-metacontinuation-frame tag
                                                                            k
                                                                            (current-empty-k)
                                                                            (current-winders)
                                                                            (if tail?
                                                                                (prune-immediate-frame (current-mark-stack) tail-k)
                                                                                (current-mark-stack))
                                                                            (current-mark-splice)
                                                                            #f
                                                                            #f
                                                                            #f)])
                                       (current-winders '())
                                       (current-empty-k empty-k)
                                       (current-mark-splice (and tail?
                                                                 (keep-immediate-frame (current-mark-stack) tail-k empty-k)))
                                       (current-mark-stack #f)
                                       ;; push the metacontinuation:
                                       (current-metacontinuation (cons mf (current-metacontinuation)))
                                       ;; ready:
                                       (proc))))])
                      ;; Prepare to use cc-guard, if one was enabled:
                      (let ([cc-guard (or (metacontinuation-frame-cc-guard (car (current-metacontinuation)))
                                          values)])
                        ;; Continue normally; the metacontinuation could be different
                        ;; than when we captured this metafunction frame, though:
                        (resume-metacontinuation
                         ;; Apply the cc-guard, if any, outside of the prompt:
                         (lambda () (apply cc-guard results)))))))))])
         (cond
          [(aborting? r)
           ;; Remove the prompt as we call the handler:
           (pop-metacontinuation-frame)
           (end-uninterrupted 'handle)
           (apply handler
                  (aborting-args r))]
          [else
           ;; We're returning normally; the metacontinuation frame has
           ;; been popped already by `resume-metacontinuation`
           (end-uninterrupted 'resume)
           (r)]))]))))

(define (call-as-non-tail proc)
  (proc)
  '(error 'call-as-non-tail "shouldn't get to frame that was meant to be discarded"))

(define (metacontinuation-frame-update-mark-stack current-mf mark-stack mark-splice)
  (make-metacontinuation-frame (metacontinuation-frame-tag current-mf)
                               (metacontinuation-frame-resume-k current-mf)
                               (metacontinuation-frame-empty-k current-mf)
                               (metacontinuation-frame-winders current-mf)
                               mark-stack
                               mark-splice
                               #f
                               #f
                               (metacontinuation-frame-cc-guard current-mf)))

(define (metacontinuation-frame-update-cc-guard current-mf cc-guard)
  ;; Ok to keep caches, since the cc-guard doesn't affect them
  (make-metacontinuation-frame (metacontinuation-frame-tag current-mf)
                               (metacontinuation-frame-resume-k current-mf)
                               (metacontinuation-frame-empty-k current-mf)
                               (metacontinuation-frame-winders current-mf)
                               (metacontinuation-frame-mark-stack current-mf)
                               (metacontinuation-frame-mark-splice current-mf)
                               (metacontinuation-frame-mark-chain current-mf)
                               (metacontinuation-frame-traces current-mf)
                               cc-guard))
 
;; ----------------------------------------

(define/who (abort-current-continuation tag . args)
  (check who continuation-prompt-tag? tag)
  (maybe-future-barricade tag)
  (check-prompt-tag-available who (strip-impersonator tag))
  (start-uninterrupted 'abort)
  (let ([args (apply-impersonator-abort-wrapper tag args)]
        [tag (strip-impersonator tag)])
    (do-abort-current-continuation who tag args #t)))

(define/who (unsafe-abort-current-continuation/no-wind tag arg)
  (start-uninterrupted 'abort)
  (let ([args (apply-impersonator-abort-wrapper tag (list arg))]
        [tag (strip-impersonator tag)])
    (do-abort-current-continuation who tag args #f)))

(define (do-abort-current-continuation who tag args wind?)
  (assert-in-uninterrupted)
  (cond
   [(null? (current-metacontinuation))
    ;; A reset handler must end the uninterrupted region:
    ((reset-handler))]
   [(or (not wind?)
        (null? (current-winders)))
    (let ([mf (car (current-metacontinuation))])
      (cond
       [(eq? tag (strip-impersonator (metacontinuation-frame-tag mf)))
        ((metacontinuation-frame-resume-k mf)
         (make-aborting args))]
       [else
        ;; Aborting to an enclosing prompt, so keep going:
        (pop-metacontinuation-frame)
        (do-abort-current-continuation who tag args wind?)]))]
   [else
    (wind-to
     '()
     ;; No winders left:
     (lambda ()
       (do-abort-current-continuation who tag args wind?))
     ;; If the metacontinuation changes, check target before retrying:
     (lambda ()
       (check-prompt-still-available who tag)
       (do-abort-current-continuation who tag args wind?)))]))

(define (check-prompt-still-available who tag)
  (unless (continuation-prompt-available? tag)
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
  (call-in-empty-metacontinuation-frame
   the-barrier-prompt-tag ; <- recognized as a barrier by continuation capture or call
   #f
   #f ; not a tail call
   (lambda ()
     (end-uninterrupted 'barrier)
     (|#%app| p))))

;; ----------------------------------------
;; Capturing and applying continuations

(define-record continuation ())
(define-record full-continuation continuation (k winders mark-stack mark-splice empty-k mc))
(define-record composable-continuation full-continuation ())
(define-record composable-continuation/no-wind composable-continuation ())
(define-record non-composable-continuation full-continuation (tag))
(define-record escape-continuation continuation (tag))

(define/who call-with-current-continuation
  (case-lambda
    [(proc) (call-with-current-continuation proc
                                            the-default-continuation-prompt-tag)]
    [(proc tag)
     (check who (procedure-arity-includes/c 1) proc)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (call-with-end-uninterrupted
      (lambda ()
        (call/cc
         (lambda (k)
           (|#%app|
            proc
            (make-non-composable-continuation
             k
             (current-winders)
             (current-mark-stack)
             (current-mark-splice)
             (current-empty-k)
             (extract-metacontinuation 'call-with-current-continuation (strip-impersonator tag) #t)
             tag))))))]))

(define/who call-with-composable-continuation
  (case-lambda
    [(p) (call-with-composable-continuation p the-default-continuation-prompt-tag)]
    [(p tag)
     (check who (procedure-arity-includes/c 1) p)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (call-with-composable-continuation* p tag #t)]))

(define (call-with-composable-continuation* p tag wind?)
  (call-with-end-uninterrupted
   (lambda ()
     (call/cc
      (lambda (k)
        (|#%app|
         p
         ((if wind?
              make-composable-continuation
              make-composable-continuation/no-wind)
          k
          (current-winders)
          (current-mark-stack)
          (current-mark-splice)
          (current-empty-k)
          (extract-metacontinuation 'call-with-composable-continuation (strip-impersonator tag) #f))))))))

(define (unsafe-call-with-composable-continuation/no-wind p tag)
  (call-with-composable-continuation* p tag #f))

(define/who (call-with-escape-continuation p)
  (check who (procedure-arity-includes/c 1) p)
  (let ([tag (make-continuation-prompt-tag)])
    (call-with-continuation-prompt
     (lambda ()
       (|#%app| p (make-escape-continuation tag)))
     tag
     values)))

;; Applying a continuation calls this internal function:
(define (apply-continuation c args)
  (start-uninterrupted 'continue)
  (cond
   [(composable-continuation? c)
    ;; To compose the metacontinuation, first make sure the current
    ;; continuation is reified in `(current-metacontinuation)`:
    (call-in-empty-metacontinuation-frame
     the-compose-prompt-tag
     fail-abort-to-delimit-continuation
     #t ; a tail call
     (lambda ()
       ;; The current metacontinuation frame has an
       ;; empty continuation, so we can "replace" that
       ;; with the composable one:
       (if (composable-continuation/no-wind? c)
           (apply-immediate-continuation/no-wind c args)
           (apply-immediate-continuation c (reverse (full-continuation-mc c)) args))))]
   [(non-composable-continuation? c)
    (apply-non-composable-continuation c args)]
   [(escape-continuation? c)
    (let ([tag (escape-continuation-tag c)])
      (unless (continuation-prompt-available? tag)
        (end-uninterrupted 'escape-fail)
        (raise-continuation-error '|continuation application|
                                  "attempt to jump into an escape continuation"))
      (do-abort-current-continuation '|continuation application| tag args #t))]))

(define (apply-non-composable-continuation c args)
  (assert-in-uninterrupted)
  (let* ([tag (non-composable-continuation-tag c)])
    (let-values ([(common-mc   ; shared part of the current metacontinuation
                   rmc-append) ; non-shared part of the destination metacontinuation
                  ;; We check every time, just in case control operations
                  ;; change the current continuation out from under us.
                  (find-common-metacontinuation (full-continuation-mc c)
                                                (current-metacontinuation)
                                                (strip-impersonator tag))])
      (let loop ()
        (cond
         [(eq? common-mc (current-metacontinuation))
          ;; Replace the current metacontinuation frame's continuation
          ;; with the saved one; this replacement will take care of any
          ;; shared winders within the frame.
          (apply-immediate-continuation c rmc-append args)]
         [else
          ;; Unwind this metacontinuation frame:
          (wind-to
           '()
           ;; If all winders complete simply:
           (lambda ()
             (pop-metacontinuation-frame)
             (loop))
           ;; If a winder changes the metacontinuation, then
           ;; start again:
           (lambda ()
             (apply-non-composable-continuation c args)))])))))

;; Apply a continuation within the current metacontinuation frame:
(define (apply-immediate-continuation c rmc args)
  (assert-in-uninterrupted)
  (call-with-appended-metacontinuation
   rmc
   c
   args
   (lambda ()
     (let ([mark-stack (full-continuation-mark-stack c)]
           [empty-k (full-continuation-empty-k c)])
       (current-mark-splice (let ([mark-splice (full-continuation-mark-splice c)])
                              (if (composable-continuation? c)
                                  (prune-mark-splice (merge-mark-splice mark-splice (current-mark-splice))
                                                     mark-stack
                                                     empty-k)
                                  mark-splice)))
       (current-empty-k empty-k)
       (wind-to
        (full-continuation-winders c)
        ;; When no winders are left:
        (lambda ()
          (current-mark-stack mark-stack)
          (when (non-composable-continuation? c)
            ;; Activate/add cc-guards in target prompt; any user-level
            ;; callbacks here are run with a continuation barrier, so
            ;; the metacontinuation won't change (except by escaping):
            (activate-and-wrap-cc-guard-for-impersonator!
             (non-composable-continuation-tag c)))
          (apply (full-continuation-k c) args))
        ;; If a winder changed the meta-continuation, try again for a
        ;; non-composable continuation:
        (and (non-composable-continuation? c)
             (lambda ()
               (apply-non-composable-continuation c args))))))))

;; Like `apply-immediate-continuation`, but don't run winders
(define (apply-immediate-continuation/no-wind c args)
  (current-metacontinuation (append
                             (map metacontinuation-frame-clear-cache (full-continuation-mc c))
                             (current-metacontinuation)))
  (current-winders (full-continuation-winders c))
  (current-mark-stack (full-continuation-mark-stack c))
  (current-mark-splice (full-continuation-mark-splice c))
  (current-empty-k (full-continuation-empty-k c))
  (apply (full-continuation-k c) args))

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
  (let-values ([(rev-current ; (list (cons mf mc) ...)
                 base-current-mc)
                ;; Get the reversed prefix of `current-mc` that is to be
                ;; replaced by `mc`:
                (let loop ([current-mc current-mc] [accum null])
                  (cond
                   [(null? current-mc)
                    (unless (or (eq? tag the-default-continuation-prompt-tag)
                                (eq? tag the-root-continuation-prompt-tag))
                      (do-raise-arguments-error '|continuation application|
                                                "continuation includes no prompt with the given tag"
                                                exn:fail:contract:continuation
                                                (list "tag" tag)))
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
  (unless (null? rev-mc)
    (when (eq? (metacontinuation-frame-tag (car rev-mc))
               the-barrier-prompt-tag)
      (raise-barrier-error))
    (check-for-barriers (cdr rev-mc))))

(define (raise-barrier-error)
  (end-uninterrupted 'hit-barrier)
  (raise-continuation-error '|continuation application|
                            "attempt to cross a continuation barrier"))

(define (call-with-end-uninterrupted thunk)
  ;; Using `call/cm` with a key of `none` ensures that we have an
  ;; `(end-uninterrupted)` in the immediate continuation, but
  ;; keeping the illusion that `thunk` is called in tail position.
  (call/cm none #f thunk))

;; Update `splice-k` to be the "inside" of a continuation prompt.
(define (call-with-splice-k thunk)
  (call-with-end-uninterrupted
   (lambda ()
     (call/cc
      (lambda (k)
        (current-empty-k k)
        (thunk))))))

(define (set-continuation-applicables!)
  (let ([add (lambda (rtd)
               (struct-property-set! prop:procedure
                                     rtd
                                     (lambda (c . args)
                                       (apply-continuation c args))))])
    (add (record-type-descriptor composable-continuation))
    (add (record-type-descriptor composable-continuation/no-wind))
    (add (record-type-descriptor non-composable-continuation))
    (add (record-type-descriptor escape-continuation))))

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
          (do-raise-arguments-error who "continuation includes no prompt with the given tag"
                                    exn:fail:contract:continuation
                                    (list "tag" tag)))
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
  (unless (continuation-prompt-available? tag)
    (do-raise-arguments-error who "continuation includes no prompt with the given tag"
                              exn:fail:contract:continuation
                              (list "tag" tag))))

(define (call-with-appended-metacontinuation rmc dest-c dest-args proc)
  ;; Assumes that the current metacontinuation frame is ready to be
  ;; replaced with `mc` (reversed as `rmc`) plus `proc`.
  ;; In the simple case of no winders and an empty frame immediate
  ;;  metacontinuation fame, we could just
  ;;  (current-metacontinuation (append mc (current-metacontinuation)))
  ;; But, to run winders and replace anything in the current frame,
  ;; we proceed frame-by-frame in `mc`.
  (assert-in-uninterrupted)
  (let loop ([rmc rmc])
    (cond
     [(null? rmc) (proc)]
     [else
      (let ([mf (maybe-merge-splice (composable-continuation? dest-c)
                                    (metacontinuation-frame-clear-cache (car rmc)))]
            [rmc (cdr rmc)])
        ;; Set splice before jumping, so it can be used by winders
        (current-mark-splice (metacontinuation-frame-mark-splice mf))
        ;; Run "in" winders for the metacontinuation
        (wind-to
         (metacontinuation-frame-winders mf)
         ;; When all winders done for this frame:
         (lambda ()
           (current-metacontinuation (cons mf (current-metacontinuation)))
           (current-winders '())
           (loop rmc))
         ;; When a winder changes the metacontinuation, try again
         ;; for a non-composable continuation:
         (and (non-composable-continuation? dest-c)
              (lambda ()
                (apply-non-composable-continuation dest-c dest-args)))))])))

(define (metacontinuation-frame-clear-cache mf)
  (metacontinuation-frame-update-mark-stack mf
                                            (metacontinuation-frame-mark-stack mf)
                                            (metacontinuation-frame-mark-splice mf)))

;; Get/cache a converted list of marks for a metacontinuation
(define (metacontinuation-marks mc)
  (cond
   [(null? mc) null]
   [else (let ([mf (car mc)])
           (or (metacontinuation-frame-mark-chain mf)
               (let* ([r (metacontinuation-marks (cdr mc))]
                      [m (let ([mark-splice (metacontinuation-frame-mark-splice mf)])
                           (if mark-splice
                               (cons (make-mark-chain-frame
                                      (strip-impersonator (metacontinuation-frame-tag mf))
                                      ;; maybe splicing:
                                      (mark-stack-tail-matches? (metacontinuation-frame-mark-stack mf)
                                                                (mark-stack-frame-k mark-splice))
                                      (mark-stack-to-marks mark-splice))
                                     r)
                               r))]
                      [l (cons (make-mark-chain-frame
                                (strip-impersonator (metacontinuation-frame-tag mf))
                                #t ; not splicing
                                (mark-stack-to-marks
                                 (metacontinuation-frame-mark-stack mf)))
                               m)])
                 (set-metacontinuation-frame-mark-chain! mf l)
                 l)))]))

(define (maybe-merge-splice splice? mf)
  (cond
   [(and splice? (current-mark-splice))
    => (lambda (mark-splice)
         (current-mark-splice #f)
         (metacontinuation-frame-update-mark-stack mf
                                                   (metacontinuation-frame-mark-stack mf)
                                                   (merge-mark-splice (metacontinuation-frame-mark-splice mf)
                                                                      mark-splice)))]
   [else mf]))

;; ----------------------------------------
;; Continuation marks

(define-record continuation-mark-set (mark-chain traces))
(define-record mark-stack-frame (prev   ; prev frame
                                 k      ; continuation for this frame
                                 table  ; intmap mapping keys to values
                                 flat)) ; #f or cached list that contains only tables and elem+caches

;; A mark stack is made of marks-stack frames:
(define-virtual-register current-mark-stack #f)

;; An extra mark stack of size 0 or 1 that is conceptually appended to
;; the end of `current-mark-stack`, mainly to support composable
;; continuations and `dynamic-wind`. If the last frame of
;; `current-mark-stack` has the same `k` as a frame in
;; `current-mark-stack-splice`, then then frames are conceptually
;; merged, so no key should be inthe mark-splice frame if it's in the
;; mark-stack frame.
(define-virtual-register current-mark-splice #f)

;; See copy in "expander.sls"
(define-syntax with-continuation-mark
  (syntax-rules ()
    [(_ key val body)
     (call/cm key val (lambda () body))]))

;; Sets a continuation mark.
;; Using `none` as a key ensures that a
;; stack-restoring frame is pushed without
;; adding a key--value mapping.
(define (call/cm key val proc)
  (call/cc
   (lambda (k)
     (when (eq? k (current-empty-k))
       ;; Need to merge the main stack and splice, if both are active
       (when (current-mark-splice)
         (merge-mark-splice!)))
     (let ([mark-stack (current-mark-stack)])
       (cond
        [(and mark-stack
              (eq? k (mark-stack-frame-k mark-stack)))
         (unless (eq? key none)
           (current-mark-stack (make-mark-stack-frame (mark-stack-frame-prev mark-stack)
                                                      k
                                                      (intmap-set/cm-key (mark-stack-frame-table mark-stack)
                                                                         key
                                                                         val)
                                                      #f)))
         (proc)]
        [else
         (begin0
          (call/cc
           (lambda (new-k)
             (current-mark-stack
              (make-mark-stack-frame mark-stack
                                     new-k
                                     (if (eq? key none)
                                         empty-hasheq
                                         (intmap-set/cm-key empty-hasheq key val))
                                     #f))
             (proc)))
          (current-mark-stack (mark-stack-frame-prev (current-mark-stack)))
          ;; To support exiting an uninterrupted region on resumption of
          ;; a continuation (see `call-with-end-uninterrupted`):
          (when (current-in-uninterrupted)
            (pariah (end-uninterrupted/call-hook 'cm))))])))))

;; For internal use, such as `dynamic-wind` pre thunks:
(define (call/cm/nontail key val proc)
  (current-mark-stack
   (make-mark-stack-frame (current-mark-stack)
                          #f
                          (intmap-set empty-hasheq key val)
                          #f))
  (proc)
  ;; If we're in an escape process, then `(current-mark-stack)` might not
  ;; match, and that's ok; it doesn't matter what we set the mark stack to
  ;; in that case, so we do something that's right for the non-escape case
  (when (current-mark-stack)
    (current-mark-stack (mark-stack-frame-prev (current-mark-stack)))))

(define (current-mark-chain)
  (get-current-mark-chain (current-mark-stack) (current-mark-splice) (current-metacontinuation)))

(define (mark-stack-to-marks mark-stack)
  (let loop ([mark-stack mark-stack])
    (cond
     [(not mark-stack) null]
     [(mark-stack-frame-flat mark-stack) => (lambda (l) l)]
     [else
      (let ([l (cons (mark-stack-frame-table mark-stack)
                     (loop (mark-stack-frame-prev mark-stack)))])
        (set-mark-stack-frame-flat! mark-stack l)
        l)])))

(define-record mark-chain-frame (tag splice? marks))

(define (get-current-mark-chain mark-stack mark-splice mc)
  (let ([hd (make-mark-chain-frame
             #f ; no tag
             #f ; not a splice
             (mark-stack-to-marks mark-stack))]
        [mid (and mark-splice
                  (make-mark-chain-frame
                   #f ; no tag
                   (mark-stack-tail-matches? mark-stack (mark-stack-frame-k mark-splice)) ; maybe splicing
                   (mark-stack-to-marks mark-splice)))]
        [tl (metacontinuation-marks mc)])
    (if mid
        (cons hd (cons mid tl))
        (cons hd tl))))

(define (mark-stack-tail-matches? mark-stack k)
  (and mark-stack
       (let ([prev (mark-stack-frame-prev mark-stack)])
         (or (and (not prev)
                  (eq? (mark-stack-frame-k mark-stack) k))
             (mark-stack-tail-matches? prev k)))))

(define (prune-mark-chain-prefix tag mark-chain)
  (cond
   [(eq? tag (mark-chain-frame-tag (elem+cache-strip (car mark-chain))))
    mark-chain]
   [else
    (prune-mark-chain-prefix tag (cdr mark-chain))]))

(define (prune-mark-chain-suffix tag mark-chain)
  (cond
   [(null? mark-chain) null]
   [(eq? tag (mark-chain-frame-tag (elem+cache-strip (car mark-chain))))
    null]
   [else
    (let ([rest-mark-chain (prune-mark-chain-suffix tag (cdr mark-chain))])
      (if (eq? rest-mark-chain (cdr mark-chain))
          mark-chain
          (cons (car mark-chain)
                rest-mark-chain)))]))

;; Used by `continuation-mark-set->list*` to determine when to splice
(define (splice-next? mark-chain)
  (and (pair? mark-chain)
       (pair? (cdr mark-chain))
       (mark-chain-frame-splice? (elem+cache-strip (cadr mark-chain)))))

;; Called when the curent continuation is `(current-empty-k)`,
;; merge anything in `(current-mark-splice)` into `(current-mark-stack)`
(define (merge-mark-splice!)
  (let ([mark-splice (current-mark-splice)])
    (when mark-splice
      (current-mark-stack (merge-mark-splice (current-mark-stack)
                                             mark-splice))
      (current-mark-splice #f))))

;; Merge immediate frame of `mark-splice` into immediate frame of
;; `mark-stack`, where `mark-stack` takes precedence. We expect that
;; each argument is a stack of length 0 or 1, since that's when
;; merging makes sense.
(define (merge-mark-splice mark-stack mark-splice)
  (cond
   [(not mark-stack) mark-splice]
   [(not mark-splice) mark-stack]
   [else
    (make-mark-stack-frame #f
                           (mark-stack-frame-k mark-stack)
                           (merge-mark-table (mark-stack-frame-table mark-stack)
                                             (mark-stack-frame-table mark-splice))
                           #f)]))

(define (merge-mark-table a b)
  (cond
   [(eq? empty-hasheq a) b]
   [(eq? empty-hasheq b) a]
   [else
    (let loop ([b b] [i (hash-iterate-first a)])
      (cond
       [(not i) b]
       [else (let-values ([(key val) (hash-iterate-key+value a i)])
               (loop (hash-set b key val)
                     (hash-iterate-next a i)))]))]))

;; If `mark-stack` ends with a frame that is conceptually
;; merged with one in `mark-splice`, then discard any keys
;; in `mark-splice` that are in the `mark-stack` frame.
;; Also, update `mark-splice` to use `empty-k`.
(define (prune-mark-splice mark-splice mark-stack empty-k)
  (cond
   [(not mark-splice) #f]
   [else
    (let loop ([mark-stack mark-stack])
      (cond
       [(not mark-stack) (make-mark-stack-frame #f
                                                empty-k
                                                (mark-stack-frame-table mark-splice)
                                                #f)]
       [else
        (let ([prev (mark-stack-frame-prev mark-stack)])
          (cond
           [(and (not prev) (eq? (mark-stack-frame-k mark-stack) empty-k))
            (make-mark-stack-frame #f
                                   empty-k
                                   (prune-mark-table (mark-stack-frame-table mark-stack)
                                                     (mark-stack-frame-table mark-splice))
                                   #f)]
           [else (loop prev)]))]))]))

(define (prune-mark-table a b)
  (cond
   [(eq? empty-hasheq a) b]
   [(eq? empty-hasheq b) a]
   [else
    (let loop ([b b] [i (hash-iterate-first a)])
      (cond
       [(not i) b]
       [else (loop (hash-remove b (hash-iterate-key a i))
                   (hash-iterate-next a i))]))]))

(define (mark-stack-starts-with? mark-stack k)
  (and mark-stack
       (eq? k (mark-stack-frame-k mark-stack))))

;; Drop any marks on the immediate frame --- used when
;; moving a frame across a metacontinuation boundary
(define (prune-immediate-frame mark-stack k)
  (cond
   [(mark-stack-starts-with? mark-stack k)
    (make-mark-stack-frame (mark-stack-frame-prev mark-stack)
                           (mark-stack-frame-k mark-stack)
                           empty-hasheq
                           #f)]
   [else mark-stack]))

(define (keep-immediate-frame mark-stack k empty-k)
  (cond
   [(mark-stack-starts-with? mark-stack k)
    (make-mark-stack-frame #f
                           empty-k
                           (mark-stack-frame-table mark-stack)
                           #f)]
   [else #f]))

(define (mark-stack-append a b)
  (cond
   [(not a) b]
   [(not b) a]
   [else
    (make-mark-stack-frame (mark-stack-append (mark-stack-frame-prev a) b)
                           (mark-stack-frame-k a)
                           (mark-stack-frame-table a)
                           #f)]))

;; ----------------------------------------
;; Continuation-mark caching

;; A `elem+cache` can replace a plain table in a "flat" variant of the
;; mark stack within a metacontinuation frame, or in a mark-stack
;; chain for a metacontinuation. The cache is a table that records
;; results found later in the list, which allows
;; `continuation-mark-set-first` to take amortized constant time.
(define-record elem+cache (elem cache))
(define (elem+cache-strip t) (if (elem+cache? t) (elem+cache-elem t) t))

(define/who call-with-immediate-continuation-mark
  (case-lambda
    [(key proc) (call-with-immediate-continuation-mark key proc #f)]
    [(key proc default-v)
     (check who (procedure-arity-includes/c 1) proc)
     (let-values ([(key wrapper) (extract-continuation-mark-key-and-wrapper 'call-with-immediate-continuation-mark key)])
       (cond
        [(not (current-mark-stack)) (|#%app| proc default-v)]
        [else
         (call/cc (lambda (k)
                    (when (eq? k (current-empty-k)) (merge-mark-splice!))
                    (if (eq? k (mark-stack-frame-k (current-mark-stack)))
                        (|#%app| proc (let ([v (intmap-ref (mark-stack-frame-table (current-mark-stack))
                                                           key
                                                           none)])
                                        (if (eq? v none)
                                            default-v
                                            (wrapper v))))
                        (|#%app| proc default-v))))]))]))

(define/who continuation-mark-set-first
  (case-lambda
    [(marks key) (continuation-mark-set-first marks key #f)]
    [(marks key none-v)
     (continuation-mark-set-first marks key none-v
                                  ;; Treat `break-enabled-key` and `parameterization-key`, specially
                                  ;; so that things like `current-break-parameterization` work without
                                  ;; referencing the root continuation prompt tag
                                  (if (or (eq? key break-enabled-key)
                                          (eq? key parameterization-key))
                                      the-root-continuation-prompt-tag
                                      the-default-continuation-prompt-tag))]
    [(marks key none-v prompt-tag)
     (check who continuation-mark-set? :or-false marks)
     (check who continuation-prompt-tag? prompt-tag)
     (maybe-future-barricade prompt-tag)
     (let ([prompt-tag (strip-impersonator prompt-tag)])
       (let-values ([(key wrapper) (extract-continuation-mark-key-and-wrapper 'continuation-mark-set-first key)])
         (let ([v (marks-search (or (and marks
                                         (continuation-mark-set-mark-chain marks))
                                    (current-mark-chain))
                                key
                                #t ; at-outer?
                                prompt-tag)])
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
            [else (wrapper v)]))))]))

;; To make `continuation-mark-set-first` constant-time, if we traverse
;; N elements to get an answer, then cache the answer at N/2 elements.
(define (marks-search elems key at-outer? prompt-tag)
  (let loop ([elems elems] [elems/cache-pos elems] [cache-step? #f] [depth 0])
    (cond
     [(or (null? elems)
          (and at-outer?
               (eq? (mark-chain-frame-tag (elem+cache-strip (car elems))) prompt-tag)))
      ;; Not found
      (cache-result! elems elems/cache-pos depth key none at-outer? prompt-tag)
      none]
     [else
      (let ([t (car elems)]
            [check-elem
             (lambda (t)
               (let ([v (if at-outer?
                            ;; Search within the metacontinuation frame:
                            (let ([marks (mark-chain-frame-marks t)])
                              (marks-search marks key #f #f))
                            ;; We're looking at just one frame:
                            (intmap-ref t key none))])
                 (cond
                  [(eq? v none)
                   ;; Not found at this point; keep looking
                   (loop (cdr elems)
                         (if cache-step? (cdr elems/cache-pos) elems/cache-pos)
                         (not cache-step?)
                         (fx+ 1 depth))]
                  [else
                   ;; Found it
                   (cache-result! elems elems/cache-pos depth key v at-outer? prompt-tag)
                   v])))])
        (cond
         [(elem+cache? t)
          (let ([v (intmap-ref (elem+cache-cache t) key none2)])
            (cond
             [(eq? v none2)
              ;; No mapping in cache, so try the element and continue:
              (check-elem (elem+cache-elem t))]
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
                  (check-elem (elem+cache-elem t))]
                 [(eq? v none)
                  ;; The cache records that it's not in the rest:
                  (cache-result! elems elems/cache-pos depth key none at-outer? prompt-tag)
                  none]
                 [else
                  ;; The cache provides a value from the rest:
                  (cache-result! elems elems/cache-pos depth key v at-outer? prompt-tag)
                  v]))]))]
         [else
          ;; Try the element:
          (check-elem t)]))])))

;; To make `continuation-mark-set-first` constant-time, cache
;; a key--value mapping at a point that's half-way in
(define (cache-result! marks marks/cache-pos depth key v at-outer? prompt-tag)
  (unless (< depth 16)
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
                                                   (let ([old (intmap-ref (elem+cache-cache new-t) key none2)])
                                                     (intmap-set (if (eq? old none2) empty-hasheq old) prompt-tag v))
                                                   v))))))

(define/who continuation-mark-set->list
  (case-lambda
    [(marks key) (continuation-mark-set->list marks key the-default-continuation-prompt-tag)]
    [(marks key prompt-tag)
     (check who continuation-mark-set? :or-false marks)
     (check who continuation-prompt-tag? prompt-tag)
     (maybe-future-barricade prompt-tag)
     (let ([prompt-tag (strip-impersonator prompt-tag)])
       (let-values ([(key wrapper) (extract-continuation-mark-key-and-wrapper 'continuation-mark-set->list key)])
         (let chain-loop ([mark-chain (or (and marks
                                               (continuation-mark-set-mark-chain marks))
                                          (current-mark-chain))])
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
                     (let* ([v (intmap-ref (elem+cache-strip (car marks)) key none)])
                       (if (eq? v none)
                           (loop (cdr marks))
                           (cons (wrapper v) (loop (cdr marks)))))]))]))]))))]))

(define/who continuation-mark-set->list*
  (case-lambda
    [(marks keys) (continuation-mark-set->list* marks keys the-default-continuation-prompt-tag #f)]
    [(marks keys prompt-tag) (continuation-mark-set->list* marks keys prompt-tag #f)]
    [(marks keys prompt-tag none-v)
     (check who continuation-mark-set? :or-false marks)
     (check who list? keys)
     (check who continuation-prompt-tag? prompt-tag)
     (maybe-future-barricade prompt-tag)
     (let ([prompt-tag (strip-impersonator prompt-tag)])
       (let-values ([(all-keys all-wrappers)
                     (map/2-values (lambda (k)
                                     (extract-continuation-mark-key-and-wrapper 'continuation-mark-set->list* k))
                                   keys)])
         (let* ([n (length all-keys)]
                [tmp (make-vector n)])
           (let chain-loop ([mark-chain (or (and marks
                                                 (continuation-mark-set-mark-chain marks))
                                            (current-mark-chain))])
             (cond
              [(null? mark-chain)
               null]
              [else
               (let* ([mcf (elem+cache-strip (car mark-chain))])
                 (cond
                  [(eq? (mark-chain-frame-tag mcf) prompt-tag)
                   null]
                  [else
                   (let loop ([marks (let ([marks (mark-chain-frame-marks mcf)])
                                       (if (splice-next? mark-chain)
                                           ;; handle splicing (created by applying a composable
                                           ;; continuation to a context that had marks already)
                                           (append marks
                                                   (mark-chain-frame-marks (elem+cache-strip (cadr mark-chain))))
                                           marks))])
                     (cond
                      [(null? marks)
                       (chain-loop (if (splice-next? mark-chain)
                                       (cddr mark-chain)
                                       (cdr mark-chain)))]
                      [else
                       (let ([t (elem+cache-strip (car marks))])
                         (let key-loop ([keys all-keys] [wrappers all-wrappers] [i 0] [found? #f])
                           (cond
                            [(null? keys)
                             (if found?
                                 (let ([vec (vector-copy tmp)])
                                   (cons vec (loop (cdr marks))))
                                 (loop (cdr marks)))]
                            [else
                             (let ([v (intmap-ref t (car keys) none)])
                               (cond
                                [(eq? v none)
                                 (vector-set! tmp i none-v)
                                 (key-loop (cdr keys) (cdr wrappers) (add1 i) found?)]
                                [else
                                 (vector-set! tmp i ((car wrappers) v))
                                 (key-loop (cdr keys) (cdr wrappers) (add1 i) #t)]))])))]))]))])))))]))

(define/who (continuation-mark-set->context marks)
  (check who continuation-mark-set? marks)
  (traces->context (continuation-mark-set-traces marks)))

(define/who current-continuation-marks
  (case-lambda
    [() (current-continuation-marks the-default-continuation-prompt-tag)]
    [(tag)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (call/cc
      (lambda (k)
        (make-continuation-mark-set (prune-mark-chain-suffix (strip-impersonator tag) (current-mark-chain))
                                    (cons (continuation->trace k)
                                          (get-metacontinuation-traces (current-metacontinuation))))))]))

;; Wrapped a threads layer to handle thread arguments:
(define/who continuation-marks
  (case-lambda
    [(k) (continuation-marks k (default-continuation-prompt-tag))]
    [(k tag)
     ;; If `k` is a procedure, we assume that it's an engine
     (check who (lambda (p) (or (not p)
                                (continuation? p)
                                (and (#%procedure? p) (procedure-arity-includes? p 0))))
            :contract "(or/c continuation? engine-procedure? #f)"
            k)
     (check who continuation-prompt-tag? tag)
     (maybe-future-barricade tag)
     (let ([tag (strip-impersonator tag)])
       (cond
        [(#%procedure? k)
         (let ([mc (saved-metacontinuation-mc (k))])
           (make-continuation-mark-set
            (prune-mark-chain-suffix
             tag
             (get-current-mark-chain #f #f mc))
            (get-metacontinuation-traces mc)))]
        [(full-continuation? k)
         (make-continuation-mark-set
          (prune-mark-chain-suffix
           tag
           (get-current-mark-chain (full-continuation-mark-stack k)
                                   (full-continuation-mark-splice k)
                                   (full-continuation-mc k)))
          (cons (continuation->trace (full-continuation-k k))
                (get-metacontinuation-traces (full-continuation-mc k))))]
        [(escape-continuation? k)
         (unless (continuation-prompt-available? (escape-continuation-tag k))
           (raise-continuation-error '|continuation application|
                                     "escape continuation not in the current continuation"))
         (make-continuation-mark-set
          (prune-mark-chain-suffix
           tag
           (prune-mark-chain-prefix (escape-continuation-tag k) (current-mark-chain)))
          null)]
        [else
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
   [() (make-continuation-mark-key (gensym))]
   [(name) (create-continuation-mark-key name)]))

(define (continuation-mark-key? v)
  (or (authentic-continuation-mark-key? v)
      (and (impersonator? v)
           (authentic-continuation-mark-key? (impersonator-val v)))))

;; Like `intmap-set`, but handles continuation-mark-key impersonators
(define (intmap-set/cm-key ht k v)
  (cond
   [(and (impersonator? k)
         (authentic-continuation-mark-key? (impersonator-val k)))
    (let loop ([k k] [v v])
      (cond
       [(or (continuation-mark-key-impersonator? k)
            (continuation-mark-key-chaperone? k))
        (let ([new-v (|#%app|
                      (if (continuation-mark-key-impersonator? k)
                          (continuation-mark-key-impersonator-set k)
                          (continuation-mark-key-chaperone-set k))
                      v)])
          (unless (or (continuation-mark-key-impersonator? k)
                      (chaperone-of? new-v v))
            (raise-chaperone-error 'with-continuation-mark "value" v new-v))
          (loop (impersonator-next k) new-v))]
       [(impersonator? k)
        (loop (impersonator-next k) v)]
       [else
        (intmap-set ht k v)]))]
   [else (intmap-set ht k v)]))

;; Extracts the key and converts the wrapper functions into
;; a single function:
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
    (values k (lambda (v) v))]))

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
  (call-with-values (lambda () (apply wrapper args))
    (lambda new-args
      (unless (= (length args) (length new-args))
        (raise-result-arity-error at-when (length args) new-args))
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
  (assert-in-uninterrupted)
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
  (assert-in-uninterrupted)
  (cond
   [(continuation-prompt-tag-impersonator-or-chaperone? tag)
    (let ([cc-impersonate (continuation-prompt-tag-procs-cc-impersonate
                           (continuation-prompt-tag-impersonator-or-chaperone-procs tag))]
          [chaperone? (continuation-prompt-tag-chaperone? tag)])
      (let ([cc-guard (wrap-cc-guard-for-impersonator (impersonator-next tag) cc-guard)])
        (let ([new-cc-guard (call-with-continuation-barrier
                             (lambda ()
                               (end-uninterrupted 'cc-guard)
                               (begin0
                                (|#%app| cc-impersonate cc-guard)
                                (start-uninterrupted 'cc-guard))))])
          (when chaperone?
            (unless (chaperone-of? new-cc-guard cc-guard)
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

(define-record winder (depth k pre post mark-stack))

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
  ((call/cc
    (lambda (k)
      (let* ([winders (current-winders)]
             [winder (make-winder (if (null? winders)
                                      0
                                      (fx+ 1 (winder-depth (car winders))))
                                  k
                                  pre
                                  post
                                  (current-mark-stack))])
        (start-uninterrupted 'dw)
        (begin
          (call-winder-thunk 'dw-pre pre)
          (current-winders (cons winder winders))
          (end-uninterrupted/call-hook 'dw-body)
          (call-with-values thunk
            (lambda args
              (start-uninterrupted 'dw-body)
              (current-winders winders)
              (call-winder-thunk 'dw-post post)
              (end-uninterrupted/call-hook 'dw)
              (lambda () (apply values args))))))))))

(define (call-winder-thunk who thunk)
  (call/cm/nontail
   break-enabled-key (make-thread-cell #f #t)
   (lambda ()
     (end-uninterrupted who)
     (thunk)
     (start-uninterrupted who))))

(define (wind-in winders k)
  (do-wind 'dw-pre winders winder-pre k))

(define (wind-out k)
  (do-wind 'dw-post (current-winders) winder-post k))

(define (do-wind who winders winder-thunk k)
  (assert-in-uninterrupted)
  (let ([winder (car winders)]
        [winders (cdr winders)])
    (current-winders winders)
    (current-mark-stack (winder-mark-stack winder))
    (let ([thunk (winder-thunk winder)])
      ((winder-k winder)
       (lambda ()
         (call-winder-thunk who thunk)
         (k))))))

(define (wind-to dest-winders done-k retry-k)
  (let ([starting-metacontinuation (current-metacontinuation)])
    (let loop ([rev-dest-winders-head '()]
               [dest-winders-tail dest-winders])
      (cond
       [(and retry-k
             (not (eq? starting-metacontinuation (current-metacontinuation))))
        (retry-k)]
       [else
        (let ([winders (current-winders)])
          (cond
           [(same-winders? winders dest-winders-tail)
            ;; No winders to leave
            (cond
             [(null? rev-dest-winders-head)
              (done-k)]
             [else
              ;; Go in one winder
              (let ([new-winders (cons (car rev-dest-winders-head) winders)]
                    [rev-dest-winders-head (cdr rev-dest-winders-head)])
                (wind-in new-winders
                         (lambda ()
                           (current-winders new-winders)
                           (loop rev-dest-winders-head new-winders))))])]
           [(or (null? dest-winders-tail)
                (and (pair? winders)
                     (> (winder-depth (car winders)) (winder-depth (car dest-winders-tail)))))
            ;; Go out by one winder
            (wind-out (lambda () (loop rev-dest-winders-head dest-winders-tail)))]
           [else
            ;; Move a dest winder from tail to head:
            (loop (cons (car dest-winders-tail) rev-dest-winders-head)
                  (cdr dest-winders-tail))]))]))))

(define (same-winders? winders dest-winders-tail)
  (or (and (null? winders)
           (null? dest-winders-tail))
      (and (pair? winders)
           (pair? dest-winders-tail)
           (eq? (car winders) (car dest-winders-tail)))))

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
  (call/cm
   break-enabled-key (make-thread-cell #f #t)
   thunk))

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

;; ----------------------------------------
;; Metacontinuation swapping for engines

(define-record saved-metacontinuation (mc system-winders exn-state))

(define empty-metacontinuation (make-saved-metacontinuation '() '() (create-exception-state)))

;; Similar to `call-with-current-continuation` plus
;; applying an old continuation, but does not run winders;
;; this operation makes sense for thread or engine context
;; switches
(define (swap-metacontinuation saved proc)
  (cond
   [(current-system-wind-start-k)
    => (lambda (k) (swap-metacontinuation-with-system-wind saved proc k))]
   [else
    (call-in-empty-metacontinuation-frame
     #f
     fail-abort-to-delimit-continuation
     #f ; don't try to shift continuation marks
     (lambda ()
       (let ([now-saved (make-saved-metacontinuation
                         (current-metacontinuation)
                         (#%$current-winders)
                         (current-exception-state))])
         (current-metacontinuation (saved-metacontinuation-mc saved))
         (#%$current-winders (saved-metacontinuation-system-winders saved))
         (current-exception-state (saved-metacontinuation-exn-state saved))
         (current-empty-k #f)
         (set! saved #f) ; break link for space safety
         (proc now-saved))))]))

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
               (apply values args)))))
       (lambda () (current-system-wind-start-k #f)))))))

(define (swap-metacontinuation-with-system-wind saved proc start-k)
  (current-system-wind-start-k #f)
  (call/cc
   (lambda (system-wind-k) ; continuation with system `dynamic-wind` behavior
     ;; escape to starting point, running winders, before
     ;; capturing the rest of the metacontinuation:
     (start-k (lambda ()
                (let ([prefix (swap-metacontinuation saved proc)])
                  (current-system-wind-start-k start-k)
                  (system-wind-k prefix)))))))

(define (assert-not-in-system-wind)
  (CHECK-uninterrupted
   (when (current-system-wind-start-k)
     (internal-error 'not-in-system-wind "assertion failed"))))
