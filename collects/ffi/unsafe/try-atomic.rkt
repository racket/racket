#lang scheme/base
(require ffi/unsafe
         ffi/unsafe/atomic)

(provide call-as-nonatomic-retry-point
         can-try-atomic?
         try-atomic)

(define scheme_abort_continuation_no_dws
  (get-ffi-obj 'scheme_abort_continuation_no_dws #f (_fun _scheme _scheme -> _scheme)))
(define scheme_call_with_composable_no_dws
  (get-ffi-obj 'scheme_call_with_composable_no_dws #f (_fun _scheme _scheme -> _scheme)))
(define scheme_set_on_atomic_timeout
  (get-ffi-obj 'scheme_set_on_atomic_timeout #f (_fun (_fun _int -> _void) -> _pointer)))
(define scheme_restore_on_atomic_timeout 
  (get-ffi-obj 'scheme_set_on_atomic_timeout #f (_fun _pointer -> _pointer)))

(define freezer-box (make-parameter #f))
(define freeze-tag (make-continuation-prompt-tag))
(define force-timeout (make-parameter #f))

;; Runs `thunk' atomically, but cooperates with 
;; `try-atomic' to continue a frozen
;; computation in non-atomic mode.
(define (call-as-nonatomic-retry-point thunk)
  (when (freezer-box) 
    ;; Try to avoid a nested try-atomic:
    (parameterize ([force-timeout #t])
      (sleep)))
  (let ([b (box (if (freezer-box)
                    ;; Still in try-atomic; we'll have to complete
                    ;;  everything atomically, and starting with
                    ;;  a non-empty list means that we won't bother
                    ;;  capturing continuations.
                    (list void)
                    ;; Start with an empty list of things to finish:
                    null))])
    (begin0
     (parameterize ([freezer-box b])
       ;; In atomic mode (but not using call-as-atomic, because we
       ;; don't want to change the exception handler, etc.)
       (start-atomic)
       (begin0 
        (thunk)
        (end-atomic)))
     ;; Retries out of atomic mode:
     (let ([l (unbox b)])
       (for ([k (in-list (reverse l))])
         (call-with-continuation-prompt
          k
          freeze-tag))))))

(define (can-try-atomic?) (and (freezer-box) #t))

;; prevent GC of handler while it's installed:
(define saved-ptrs (make-hash))

(define (try-atomic thunk default 
                    #:should-give-up? [should-give-up?
                                       (let ([now (current-inexact-milliseconds)])
                                         (lambda ()
                                           ((current-inexact-milliseconds) . > . (+ now 200))))]
                    #:keep-in-order? [keep-in-order? #t])
  (let ([b (freezer-box)])
    (cond
     [(not b) (error 'try-atomic "not inside a nonatomic retry point")]
     [(and (pair? (unbox b)) keep-in-order?)
      ;; gave up on previous try, so give up now immediately:
      (set-box! b (cons thunk (unbox b)))
      default]
     [else
      ;; try to do some work:
      (let* ([ready? #f]
             [handler (lambda (must-give-up)
                        (when (and ready? 
                                   (or (positive? must-give-up)
                                       (force-timeout)
                                       (should-give-up?)))
                          (scheme_call_with_composable_no_dws
                           (lambda (proc)
                             (set-box! b (cons proc (unbox b)))
                             (scheme_restore_on_atomic_timeout #f)
                             (scheme_abort_continuation_no_dws
                              freeze-tag
                              (lambda () default)))
                           freeze-tag)
                          (void)))]
             [done? #f])
        (hash-set! saved-ptrs handler #t)
        (begin
          (let/ec esc ;; esc + dynamic-wind prevents escape via alternate prompt tags
            (dynamic-wind
             void
             (lambda ()
               (call-with-continuation-prompt ; for composable continuation
                (lambda ()
                  (call-with-continuation-prompt ; to catch aborts
                   (lambda ()
                     (when (scheme_set_on_atomic_timeout handler)
                       (error 'try-atomic "nested atomic timeout"))
                     (set! ready? #t)
                     (begin0
                      (thunk)
                      (set! done? #t)))
                   (default-continuation-prompt-tag)
                   (lambda args
                     (set! done? #t)
                     ;; re-abort later...
                     (set-box! b (cons (lambda ()
                                         (apply abort-current-continuation 
                                                (default-continuation-prompt-tag)
                                                args))
                                       (unbox b))))))
                freeze-tag
                (lambda (thunk)
                  (set! done? #t)
                  (thunk))))
             (lambda ()
               (hash-remove! saved-ptrs handler)
               (scheme_restore_on_atomic_timeout #f)
               (unless done? (esc (void))))))))])))

