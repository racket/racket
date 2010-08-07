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
  (get-ffi-obj 'scheme_set_on_atomic_timeout #f (_fun (_fun -> _void) -> _pointer)))
(define scheme_restore_on_atomic_timeout 
  (get-ffi-obj 'scheme_set_on_atomic_timeout #f (_fun _pointer -> _pointer)))

(define freezer-box (make-parameter #f))
(define freeze-tag (make-continuation-prompt-tag))

;; Runs `thunk' atomically, but cooperates with 
;; `try-atomic' to continue a frozen
;; computation in non-atomic mode.
(define (call-as-nonatomic-retry-point thunk)
  (let ([b (box null)])
    (parameterize ([freezer-box b])
      ;; In atomic mode:
      (call-as-atomic thunk))
    ;; Out of atomic mode:
    (let ([l (unbox b)])
      (for ([k (in-list (reverse l))])
        (call-with-continuation-prompt ; to catch aborts
         (lambda ()
           (call-with-continuation-prompt
            k
            freeze-tag)))))
    (void)))

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
      (let* ([prev #f]
             [ready? #f]
             [handler (lambda ()
                        (when (and ready? (should-give-up?))
                          (scheme_call_with_composable_no_dws
                           (lambda (proc)
                             (set-box! b (cons proc (unbox b)))
                             (scheme_restore_on_atomic_timeout prev)
                             (scheme_abort_continuation_no_dws
                              freeze-tag
                              (lambda () default)))
                           freeze-tag)
                          (void)))])
        (hash-set! saved-ptrs handler #t)
        (begin0
         (parameterize ([freezer-box #f])
           (dynamic-wind
            void
            (lambda ()
              (call-with-continuation-prompt ; for composable continuation
               (lambda ()
                 (call-with-continuation-prompt ; to catch aborts
                  (lambda ()
                    (set! prev (scheme_set_on_atomic_timeout handler))
                    (when prev (log-error "uh oh"))
                    (set! ready? #t)
                    (thunk))))
               freeze-tag))
            (lambda ()
              (scheme_restore_on_atomic_timeout prev))))
         (hash-remove! saved-ptrs handler)))])))

