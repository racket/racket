#lang racket/base
(require ffi/unsafe
         ffi/unsafe/atomic
         (only-in '#%unsafe
                  unsafe-abort-current-continuation/no-wind
                  unsafe-call-with-composable-continuation/no-wind
                  unsafe-set-on-atomic-timeout!))

(provide call-as-nonatomic-retry-point
         can-try-atomic?
         try-atomic)

(define freezer-tag (make-continuation-prompt-tag))
(define freezer-box-key (gensym))
(define in-try-atomic-key (gensym))
(define freeze-tag (make-continuation-prompt-tag))
(define force-timeout (make-parameter #f))

(define (freezer-box)
  (and (continuation-prompt-available? freezer-tag)
       (continuation-mark-set-first #f freezer-box-key #f freezer-tag)))

(define (in-try-atomic?)
  (and (continuation-prompt-available? freezer-tag)
       (continuation-mark-set-first #f in-try-atomic-key #f freezer-tag)))

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
     (call-with-continuation-prompt
      (lambda ()
        (with-continuation-mark freezer-box-key b
          ;; In atomic mode (but not using call-as-atomic, because we
          ;; don't want to change the exception handler, etc.)
          (begin
            (start-atomic)
            (begin0 
             (thunk)
             (end-atomic)))))
      freezer-tag) ; so we can look past any default prompts for `freezer-box-key'
     ;; Retries out of atomic mode:
     (let ([l (unbox b)])
       (for ([k (in-list (reverse l))])
         (call-with-continuation-prompt
          k
          freeze-tag))))))

(define (can-try-atomic?) (and (freezer-box) (not (in-try-atomic?))))

(define (try-atomic thunk default
                    #:should-give-up? [should-give-up?
                                       (let ([now (current-inexact-monotonic-milliseconds)])
                                         (lambda ()
                                           ((current-inexact-monotonic-milliseconds) . > . (+ now 200))))]
                    #:keep-in-order? [keep-in-order? #t])
  (let ([b (freezer-box)])
    (cond
     [(not b) (error 'try-atomic "not inside a nonatomic retry point")]
     [(in-try-atomic?) (error 'try-atomic "already trying atomic")]
     [(and (pair? (unbox b)) keep-in-order?)
      ;; gave up on previous try, so give up now immediately:
      (set-box! b (cons thunk (unbox b)))
      default]
     [else
      ;; try to do some work:
      (let* ([ready? #f]
             [done? #f]
             [handler (lambda (must-give-up?)
                        (when (and ready? 
                                   (not done?)
                                   (or must-give-up?
                                       (force-timeout)
                                       (should-give-up?)))
                          (unsafe-call-with-composable-continuation/no-wind
                           (lambda (proc)
                             (set-box! b (cons proc (unbox b)))
                             (unsafe-set-on-atomic-timeout! #f)
                             (unsafe-abort-current-continuation/no-wind
                              freeze-tag
                              (lambda () default)))
                           freeze-tag)
                          (void)))])
        (with-continuation-mark in-try-atomic-key #t
          (let/ec esc ;; esc + dynamic-wind prevents escape via alternate prompt tags
            (dynamic-wind
             void
             (lambda ()
               (call-with-continuation-prompt ; for composable continuation
                (lambda ()
                  (call-with-continuation-prompt ; to catch aborts
                   (lambda ()
                     (when (unsafe-set-on-atomic-timeout! handler) ; also records current atomicity level
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
               (unsafe-set-on-atomic-timeout! #f)
               (unless done? (esc (void))))))))])))

