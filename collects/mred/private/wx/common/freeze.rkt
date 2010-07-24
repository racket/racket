#lang scheme/base
(require scheme/foreign
         racket/draw/hold
         "utils.rkt"
         "queue.rkt"
         "../../lock.rkt")
(unsafe!)

(provide call-as-unfreeze-point
         constrained-reply)

(define-mz scheme_abort_continuation_no_dws (_fun _scheme _scheme -> _scheme))
(define-mz scheme_call_with_composable_no_dws (_fun _scheme _scheme -> _scheme))
(define-mz scheme_set_on_atomic_timeout (_fun (_fun -> _void) -> _pointer))
(define-mz scheme_restore_on_atomic_timeout (_fun _pointer -> _pointer)
  #:c-id scheme_set_on_atomic_timeout)

(define freezer-box (make-parameter null))
(define freeze-tag (make-continuation-prompt-tag))

;; Runs `thunk' atomically, but cooperates with 
;; `constrained-reply' to continue a frozen
;; computation in non-atomic mode.
(define (call-as-unfreeze-point thunk)
  (let ([b (box null)])
    (parameterize ([freezer-box b])
      ;; In atomic mode:
      (as-entry (lambda () (thunk)))
      ;; Out of atomic mode:
      (let ([l (unbox b)])
        (for ([k (in-list (reverse l))])
          (call-with-continuation-prompt ; to catch aborts
           (lambda ()
             (call-with-continuation-prompt
              k
              freeze-tag)))))
      (void))))

;; FIXME: waiting 200msec is not a good enough rule.
(define (constrained-reply es thunk default [should-give-up?
                                             (let ([now (current-inexact-milliseconds)])
                                               (lambda ()
                                                 ((current-inexact-milliseconds) . > . (+ now 200))))])
  (let ([b (freezer-box)])
    (unless b
      (log-error "internal error: constrained-reply not within an unfreeze point"))
    (if (eq? (current-thread) (eventspace-handler-thread es))
        (if (pair? b)
            ;; already suspended, so push this work completely:
            (set-box! b (cons thunk (unbox b)))
            ;; try to do some work:
            (let* ([prev #f]
                   [ready? #f]
                   [handler (lambda ()
                              (when (and ready? (should-give-up?))
                                (scheme_call_with_composable_no_dws
                                 (lambda (proc)
                                   (set-box! (freezer-box) (cons proc (freezer-box)))
                                   (scheme_restore_on_atomic_timeout prev)
                                   (scheme_abort_continuation_no_dws
                                    freeze-tag
                                    (lambda () default)))
                                 freeze-tag)
                                (void)))]
                   [old (scheme_set_on_atomic_timeout handler)])
              (with-holding 
               handler
               (call-with-continuation-prompt ; to catch aborts
                (lambda ()
                  (call-with-continuation-prompt ; for composable continuation
                   (lambda ()
                     (set! prev old)
                     (set! ready? #t)
                     (begin0
                      (parameterize ([freezer-box #f])
                        (thunk))
                      (scheme_restore_on_atomic_timeout prev)))
                   freeze-tag))))))
        (begin
          (log-error "internal error: wrong eventspace for constrained event handling\n")
          default))))
