#lang scheme/base
(require scheme/foreign
         "../common/utils.rkt"
         "../common/queue.rkt")
(unsafe!)

(provide call-with-frozen-stack
         frozen-stack-run-some
         constrained-reply)

(define-mz scheme_with_stack_freeze (_fun (_fun _scheme -> _int) _scheme -> _int))
(define-mz scheme_frozen_run_some (_fun (_fun _scheme -> _int) _scheme _int -> _int))
(define-mz scheme_is_in_frozen_stack (_fun -> _int))

(define (do-apply p) 
  ;; Continuation prompt ensures that errors do not escape
  ;; (and escapes are not supported by the frozen-stack implementation)
  (call-with-continuation-prompt p)
  1)

(define (call-with-frozen-stack thunk)
  (void (scheme_with_stack_freeze do-apply thunk)))

(define (frozen-stack-run-some thunk msecs)
  (positive? (scheme_frozen_run_some do-apply thunk msecs)))

;; FIXME: this loop needs to give up on the thunk
;;  if it takes too long to return; as long as we're in the
;;  loop, no other threads/eventspaces can run
(define (constrained-reply es thunk default)
  (if (eq? (current-thread) (eventspace-handler-thread es))
      (let ([done? #f]
            [result default])
        (frozen-stack-run-some (lambda () (set! result (thunk)))
                               200)
        (let loop ()
          (frozen-stack-run-some (lambda () (set! done? #t)) 200)
          (unless done? (loop)))
        result)
      (begin
        (eprintf "WARNING: internal error: wrong eventspace for constrained event handling\n")
        (eprintf "~s\n" (continuation-mark-set->context (current-continuation-marks)))
        default)))


