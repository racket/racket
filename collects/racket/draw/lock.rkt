#lang racket/base
(require (for-syntax racket/base)
         ffi/unsafe/atomic)

(provide (protect-out as-entry
                      as-exit
                      entry-point))

;; We need atomic mode for a couple of reasons:
;;
;; * We may need to bracket some (trusted) operations so that the
;;   queue thread doesn't poll for events during the operation.
;;
;; * The scheme/gui classes have internal-consistency requirements.
;;   When the user creates an object or calls a method, or when the
;;   system invokes a callback, many steps may be required to
;;   initialize or reset fields to maintain invariants. To ensure that
;;   other threads do not call methods during a time when invariants
;;   do not hold, we force all of the following code to be executed in
;;   a single threaded manner, and we temporarily disable breaks.
;;
;; Atomic mode is implemented with a single monitor: all entry points
;; into the code use `entry-point' or `as-entry', and all points with
;; this code that call back out to user code uses `as-exit'.
;;
;; If an exception is raised within an `enter'ed area, control is
;; moved back outside by the exception handler, and then the exception
;; is re-raised. The user can't tell that the exception was caught an
;; re-raised. But without the catch-and-reraise, the user's exception
;; handler might try to use GUI elements from a different thread, or
;; other such things, leading to deadlock.

(define monitor-owner #f)

;; An exception may be constructed while we're entered:
(define entered-err-string-handler
  (lambda (s n)
    (as-exit
     (lambda ()
       ((error-value->string-handler) s n)))))

(define old-paramz #f)
(define old-break-paramz #f)

(define exited-key (gensym 'as-exit))
(define lock-tag (make-continuation-prompt-tag 'lock))

(define (as-entry f)
  (cond
   [(eq? monitor-owner (current-thread))
    (f)]
   [else
    (with-continuation-mark 
        exited-key 
        #f
      (call-with-continuation-prompt
       (lambda ()
         (dynamic-wind
             (lambda () 
               (start-atomic)  
               (set! monitor-owner (current-thread)))
             (lambda () 
               (set! old-paramz (current-parameterization))
               (set! old-break-paramz (current-break-parameterization))
               (parameterize ([error-value->string-handler entered-err-string-handler])
                 (parameterize-break 
                  #f
                  (call-with-exception-handler
                   (lambda (exn)
                     ;; Get out of atomic region before letting
                     ;;  an exception handler work
                     (if (continuation-mark-set-first #f exited-key)
                         exn ; defer to previous exn handler
                         (abort-current-continuation
                          lock-tag
                          (lambda () (raise exn)))))
                   f))))
             (lambda ()
               (set! monitor-owner #f)
               (set! old-paramz #f)
               (set! old-break-paramz #f)
               (end-atomic))))
       lock-tag
       (lambda (t) (t))))]))

(define (as-exit f)
  ;; (unless (eq? monitor-owner (current-thread)) (error 'monitor-exit "not in monitored area for ~e" f))
  (let ([paramz old-paramz]
        [break-paramz old-break-paramz])
    (with-continuation-mark 
        exited-key 
        #t ; disables special exception handling
      (call-with-parameterization
       paramz
       (lambda ()
         (call-with-break-parameterization
          break-paramz
          (lambda ()
            (dynamic-wind
                (lambda ()		
                  (set! monitor-owner #f)	 
                  (end-atomic))
                f
                (lambda ()
                  (set! old-paramz paramz)
                  (set! old-break-paramz break-paramz)
                  (start-atomic)
                  (set! monitor-owner (current-thread)))))))))))

(define-syntax entry-point 
  (lambda (stx)
    (syntax-case stx (lambda #%plain-lambda case-lambda)
      [(_ (lambda args body1 body ...))
       (syntax (lambda args (as-entry (lambda () body1 body ...))))]
      [(_ (#%plain-lambda args body1 body ...))
       (syntax (#%plain-lambda args (as-entry (lambda () body1 body ...))))]
      [(_ (case-lambda [vars body1 body ...] ...))
       (syntax (case-lambda 
                [vars (as-entry (lambda () body1 body ...))]
                ...))])))
