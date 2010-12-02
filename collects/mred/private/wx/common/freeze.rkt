#lang racket/base
(require ffi/unsafe/try-atomic
         "queue.rkt")

(provide 
 call-as-nonatomic-retry-point
 (protect-out constrained-reply))

(define (internal-error str)
  (log-error
   (apply string-append
          (format "internal error: ~a" str)
          (append
           (for/list ([c (continuation-mark-set->context (current-continuation-marks))])
             (let ([name (car c)]
                   [loc (cdr c)])
               (cond
                [loc
                 (string-append
                  "\n"
                  (cond 
                   [(srcloc-line loc)
                    (format "~a:~a:~a" 
                            (srcloc-source loc)
                            (srcloc-line loc)
                            (srcloc-column loc))]
                   [else
                    (format "~a::~a" 
                            (srcloc-source loc)
                            (srcloc-position loc))])
                  (if name (format " ~a" name) ""))]
                [else (format "\n ~a" name)])))
           '("\n")))))

;; FIXME: waiting 200msec is not a good enough rule.
(define (constrained-reply es thunk default 
                           #:fail-result [fail-result default])
  (cond
   [(not (can-try-atomic?))
    ;; Ideally, this would count as an error that we can fix. It seems that we
    ;; don't always have enough control to use the right eventspace with a
    ;; retry point, though, so just bail out with the default.
    #;(internal-error (format "constrained-reply not within an unfreeze point for ~s" thunk))
    fail-result]
   [(not (eq? (current-thread) (eventspace-handler-thread es)))
    (internal-error "wrong eventspace for constrained event handling\n")
    fail-result]
   [else
    (try-atomic thunk default)]))
