#lang racket/base
(require "../../lock.rkt"
         "queue.rkt")

(provide 
 (protect-out do-request-flush-delay
              do-cancel-flush-delay))

;; Auto-cancel schedules a cancel of a request flush
;; on event boundaries. It makes sense if you don't
;; trust a program to un-delay important refreshes,
;; but auto-cancel is currently disabled because
;; bad refresh-delay effects are confined to the enclosing
;; window on all platforms.
(define AUTO-CANCEL-DELAY? #f)

(define (do-request-flush-delay win disable enable)
  (atomically
   (let ([req (box win)])
     (and 
      (disable win)
      (begin
        (when AUTO-CANCEL-DELAY?
          (add-event-boundary-sometimes-callback! 
           req
           (lambda (v) 
             ;; in atomic mode
             (when (unbox req) 
               (set-box! req #f)
               (enable win)))))
        req)))))

(define (do-cancel-flush-delay req enable)
  (atomically
   (let ([win (unbox req)])
     (when win
       (set-box! req #f)
       (enable win)
       (when AUTO-CANCEL-DELAY?
         (remove-event-boundary-callback! req))))))
