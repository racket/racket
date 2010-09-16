#lang racket/base
(require "../../lock.rkt"
         "queue.rkt")

(provide do-request-flush-delay
         do-cancel-flush-delay)

(define (do-request-flush-delay win disable enable)
  (atomically
   (let ([req (box win)])
     (and 
      (disable win)
      (begin
        (add-event-boundary-sometimes-callback! 
         req
         (lambda (v) 
           ;; in atomic mode
           (when (unbox req) 
             (set-box! req #f)
             (enable win))))
        req)))))

(define (do-cancel-flush-delay req enable)
  (atomically
   (let ([win (unbox req)])
     (when win
       (set-box! req #f)
       (enable win)
       (remove-event-boundary-callback! req)))))
