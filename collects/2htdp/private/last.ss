#lang scheme/gui

(require "timer.ss")

(provide last-mixin)

(define last-mixin
  (mixin (start-stop<%>) ()
    (field [end:ch  (make-channel)])

    ;; X -> Void
    (define/override (stop! w)
      (send-to-last w)
      (super stop! w))
    
    ;; -> World
    (define/public (last) 
      (define result (yield end:ch))
      (if (exn? result) (raise result) result))
    
    (field [dr:cust (current-custodian)])

    ;; X -> Void
    ;; send x to last method
    (define/private (send-to-last x)
      (parameterize ((current-custodian dr:cust))
        (thread (lambda () (channel-put end:ch x)))))
    
    (super-new)))
    

