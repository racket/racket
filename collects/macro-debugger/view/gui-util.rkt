#lang racket/base
(require racket/class
         racket/gui/base)
(provide status-area%)

(define SHOW-DELAY 1000)
(define FADE-DELAY 400)
(define NAP-TIME 0.1)

(define status-area%
  (class* object% (#| status-area<%> |#)
    (init parent)

    (define lock (make-semaphore 1))

    (define-syntax-rule (with-lock . body)
      (dynamic-wind (lambda () (semaphore-wait lock))
                    (lambda () . body)
                    (lambda () (semaphore-post lock))))

    (define timer (new timer% (notify-callback (lambda () (update)))))

    (define pane
      (new horizontal-pane%
           (parent parent)
           (stretchable-height #f)))
    (define message
      (new message%
           (parent pane)
           (label "")
           (auto-resize #t)
           (style '(deleted))))

    #|
    Four states:
      - 'none    = no message displayed, none pending
      - 'pending = no message displayed, message pending
      - 'shown   = message displayed
      - 'fade    = message displayed, waiting to erase
    |#
    (define state 'none)
    (define pending #f)

    (define/public (set-status msg [immediate? #f])
      (with-lock
       (when immediate? (send timer stop))
       (cond [msg
              (case state
                ((none)
                 (cond [#f ;; immediate?
                        (set! state 'shown)
                        (send pane change-children (lambda _ (list message)))
                        (send message set-label msg)
                        (set! pending #f)
                        (sleep/yield NAP-TIME)]
                       [else
                        (set! state 'pending)
                        (set! pending msg)
                        (unless immediate? (send timer start SHOW-DELAY #t))]))
                ((pending)
                 (set! pending msg))
                ((shown)
                 (send message set-label msg))
                ((fade)
                 (send timer stop) ;; but (update) may already be waiting
                 (set! state 'shown)
                 (send message set-label msg)))]
             [(not msg)
              (case state
                ((none) (void))
                ((pending)
                 (send timer stop) ;; but (update) may already be waiting
                 (set! state 'none)
                 (set! pending #f))
                ((shown)
                 (set! state 'fade)
                 (unless immediate? (send timer start FADE-DELAY #t))))])
       (when immediate? (update*) (sleep/yield NAP-TIME))))

    (define/private (update)
      (with-lock (update*)))

    (define/private (update*)
      (case state
        ((pending)
         (set! state 'shown)
         (send pane change-children (lambda _ (list message)))
         (send message set-label pending)
         (set! pending #f))
        ((fade)
         (set! state 'none)
         (send pane change-children (lambda _ null)))
        ((none shown)
         ;; timer not stopped in time; do nothing
         (void))))

    (super-new)))
