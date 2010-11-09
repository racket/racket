#lang racket/base
(require racket/class
         racket/gui/base)
(provide status-area%)

(define FADE-DELAY 1000)
(define NAP-TIME 0.01)

(define status-area%
  (class* object% (#| status-area<%> |#)
    (init parent
          stop-callback)

    (define lock (make-semaphore 1))

    (define-syntax-rule (with-lock . body)
      (dynamic-wind (lambda () (yield lock))
                    (lambda () . body)
                    (lambda () (semaphore-post lock))))

    (define timer (new timer% (notify-callback (lambda () (fade-out)))))

    (define pane
      (new horizontal-pane%
           (parent parent)
           (stretchable-height #f)))
    (define message
      (new message%
           (parent pane)
           (label "")
           (auto-resize #t)
           (stretchable-width #t)
           (style '(deleted))))
    (define stop-button
      (new button%
           (parent pane)
           (label "Stop")
           (enabled #f)
           (callback stop-callback)
           (style '(deleted))))

    (define visible? #t)

    (define/public (set-visible new-visible?)
      (with-lock
       (set! visible? new-visible?)
       (show (memq state '(shown fade)))))

    #|
    Three states:
      - 'none    = no message displayed
      - 'shown   = message displayed
      - 'fade    = message displayed, waiting to erase

    Timer is only started during 'fade state.
    |#
    (define state 'none)

    (define/private (show ?)
      (send pane change-children
            (lambda _
              (if (and ? visible?)
                  (list message stop-button)
                  null))))

    (define/public (set-status msg)
      (with-lock
       (cond [msg
              (case state
                ((none)
                 (send message set-label msg)
                 (send message enable #t)
                 (show #t)
                 (set! state 'shown))
                ((shown)
                 (send message set-label msg))
                ((fade)
                 (send timer stop) ;; but (update) may already be waiting
                 (send message set-label msg)
                 (send message enable #t)
                 (set! state 'shown)))]
             [(not msg)
              (case state
                ((shown)
                 (send timer start FADE-DELAY #t)
                 (send message enable #f)
                 (set! state 'fade)))])))

    (define/private (fade-out)
      (with-lock (fade-out*)))

    (define/private (fade-out*)
      (case state
        ((fade)
         (show #f)
         (send message set-label "")
         (set! state 'none))
        (else
         ;; timer not stopped in time; do nothing
         (void))))

    (define/public (enable-stop ?)
      (send stop-button enable ?))

    (super-new)))
