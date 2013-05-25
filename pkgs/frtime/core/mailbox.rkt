#lang racket/base

(require racket/bool
         racket/list
         racket/match
         "contract.rkt"
         "match.rkt"
         racket/async-channel)

; XXX More efficient structure
(define (snoc x l) (append l (list x)))

; Define mailboxes
(define-struct mailbox (manager control))
(define (new-mailbox)
  (define control-ch (make-channel))
  (define (thread-recv-evt)
    (wrap-evt (thread-receive-evt)
              (lambda (e) (thread-receive))))
  ; Try to match one message
  (define (try-to-match req msg)
    (match req
      [(struct receive (reply-ch _ _ matcher))
       (define the-match-thunk (matcher msg))
       (if (eq? the-match-thunk match-fail)
           #f
           (begin
             ; XXX Handle partner's death
             (channel-put reply-ch the-match-thunk)
             #t))]))
  ; Try to match a list of messages
  (define (try-to-match* req msgs)
    (match msgs
      [(list) (error 'try-to-match* "No matches")]
      [(list-rest msg msgs)
       (if (try-to-match req msg)
           msgs
           (list* msg (try-to-match* req msgs)))]))
  ; Accept new messages until we need to match one
  (define (not-on-receive msgs)
    (sync (handle-evt (thread-recv-evt)
                      (lambda (new-msg)
                        (not-on-receive (snoc new-msg msgs))))
          (handle-evt control-ch
                      (lambda (req)
                        (with-handlers ([exn? (lambda (x) (waiting-for-matching (current-inexact-milliseconds) req msgs))])
                          (define new-msgs (try-to-match* req msgs))
                          ; One worked
                          (not-on-receive new-msgs))))))
  ; Waiting for a message that will match
  (define (waiting-for-matching start-time req msgs)
    (match req
      [(struct receive (reply-ch timeout timeout-thunk _))
       (define elapsed (- (current-inexact-milliseconds) start-time))
       (define wait-time
         (cond
           [(not timeout) false]
           [(> elapsed timeout) 0]
           [else (/ (- timeout elapsed) 1000.0)]))
       (define new-msg (sync/timeout wait-time (thread-recv-evt)))
       (if new-msg
           (if (try-to-match req new-msg)
               (not-on-receive msgs)
               (waiting-for-matching start-time req (snoc new-msg msgs)))
           (begin 
             (channel-put reply-ch timeout-thunk)
             (not-on-receive msgs)))]))
  (define manager
    (thread
     (lambda ()
       (not-on-receive empty))))  
  (make-mailbox manager control-ch))

(define-struct receive (reply-ch timeout timeout-thunk matcher))
(define (mailbox-send! mb msg)
  (match mb
    [(struct mailbox (thd _))
     (thread-resume thd)
     (thread-send thd msg)]))
(define (mailbox-receive mb timeout timeout-thunk matcher)
  (match mb
    [(struct mailbox (thd control))
     (define reply-ch (make-channel))
     (thread-resume thd)
     (channel-put control (make-receive reply-ch timeout timeout-thunk matcher))
     (channel-get reply-ch)]))  

(provide/contract*
 [mailbox? (any/c . -> . boolean?)]
 [new-mailbox (-> mailbox?)]
 [mailbox-send! (mailbox? (not/c false/c) . -> . void)]
 [mailbox-receive (mailbox? (or/c false/c number?) (-> any) (any/c . -> . (-> any)) . -> . (-> any))])
