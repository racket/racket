#lang scheme
(require scheme/async-channel)

; XXX More efficient structure
(define (snoc x l) (append l (list x)))

; Define mailboxes
(define-struct mailbox (manager control msgs))
(define (new-mailbox)
  (define control-ch (make-channel))
  (define msgs-ch (make-async-channel))
  ; Try to match one message
  (define (try-to-match req msg)
    (match req
      [(struct receive (reply-ch _ _ matcher))
       (with-handlers ([exn:misc:match? (lambda (x) #f)])
         (define the-match-thunk (matcher msg))
         ; XXX Handle partner's death
         (channel-put reply-ch the-match-thunk)
         #t)]))       
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
    (sync (handle-evt msgs-ch
                      (lambda (new-msg)
                        (not-on-receive (snoc new-msg msgs))))
          (handle-evt control-ch
                      (lambda (req)
                        (with-handlers ([exn? (lambda (x) (waiting-for-matching req msgs))])
                          (define new-msgs (try-to-match* req msgs))
                          ; One worked
                          (not-on-receive new-msgs))))))
  ; Waiting for a message that will match
  (define (waiting-for-matching req msgs)
    (match req
      [(struct receive (reply-ch timeout-evt timeout-thunk _))
       (sync (handle-evt timeout-evt
                         (lambda (_)
                           (channel-put reply-ch timeout-thunk)
                           (not-on-receive msgs)))
             (handle-evt msgs-ch
                         (lambda (new-msg)
                           (if (try-to-match req new-msg)
                               (not-on-receive msgs)
                               (waiting-for-matching req (snoc new-msg msgs))))))]))  
  (define manager
    (thread
     (lambda ()
       (not-on-receive empty))))  
  (make-mailbox manager control-ch msgs-ch))

(define-struct receive (reply-ch timeout timeout-thunk  matcher))
(define (mailbox-send! mb msg)
  (match mb
    [(struct mailbox (thd _ msgs))
     (thread-resume thd)
     (async-channel-put msgs msg)]))
(define (mailbox-receive mb timeout-evt timeout-thunk matcher)
  (match mb
    [(struct mailbox (thd control _))
     (define reply-ch (make-channel))
     (thread-resume thd)
     (channel-put control (make-receive reply-ch timeout-evt timeout-thunk matcher))
     (channel-get reply-ch)]))  

(provide/contract
 [mailbox? (any/c . -> . boolean?)]
 [new-mailbox (-> mailbox?)]
 [mailbox-send! (mailbox? any/c . -> . void)]
 [mailbox-receive (mailbox? evt? (-> any) (any/c . -> . (-> any)) . -> . (-> any))])