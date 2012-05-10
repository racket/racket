#lang racket/base
(require racket/contract
         racket/list
         web-server/dispatchers/dispatch)
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make ((number? dispatcher/c) (#:over-limit (symbols 'block 'kill-new 'kill-old)) . ->* . dispatcher/c)])

(define interface-version 'v1)
(define (make num inner #:over-limit [over-limit 'block])
  (define-struct in-req (partner reply-ch))
  (define in-ch (make-channel))
  (define-struct out-req (partner))
  (define out-ch (make-channel))
  (define limit-manager
    (thread
     (lambda ()
       (let loop ([i 0]
                  [partners empty])
         (apply sync 
                ; Do we have room for another...
                (if (< i num)
                    ; If so, allow them in
                    (handle-evt in-ch
                                (lambda (req)
                                  ; Reply asynchronously
                                  (thread 
                                   (lambda ()
                                     (channel-put (in-req-reply-ch req) #t)))
                                  (loop (add1 i)
                                        (list* (in-req-partner req) partners))))
                    ; Otherwise, decide what to do with new requests
                    (case over-limit
                      ; Make them block...
                      [(block)
                       never-evt]
                      ; Instruct the new request to die
                      [(kill-new)
                       (handle-evt in-ch
                                   (lambda (req)
                                     ; Reply asynchronously
                                     (thread
                                      (lambda ()
                                        (channel-put (in-req-reply-ch req) #f)))
                                     (loop i partners)))]
                      ; Kill an old request handler and allow this one
                      [(kill-old)
                       (handle-evt in-ch
                                   (lambda (req)
                                     (define oldest (last partners))
                                     (define remaining (take partners (sub1 (length partners))))
                                     ; Kill the oldest thread
                                     (kill-thread oldest)
                                     ; Reply asynchronously
                                     (thread 
                                      (lambda ()
                                        (channel-put (in-req-reply-ch req) #t)))
                                     (loop i (list* (in-req-partner req) remaining))))]))
                ; Wait for partners to complete
                (handle-evt out-ch
                            (lambda (req)
                              (loop (sub1 i)
                                    (remq (out-req-partner req) partners))))
                ; Check if partners are dead
                (map (lambda (p)
                       (handle-evt (thread-dead-evt p)
                                   (lambda _
                                     (loop (sub1 i) (remq p partners)))))
                     partners))))))
  (define (in)
    (define reply (make-channel))
    (channel-put in-ch (make-in-req (current-thread) reply))
    (unless (channel-get reply)
      (error 'limit "limit-manager requested load shedding")))
  (define (out)
    (channel-put out-ch (make-out-req (current-thread))))
  (lambda (conn req)
    (dynamic-wind 
     in
     (lambda () 
       (inner conn req))
     out)))
