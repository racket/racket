#lang scheme
(require "dispatch.ss")
(provide/contract
 [interface-version dispatcher-interface-version/c]
 [make (number? dispatcher/c . -> . dispatcher/c)])

(define interface-version 'v1)
(define (make num inner)
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
                (if (< i num)
                    (handle-evt in-ch
                                (lambda (req)
                                  (channel-put (in-req-reply-ch req) #t)
                                  (loop (add1 i)
                                        (list* (in-req-partner req) partners))))
                    never-evt)
                (handle-evt out-ch
                            (lambda (req)
                              (loop (sub1 i)
                                    (remq (out-req-partner req) partners))))
                (map (lambda (p)
                       (handle-evt (thread-dead-evt p)
                                   (lambda _
                                     (loop (sub1 i) (remq p partners)))))
                     partners))))))
  (define (in)
    (define reply (make-channel))
    (channel-put in-ch (make-in-req (current-thread) reply))
    (channel-get reply))
  (define (out)
    (channel-put out-ch (make-out-req (current-thread))))
  (lambda (conn req)
    (dynamic-wind 
     in
     (lambda () 
       (inner conn req))
     out)))
