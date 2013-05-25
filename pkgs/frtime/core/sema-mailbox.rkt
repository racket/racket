#lang racket/base

(require racket/list
         racket/bool
         racket/match
         "match.rkt"
         "contract.rkt")

(define (call-with-semaphore s thunk)
    (semaphore-wait s)
    (let ([result (thunk)])
      (semaphore-post s)
      result))

(define Mcons mcons)
(define release void)

(define-struct mailbox (old-head old-last head tail sem-count sem-space lock-enqueue) #:mutable)

(define (new-mailbox)
  (let* ([sentinel (Mcons empty empty)]
         [old-sentinel (Mcons empty empty)]
         [old-head (Mcons empty old-sentinel)])
    (make-mailbox old-head
                  old-sentinel
                  sentinel
                  sentinel
                  (make-semaphore)
                  (make-semaphore 1000)
                  (make-semaphore 1))))

(define (try-extract m l)
  (let loop ([prev l] [cur (mcdr l)])
    (if (empty? (mcdr cur))
        #f
        (let ([v (m (mcar cur))])
          (if (eq? v match-fail)
              (loop cur (mcdr cur))
              (begin0 v
                      (set-mcdr! prev (mcdr cur))
                      (release cur)))))))

(define (mailbox-receive mb timeout timeout-thunk matcher)
  (define start-time (current-inexact-milliseconds))
  (match (try-extract matcher (mailbox-old-head mb))
    [#f
     (let wait ()
       (let* ([elapsed (- (current-inexact-milliseconds) start-time)]
              [wait-time (cond
                           [(not timeout) false]
                           [(> elapsed timeout) 0]
                           [else (/ (- timeout elapsed) 1000.0)])]
              [not-timeout? (sync/timeout wait-time (mailbox-sem-count mb))])
         (if not-timeout?
             (let* ([oldhead (mailbox-head mb)]
                    [msg (mcar oldhead)]
                    [v 
                     (begin (set-mailbox-head! mb (mcdr oldhead))
                            (release oldhead)
                            (semaphore-post (mailbox-sem-space mb))
                            (matcher msg))])
               (if (eq? v match-fail)
                   (let ([new-last (Mcons empty empty)]
                         [old-last (mailbox-old-last mb)])
                     (set-mcar! old-last msg)
                     (set-mcdr! old-last new-last)
                     (set-mailbox-old-last! mb new-last)
                     (wait))
                   v))
             timeout-thunk)))]
    [val
     val]))

(define (mailbox-send! mbox msg)
  (call-with-semaphore
   (mailbox-lock-enqueue mbox)
   (lambda ()
     (let ([newtail (Mcons empty empty)]
           [oldtail (mailbox-tail mbox)])
       (set-mcar! oldtail msg)
       (set-mcdr! oldtail newtail)
       (set-mailbox-tail! mbox newtail)
       (semaphore-wait (mailbox-sem-space mbox))
       (semaphore-post (mailbox-sem-count mbox))))))

(provide/contract*
 [mailbox? (any/c . -> . boolean?)]
 [new-mailbox (-> mailbox?)]
 [mailbox-send! (mailbox? any/c . -> . void)]
 [mailbox-receive (mailbox? (or/c false/c number?) (-> any) (any/c . -> . (-> any)) . -> . (-> any))])
