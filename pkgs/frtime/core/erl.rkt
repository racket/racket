#lang racket/base

(require racket/bool
         racket/match
         "match.rkt"
         "contract.rkt"
         #;"sema-mailbox.rkt"
         "mailbox.rkt")

(define-struct tid (lid) #:prefab)
(define (create-tid thr) (make-tid thr))

; We need a mapping from Racket's tids to our tids (just for `self')
; and a mapping from symbols to mailboxes (for local threads).

(define tids (make-weak-hash))
(define mailboxes (make-hash))

(define (do-receive timeout timeout-thunk matcher)
  (define mb (hash-ref mailboxes (tid-lid (self))))
  (define val-thunk (mailbox-receive mb timeout timeout-thunk matcher))
  (val-thunk))

(define-syntax receive
  (syntax-rules (after)
    [(_ (after timeout to-expr ...) (pat expr ...) ...)
     (do-receive 
      timeout 
      (lambda () to-expr ...)
      (match-lambda
        (pat (lambda () expr ...)) ...
        [_ match-fail]))]
    [(_ clause ...) (receive (after false (void)) clause ...)]))

; must ensure name not already taken
(define (spawn/name-help thunk name)
  (if (hash-ref mailboxes name (lambda () #f))
      #f
      (let ([new-tid (create-tid name)]
            [parent-tid (self)])
        (thread
         (lambda ()
           (hash-set! tids (current-thread) new-tid)
           (hash-set! mailboxes name (new-mailbox))
           (! parent-tid new-tid)
           (thunk)))
        (receive [(? (lambda (m) (equal? m new-tid))) new-tid]))))

(define next-thread
  (let ([last-thread 1]
        [lock (make-semaphore 1)])
    (lambda ()
      (call-with-semaphore
       lock
       (lambda ()
         (begin0
           last-thread
           (set! last-thread (add1 last-thread))))))))
(define (next-thread-name)
  (string->symbol
   (string-append "thread" (number->string (next-thread)))))

(define-syntax spawn/name
  (syntax-rules ()
    [(_ name expr ...)
     (spawn/name-help
      (lambda () expr ...)
      name)]))

(define (! tid msg)
  (define mb (hash-ref mailboxes (tid-lid tid) (lambda () false)))
  (when mb
    (send-msg mb msg)))

(define (send-msg mbox msg)
  (mailbox-send! mbox msg))

(define (self)
  (hash-ref! 
   tids (current-thread)
   ; allows thread not created by spawn to receive messages
   (lambda ()
     (define name (next-thread-name))
     (define new-tid (create-tid name))
     (hash-set! mailboxes name (new-mailbox))
     new-tid)))

(provide
 spawn/name
 receive)
(provide/contract*
 [! (tid? any/c . -> . void)]
 [self (-> tid?)])
