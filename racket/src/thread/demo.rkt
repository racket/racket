#lang racket/base
(require "bootstrap-main.rkt")

;; Don't use exception handlers here, because the "bootstrap.rkt"
;; implementation of engines can't support it.

(define done? #f)

(call-in-main-thread
 (lambda ()
   (define-syntax-rule (check a b)
     (let ([a-v a]
           [b-v b])
       (unless (equal? a-v b-v)
         (error 'failed "~s: ~e vs. ~e" 'b a-v b-v))))

   ;; Check semaphores
   (check #t (thread? (current-thread)))
   (check #t (evt? (current-thread)))
   (define s (make-semaphore))
   (define t0 (thread (lambda () (semaphore-wait s) (printf "__\n") (semaphore-post s))))
   (define t1 (thread (lambda () (semaphore-wait s) (printf "hi\n") (semaphore-post s))))
   (define t2 (thread (lambda () (printf "HI\n") (semaphore-post s))))
   (thread-wait t0)
   (thread-wait t1)
   (thread-wait t2)
   
   ;; Check channels
   (define ch (make-channel))
   (define ct1 (thread (lambda () (printf "1 ~a\n" (channel-get ch)))))
   (define ct2 (thread (lambda () (printf "2 ~a\n" (channel-get ch)))))
   (channel-put ch 'a)
   (channel-put ch 'b)
   
   (define cpt1 (thread (lambda () (channel-put ch 'c))))
   (define cpt2 (thread (lambda () (channel-put ch 'd))))
   (printf "3 ~a\n" (channel-get ch))
   (printf "4 ~a\n" (channel-get ch))

   ;; Check timeout
   (check #f (sync/timeout 0.1))
   (check #f (sync/timeout 0.1 never-evt))

   ;; Check semaphore polling
   (check s (sync/timeout 0 s))
   (check #f (sync/timeout 0 s))

   ;; Check more semaphore polling
   (define s2 (make-semaphore 3))
   (check s2 (sync/timeout 0 s s2))
   (check s2 (sync/timeout 0 s2 s))
   (check 'got-s2 (sync s (wrap-evt s2 (lambda (v) (check v s2) 'got-s2))))
   (check #f (sync/timeout 0 s2 s))

   ;; Choice evts
   (define choice1 (choice-evt s s2))
   (semaphore-post s2)
   (check s2 (sync choice1))
   (semaphore-post s)
   (check s (sync choice1))
   (check #f (sync/timeout 0 choice1))

   ;; Check channel and `sync`
   (void (thread (lambda () (channel-put ch 'c2))))
   (check 'c2 (sync ch))
   
   ;; Check channel-put events
   (void (thread (lambda () (check 'c3 (channel-get ch)))))
   (define pc (channel-put-evt ch 'c3))
   (check pc (sync pc))

   ;; Check guard event
   (define ok-evt (guard-evt
                   (lambda ()
                     (define ch (make-channel))
                     (thread (lambda () (channel-put ch 'ok)))
                     ch)))
   (check 'ok (sync ok-evt))
   
   ;; Check semaphore-peek events
   (semaphore-post s)
   (define sp (semaphore-peek-evt s))
   (check sp (sync/timeout 0 sp))
   (check sp (sync/timeout 0 sp))
   (check s (sync/timeout 0 s))
   (check #f (sync/timeout 0 sp))

   ;; Check nacks
   (define nack #f)
   (check #t (semaphore? (sync (nack-guard-evt (lambda (n) (set! nack n) (make-semaphore 1))))))
   (check #f (sync/timeout 0 nack))
   (set! nack #f)
   (let loop ()
     (check 'ok (sync (nack-guard-evt (lambda (n) (set! nack n) (make-semaphore))) ok-evt))
     (unless nack (loop)))
   (check (void) (sync/timeout 0 nack))
   
   (semaphore-post s)
   (check #f (sync/timeout 0 ch (channel-put-evt ch 'oops)))
   (check sp (sync/timeout #f ch (channel-put-evt ch 'oops) sp))

   (let ([v #f])
     (check #f (sync/timeout 0
                             (nack-guard-evt
                              (lambda (nack)
                                (set! v nack)
                                (choice-evt (make-semaphore) (make-semaphore))))))
     (check (void) (sync/timeout 0 v)))

   ;; evt chaperone
   (define e1 (make-semaphore 1))
   (check #t (chaperone-of? (chaperone-evt e1 void) e1))
   (check #f (chaperone-of? e1 (chaperone-evt e1 void)))
   (let ([hit #f])
     (check e1 (sync (chaperone-evt e1 (lambda (e)
                                         (set! hit e)
                                         (values e values)))))
     (check e1 hit))
   (check #t (semaphore? (chaperone-evt e1 void)))

   (check #t (chaperone-of? (chaperone-evt ch void) ch))
   (check #t (channel? (chaperone-evt ch void)))
   (check #t (channel? (chaperone-channel ch void void)))
   (let ([proc (lambda (arg) arg)])
     (thread (lambda () (channel-put ch proc)))
     (let ([proc2 (channel-get (chaperone-evt ch (lambda (ch)
                                                   (values ch
                                                           (lambda (proc)
                                                             (chaperone-procedure proc void))))))])
       (check #f (eq? proc2 proc))
       (check #t (chaperone-of? proc2 proc))))
   (let ([got #f])
     (define th (thread (lambda () (set! got (channel-get ch)))))
     (channel-put (chaperone-evt ch void) 'ok)
     (check th (sync th))
     (check got 'ok))
   (define (check-chaperone-channel channel-put)
     (let ([proc (lambda (arg) arg)]
           [got #f])
       (define th (thread (lambda () (set! got (channel-get ch)))))
       (channel-put (chaperone-channel ch
                                       (lambda (ch) (values ch values))
                                       (lambda (ch proc)
                                         (chaperone-procedure proc void)))
                    proc)
       (check #f (eq? got proc))
       (check #t (chaperone-of? got proc))))
   (check-chaperone-channel channel-put)
   (check-chaperone-channel (lambda (ch v) (sync (channel-put-evt ch v))))

   ;; Check sleeping in main thread
   (define now1 (current-inexact-milliseconds))
   (sleep 0.1)
   (check #t ((current-inexact-milliseconds) . >= . (+ now1 0.1)))

   ;; Check sleeping in other thread
   (define now2 (current-inexact-milliseconds))
   (define ts (thread (lambda () (sleep 0.1))))
   (check ts (sync ts))
   (check #t ((current-inexact-milliseconds) . >= . (+ now2 0.1)))

   ;; Check `alarm-evt`
   (define now2+ (current-inexact-milliseconds))
   (define alm (alarm-evt (+ 0.1 now2+)))
   (check alm (sync alm))
   (check #t ((current-inexact-milliseconds) . >= . (+ now2+ 0.1)))

   ;; Check system-idle event
   (define v 0)
   (thread (lambda () (set! v (add1 v))))
   (sync (system-idle-evt))
   (check 1 v)

   ;; Check `replace-evt`
   (check 5 (sync (replace-evt always-evt (lambda (v) (wrap-evt always-evt (lambda (v) 5))))))
   (check #f (sync/timeout 0 (replace-evt never-evt void)))
   (let ([ns null])
     (check #f (sync/timeout 0 (replace-evt (choice-evt
                                             (nack-guard-evt
                                              (lambda (n)
                                                (set! ns (cons n ns))
                                                never-evt))
                                             (nack-guard-evt
                                              (lambda (n)
                                                (set! ns (cons n ns))
                                                never-evt)))
                                            void)))
     (check 2 (length ns))
     (check (void) (sync (car ns)))
     (check (void) (sync (cadr ns))))

   ;; Check `thread-send`
   (check (void) (thread-send (current-thread) 'sent0))
   (check (void) (thread-send (current-thread) 'sent1))
   (check 'sent0 (thread-receive))
   (check 'sent1 (thread-receive))
   (check #f (thread-try-receive))
   (check (void) (thread-send (current-thread) 'sent2))
   (check 'sent2 (thread-try-receive))
   (let ([t (current-thread)])
     (thread (lambda ()
               (sync (system-idle-evt))
               (thread-send t 'sent3))))
   (check 'sent3 (thread-receive))

   (define rcv (thread-receive-evt))
   (check #f (sync/timeout 0 rcv))
   (check (void) (thread-send (current-thread) 'sent4))
   (check rcv (sync/timeout #f rcv))
   (check 'sent4 (thread-receive))
   (check #f (sync/timeout 0 rcv))

   (let ([r #f])
     (define t (thread (lambda ()
                         (set! r (sync rcv rcv)))))
     (sync (system-idle-evt))
     (thread-send t 'ok)
     (sync (system-idle-evt))
     (check t (sync/timeout 0 t))
     (check rcv r))

   (define (check-break/kill #:kill? kill?)
     (define stop-thread (if kill? kill-thread break-thread))
     (define (report-expected-exn what)
       (unless kill?
         (printf "[That ~a was from a thread, and it's expected]\n" what)))
     (define (report-expected-break)
       (report-expected-exn "break"))

     ;; Check that a loop can be abandoned
     (define tinf (thread (lambda () (let loop () (loop)))))
     (sleep)
     (stop-thread tinf)
     (check tinf (sync tinf))
     (report-expected-break)

     ;; Check that a break exception is delayed if disabled
     (define now3 (current-inexact-milliseconds))
     (define tdelay (with-continuation-mark
                        break-enabled-key
                      (make-thread-cell #f)
                      (thread (lambda ()
                                (sleep 0.1)
                                (with-continuation-mark
                                    break-enabled-key
                                  (make-thread-cell #t)
                                  (begin
                                        ;(check-for-break)
                                    (let loop () (loop))))))))
     (stop-thread tdelay)
     (check tdelay (sync tdelay))
     (report-expected-break)
     (unless kill?
       (check #t ((current-inexact-milliseconds) . >= . (+ now3 0.1))))

     ;; Check that a semaphore wait can be abandoned
     (define tstuck (thread (lambda () (semaphore-wait (make-semaphore)))))
     (sync (system-idle-evt))
     (stop-thread tstuck)
     (check tstuck (sync tstuck))
     (report-expected-break)

     ;; Check that an externally abanoned `sync` posts nacks
     (define nack1 #f)
     (define nack2 #f)
     (define tstuck2 (thread (lambda ()
                               (sync (nack-guard-evt
                                      (lambda (s) (set! nack1 s) never-evt))
                                     (nack-guard-evt
                                      (lambda (s) (set! nack2 s) never-evt))))))
     (sync (system-idle-evt))
     (stop-thread tstuck2)
     (thread-wait tstuck2)
     (report-expected-break)
     (check (void) (sync nack1))
     (check (void) (sync nack2))

     ;; Make sure a `sync` can be abandoned during a guard callback
     (define tfail (thread (lambda ()
                             (sync (nack-guard-evt
                                    (lambda (s)
                                      (set! nack1 s)
                                      (if kill?
                                          (kill-thread (current-thread))
                                          (error "oops"))))))))
     (check tfail (sync tfail))
     (report-expected-exn "oops")
     (check (void) (sync nack1))
     
     ;; Make sure nested abandoned `syncs` are ok
     (define tfail2 (thread (lambda ()
                              (sync (nack-guard-evt
                                     (lambda (s)
                                       (set! nack1 s)
                                       (sync (nack-guard-evt
                                              (lambda (s)
                                                (set! nack2 s)
                                                (if kill?
                                                    (kill-thread (current-thread))
                                                    (error "oops")))))))))))
     (check tfail2 (sync tfail2))
     (report-expected-exn "oops")
     (check (void) (sync nack1))
     (check (void) (sync nack2)))
   
   (check-break/kill #:kill? #f)
   (check-break/kill #:kill? #t)

   ;; Check that an ignored break doesn't interfere with semaphore waiting, etc.
   (define (check-ignore-break-retry make-trigger trigger-post trigger-wait)
     (define s/nb (make-trigger))
     (define done?/nb #f)
     (define t/nb (with-continuation-mark
                      break-enabled-key
                      (make-thread-cell #f)
                    (thread (lambda ()
                              (trigger-wait s/nb)
                              (set! done?/nb #t)))))
     (sync (system-idle-evt))
     (break-thread t/nb)
     (sync (system-idle-evt))
     (check #f (sync/timeout 0 t/nb))
     (check #f done?/nb)
     (trigger-post s/nb)
     (sync (system-idle-evt))
     (check t/nb (sync/timeout 0 t/nb))
     (check #t done?/nb))

   (check-ignore-break-retry make-semaphore semaphore-post sync)
   (check-ignore-break-retry make-semaphore semaphore-post semaphore-wait)
   (check-ignore-break-retry make-channel (lambda (c) (channel-put c 'go)) channel-get)
   (check-ignore-break-retry make-channel channel-get (lambda (c) (channel-put c 'go)))
   (check-ignore-break-retry (lambda () (box #f))
                             (lambda (b) (thread-resume (unbox b)))
                             (lambda (b) (set-box! b (current-thread)) (thread-suspend (current-thread))))
   (check-ignore-break-retry (lambda () (box #f))
                             (lambda (b) (thread-send (unbox b) 'ok))
                             (lambda (b) (set-box! b (current-thread)) (thread-receive)))

   ;; Check suspending and resuming a thread that is waiting on a semaphore
   (check #f (sync/timeout 0 s2))
   (define t/sw (thread
                 (lambda ()
                   (sync s2))))
   (check t/sw (sync (thread-resume-evt t/sw)))
   (define t/sw-s-evt (thread-suspend-evt t/sw))
   (check #f (sync/timeout 0 t/sw-s-evt))
   (sync (system-idle-evt))
   (check #f (sync/timeout 0 t/sw))
   (thread-suspend t/sw)
   (check t/sw (sync/timeout 0 t/sw-s-evt))
   (check #f (sync/timeout 0 t/sw))
   (semaphore-post s2)
   (sync (system-idle-evt))
   (check #f (sync/timeout 0 t/sw))
   (thread-resume t/sw)
   (check t/sw (sync t/sw))
   (check #f (sync/timeout 0 s2))

   ;; Check suspending and resuming a thread that is waiting on a message
   (define (check-suspend-thread-receive send-after-resume?)
     (define t/sr (thread
                   (lambda ()
                     (channel-put ch (thread-receive)))))
     (sync (system-idle-evt))
     (thread-suspend t/sr)
     (unless send-after-resume?
       (thread-send t/sr 'ok))
     (check #f (sync/timeout 0 ch))
     (check #f (sync/timeout 0 t/sr))
     (thread-resume t/sr)
     (when send-after-resume?
       (thread-send t/sr 'ok))
     (check 'ok (sync ch)))
   (check-suspend-thread-receive #t)
   (check-suspend-thread-receive #f)

   ;; Check sync/enable-break => break
   (define tbe (with-continuation-mark
                break-enabled-key
                (make-thread-cell #f)
                (thread (lambda ()
                          (sync/enable-break (make-semaphore))))))
   (sync (system-idle-evt))
   (check #f (sync/timeout 0 tbe))
   (break-thread tbe)
   (sync (system-idle-evt))
   (check tbe (sync/timeout 0 tbe))
   (printf "[That break was from a thread, and it's expected]\n")

   ;; Check sync/enable-break => semaphore
   (check #f (sync/timeout 0 s2))
   (define tbe2 (with-continuation-mark
                 break-enabled-key
                 (make-thread-cell #f)
                 (thread (lambda ()
                           (sync/enable-break s2)))))
   (sync (system-idle-evt))
   (semaphore-post s2) ; => chooses `s2` in `sync/enable-break`
   (break-thread tbe2)
   (sync (system-idle-evt))
   (check tbe2 (sync/timeout 0 tbe2))
   (check #f (sync/timeout 0 s2))

   ;; Check call-with-semaphore
   (semaphore-post s2)
   (check #f (call-with-semaphore s2 (lambda () (sync/timeout 0 s2))))
   (check s2 (sync/timeout 0 s2))
   (define t/cws (thread (lambda () (call-with-semaphore s2 (lambda () (error "shouldn't get here"))))))
   (sync (system-idle-evt))
   (check #f (sync/timeout 0 t/cws))
   (break-thread t/cws)
   (sync (system-idle-evt))
   (printf "[That break was from a thread, and it's expected]\n")
   (check t/cws (sync/timeout 0 t/cws))

   ;; Check call-in-nested-thread
   (check 10 (call-in-nested-thread (lambda () 10)))
   (check '(1 2) (call-with-values (lambda ()
                                     (call-in-nested-thread (lambda () (values 1 2))))
                   list))

   ;; Custodians
   (define c (make-custodian))
   (define cb (make-custodian-box c'running))
   (check 'running (custodian-box-value cb))
   (custodian-shutdown-all c)
   (check #f (custodian-box-value cb))
   (custodian-shutdown-all c)

   (define c2 (make-custodian))
   (define t/cust (parameterize ([current-custodian c2])
                    (thread (lambda () (sync (make-semaphore))))))
   (sync (system-idle-evt))
   (check #t (thread-running? t/cust))
   (custodian-shutdown-all c2)
   (check #f (thread-running? t/cust))
   
   (define c3 (make-custodian))
   (define c4 (make-custodian c3))
   (define t/cust2 (parameterize ([current-custodian c4])
                     (thread (lambda () (sync (make-semaphore))))))
   (sync (system-idle-evt))
   (check #t (thread-running? t/cust2))
   (custodian-shutdown-all c3)
   (check #f (thread-running? t/cust2))

   (define c5-1 (make-custodian))
   (define c5-2 (make-custodian))
   (define t/custs (parameterize ([current-custodian c5-1])
                     (thread (lambda () (sync (make-semaphore))))))
   (thread-resume t/custs c5-2)
   (sync (system-idle-evt))
   (check #t (thread-running? t/custs))
   (custodian-shutdown-all c5-1)
   (check #t (thread-running? t/custs))
   (custodian-shutdown-all c5-2)
   (check #f (thread-running? t/custs))

   (define c6-1 (make-custodian))
   (define c6-2 (make-custodian))
   (define s6 (make-semaphore))
   (define r6 #f)
   (define t/s2k (parameterize ([current-custodian c6-1])
                   (thread/suspend-to-kill (lambda () (sync s6) (set! r6 'complete)))))
   (sync (system-idle-evt))
   (check #t (thread-running? t/s2k))
   (custodian-shutdown-all c6-1)
   (check #f (thread-running? t/s2k))
   (check #f (thread-dead? t/s2k))
   (thread-resume t/s2k c6-2)
   (check #t (thread-running? t/s2k))
   (semaphore-post s6)
   (sync t/s2k)
   (check 'complete r6)

   (define t/r1 (thread (lambda () (sync (make-semaphore)))))
   (define t/r2 (thread (lambda () (sync (make-semaphore)))))
   (thread-resume t/r2 t/r1)
   (thread-suspend t/r2)
   (check #f (thread-running? t/r2))
   (check #f (thread-dead? t/r2))
   (thread-resume t/r1)
   (check #f (thread-running? t/r2)) ; because `t/r1` was not suspended
   (thread-suspend t/r1)
   (thread-resume t/r1)
   (check #t (thread-running? t/r2))
   (kill-thread t/r1)
   (kill-thread t/r2)

   ;; Check will executors
   (define we (make-will-executor))
   (check #t (will-executor? we))
   (check #f (will-try-execute we))
   (check (void) (will-register we (gensym) (lambda (s) s)))
   (collect-garbage)
   (check #t (symbol? (will-try-execute we)))
   (check #f (will-try-execute we))
   (check (void) (will-register we (gensym) (lambda (s) s)))
   (thread (lambda () (sync (system-idle-evt)) (collect-garbage)))
   (check #t (symbol? (will-execute we)))

   ;; Check places, where the various export symbols passed to
   ;; `dynamic-place` are registered via `register-place-symbol!`
   (register-place-symbol! 'nothing void)
   (define-values (pl1 pin1 pout1 perr1) (dynamic-place 'dummy 'nothing #f #f #f))
   (check #t (place? pl1))
   (check 0 (place-wait pl1))

   (register-place-symbol! 'exit1 (lambda (pch) (exit 1)))
   (define-values (pl2 pin2 pout2 perr2) (dynamic-place 'dummy 'exit1 #f #f #f))
   (check #t (place? pl2))
   (check 1 (place-wait pl2))

   (register-place-symbol! 'loop (lambda (pch) (let loop () (loop))))
   (define-values (pl3 pin3 pout3 perr3) (dynamic-place 'dummy 'loop #f #f #f))
   (check #t (place? pl3))
   (place-break pl3)
   (check 1 (place-wait pl3))
   (printf "[That break was from a place, and it's expected]\n")

   (define-values (pl4 pin4 pout4 perr4) (dynamic-place 'dummy 'loop #f #f #f))
   (check #f (sync/timeout 0.01 (place-dead-evt pl4)))
   (place-kill pl4)
   (check 1 (place-wait pl4))
   (check #t (evt? (sync (place-dead-evt pl4))))
   (check #t (evt? (sync/timeout 0.01 (place-dead-evt pl4))))

   ;; Place channel
   (define-values (left1 right1) (place-channel))
   (check #t (place-channel? left1))
   (check #t (place-channel? right1))
   (place-channel-put left1 'a)
   (check 'a (place-channel-get right1))
   (place-channel-put right1 'b)
   (check 'b (place-channel-get left1))
   (check #t (evt? left1))
   (check #t (evt? right1))
   (place-channel-put left1 'c)
   (place-channel-put left1 'd)
   (check 'c (sync right1))
   (check 'd (sync/timeout 0 right1))

   ;; Inter-place place channel
   (register-place-symbol! 'vector-echo (lambda (pch)
                                          (for ([i 3])
                                            (place-channel-put pch (vector (place-channel-get pch))))))
   (define-values (pl5 pin5 pout5 perr5) (dynamic-place 'dummy 'vector-echo #f #f #f))
   (check #t (place? pl5))
   (check #t (place-channel? pl5))
   (check #t (evt? pl5))
   (check (void) (place-channel-put pl5 'howdy))
   (check (vector 'howdy) (place-channel-get pl5))
   (check (void) (place-channel-put pl5 'hiya))
   (check (vector 'hiya) (sync pl5))
   (define got5 #f)
   (define t5 (thread (lambda () (set! got5 (place-channel-get pl5)))))
   (sync (system-idle-evt))
   (check #f (sync/timeout 0 t5))
   (check (void) (place-channel-put pl5 'again))
   (check t5 (sync t5))
   (check (vector 'again) got5)
   (check 0 (place-wait pl5))

   (set! done? #t)))

(unless done?
  (error "main thread stopped running due to deadlock?"))
