(import (rumble)
        (thread))

(define-syntax declare
  (syntax-rules ()
    [(_ id ...)
     (begin (define id #f) ...)]))

(define done? #f)

(call-in-main-thread
 (lambda ()
   (define-syntax check
     (syntax-rules ()
       [(_ a b)
        (unless (equal? a b)
          (printf "~s: ~s vs. ~s\n" 'b a b)
          (error 'check "failed"))]))
   
   (declare s t0 t1 t2
            ch ct1 ct2
            cpt1 cpt2
            s2
            pc
            ok-evt
            sp
            nack
            now1 now2 now3
            t tinf tdelay
            tdw dw-s dw-pre? dw-body? dw-post?)
   
   (define-syntax define
     (syntax-rules ()
       [(_ id rhs) (set! id rhs)]))

   (check #t (thread? (current-thread)))
   (check #t (evt? (current-thread)))
   (define s (make-semaphore))
   (define t0 (thread (lambda () (semaphore-wait s) (printf "__\n") (semaphore-post s))))
   (define t1 (thread (lambda () (semaphore-wait s) (printf "hi\n") (semaphore-post s))))
   (define t2 (thread (lambda () (printf "HI\n") (semaphore-post s))))
   (thread-wait t0)
   (thread-wait t1)
   (thread-wait t2)
   
   (define ch (make-channel))
   (define ct1 (thread (lambda () (printf "1 ~a\n" (channel-get ch)))))
   (define ct2 (thread (lambda () (printf "2 ~a\n" (channel-get ch)))))
   (channel-put ch 'a)
   (channel-put ch 'b)
   
   (define cpt1 (thread (lambda () (channel-put ch 'c))))
   (define cpt2 (thread (lambda () (channel-put ch 'd))))
   (printf "3 ~a\n" (channel-get ch))
   (printf "4 ~a\n" (channel-get ch))

   (check s (sync/timeout 0 s))
   (check #f (sync/timeout 0 s))

   (define s2 (make-semaphore 3))
   (check s2 (sync/timeout 0 s s2))
   (check s2 (sync/timeout 0 s2 s))
   (check 'got-s2 (sync s (wrap-evt s2 (lambda (v) (check v s2) 'got-s2))))
   (check #f (sync/timeout 0 s2 s))

   (void (thread (lambda () (channel-put ch 'c2))))
   (check 'c2 (sync ch))
   
   (void (thread (lambda () (check 'c3 (channel-get ch)))))
   (define pc (channel-put-evt ch 'c3))
   (check pc (sync pc))

   (define ok-evt (guard-evt
                   (lambda ()
                     (define ch (make-channel))
                     (thread (lambda () (channel-put ch 'ok)))
                     ch)))
   (check 'ok (sync ok-evt))
   
   (semaphore-post s)
   (define sp (semaphore-peek-evt s))
   (check sp (sync/timeout 0 sp))
   (check sp (sync/timeout 0 sp))
   (check s (sync/timeout 0 s))
   (check #f (sync/timeout 0 sp))

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

   (define now1 (current-inexact-milliseconds))
   (sleep 0.1)
   (check #t (>= (current-inexact-milliseconds) (+ now1 0.1)))

   (define now2 (current-inexact-milliseconds))
   (define ts (thread (lambda () (sleep 0.1))))
   (check ts (sync ts))
   (check #t (>= (current-inexact-milliseconds) (+ now2 0.1)))

   (define v 0)
   (thread (lambda () (set! v (add1 v))))
   (sync (system-idle-evt))
   (check 1 v)
   
   (define tinf (thread (lambda () (let loop () (loop)))))
   (break-thread tinf)
   (check tinf (sync tinf))
   (printf "[That break was from a thread, and it's expected]\n")

   (define now3 (current-inexact-milliseconds))
   (define tdelay (with-continuation-mark
                      break-enabled-key
                    (make-thread-cell #f #t)
                    (thread (lambda ()
                              (sleep 0.1)
                              (with-continuation-mark
                                  break-enabled-key
                                (make-thread-cell #t #t)
                                (begin
                                  ;(check-for-break)
                                  (let loop () (loop))))))))
   (break-thread tdelay)
   (check tdelay (sync tdelay))
   (printf "[That break was from a thread, and it's expected]\n")
   (check #t (>= (current-inexact-milliseconds) (+ now3 0.1)))

   (define got-here? #f)
   (define break-self (thread (lambda ()
                                (unsafe-start-atomic)
                                (break-thread (current-thread))
                                (unsafe-end-atomic)
                                (set! got-here? #t))))
   (check break-self (sync break-self))
   (printf "[That break was from a thread, and it's expected]\n")
   (check #f got-here?)
   
   (define break-self-immediate (thread (lambda ()
                                          (dynamic-wind
                                              void
                                              (lambda ()
                                                (unsafe-start-breakable-atomic)
                                                (break-thread (current-thread))
                                                (set! got-here? #t))
                                              (lambda ()
                                                (unsafe-end-atomic))))))
   (check break-self-immediate (sync break-self-immediate))
   (printf "[That break was from a thread, and it's expected]\n")
   (check #f got-here?)
   
   ;; Make sure breaks are disabled in a `dynamic-wind` post thunk
   (define dw-s (make-semaphore))
   (define dw-pre? #f)
   (define dw-body? #f)
   (define dw-post? #f)
   (define tdw (thread
                (lambda ()
                  (dynamic-wind
                   (lambda () (semaphore-wait dw-s) (set! dw-pre? #t))
                   (lambda () (set! dw-body? #f))
                   (lambda () (set! dw-post? #t))))))
   (sync (system-idle-evt))
   (check #f dw-pre?)
   (break-thread tdw)
   (check #f dw-pre?)
   (semaphore-post dw-s)
   (sync tdw)
   (check #t dw-pre?)
   (check #f dw-body?)
   (check #t dw-post?)

   ;; Make sure `equal?`-based hash tables are thread-safe
   (let* ([ht (make-hash)]
          [s (make-semaphore)]
          [compare-ok (semaphore-peek-evt s)]
          [trying 0]
          [result #f])
     (define-values (struct:posn make-posn posn? posn-ref posn-set!)
       (make-struct-type 'posn #f 2 0 #f (list (cons prop:equal+hash
                                                     (list
                                                      (lambda (a b eql?)
                                                        (set! trying (add1 trying))
                                                        (sync compare-ok)
                                                        #t)
                                                      (lambda (a hc) 0)
                                                      (lambda (a hc) 0))))))
     (hash-set! ht (make-posn 1 2) 11)
     (thread (lambda ()
               (set! result (hash-ref ht (make-posn 1 2) #f))))
     (sync (system-idle-evt))
     (check #f result)
     (check 1 trying)
     (thread (lambda ()
               ;; Should get stuck before calling the `posn` equality function:
               (set! result (hash-ref ht (make-posn 1 2) #f))))
     (check #f result)
     (check 1 trying) ; since the second thread is waiting for the table
     (semaphore-post s)
     (sync (system-idle-evt))
     (check 11 result)
     (sync (system-idle-evt))
     (check 2 trying)) ; second thread should have completed

   (let* ([place-symbols (make-hasheq)]
          [register-place-symbol!
           (lambda (sym proc)
             (hash-set! place-symbols sym proc))])
     (set-start-place!
      (lambda (pch mod sym in out err cust plumber)
        (lambda ()
          ((hash-ref place-symbols sym) pch))))

     (register-place-symbol! 'nothing void)
     (let-values ([(pl1 in1 out1 err1) (dynamic-place 'dummy 'nothing #f #f #f)])
       (check #t (place? pl1))
       (check 0 (place-wait pl1)))

     (register-place-symbol! 'exit1 (lambda (pch) (exit 1)))
     (let-values ([(pl2 in2 out2 err2) (dynamic-place 'dummy 'exit1 #f #f #f)])
       (check #t (place? pl2))
       (check 1 (place-wait pl2)))

     (register-place-symbol! 'loop (lambda (pch) (let loop () (loop))))
     (let-values ([(pl3 in3 out3 err3) (dynamic-place 'dummy 'loop #f #f #f)])
       (check #t (place? pl3))
       (place-break pl3)
       (check 1 (place-wait pl3))
       (printf "[That break was from a place, and it's expected]\n"))

     (let-values ([(pl4 in4 out4 err4) (dynamic-place 'dummy 'loop #f #f #f)])
       (check #f (sync/timeout 0.01 (place-dead-evt pl4)))
       (place-kill pl4)
       (check 1 (place-wait pl4))
       (check #t (evt? (sync (place-dead-evt pl4))))
       (check #t (evt? (sync/timeout 0.01 (place-dead-evt pl4))))))

   ;; Measure thread quantum:
   #;
   (let ([t1 (thread (lambda () (let loop () (loop))))]
         [t2 (thread (lambda () (let loop ()
                             (define n (current-inexact-milliseconds))
                             (sleep)
                             (fprintf (current-error-port) "~a\n" (- (current-inexact-milliseconds) n))
                             (loop))))])
     (sleep 0.5)
     (break-thread t1)
     (break-thread t2))
   
   (time
    (let ([s1 (make-semaphore)]
          [s2 (make-semaphore)])
      (let ([ping
             (lambda (s1 s2)
               (let loop ([n 1000000])
                 (if (zero? n)
                     'done
                     (begin
                       (semaphore-post s1)
                       (semaphore-wait s2)
                       (loop (sub1 n))))))])
        (let ([t1 (thread (lambda () (ping s1 s2)))]
              [t2 (thread (lambda () (ping s2 s1)))])
          (thread-wait t1)
          (thread-wait t2)))))
   
   (set! done? #t)))

(unless done?
  (error 'thread-demo "something went wrong; deadlock?"))
