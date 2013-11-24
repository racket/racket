

(load-relative "loadtest.rktl")

(Section 'synchronization)

(define SYNC-SLEEP-DELAY 0.025)
(define SYNC-BUSY-DELAY 0.1) ; go a little slower to check busy waits

;; ----------------------------------------
;;  Semaphore peeks

(let* ([s (make-semaphore)]
       [p (semaphore-peek-evt s)]
       [ch (make-channel)])
  (test #t semaphore? s)
  (test #t semaphore-peek-evt? p)
  (test #f semaphore? p)
  (test #f semaphore-peek-evt? s)
  (test #f sync/timeout 0 s p)
  (test #f sync/timeout 0 s p)
  (test 'nope sync/timeout (lambda () 'nope) s p)
  (semaphore-post s)
  (test p sync/timeout 0 p)
  (test p sync p)
  (test s sync s)
  (test #f sync/timeout 0 p)
  (thread (lambda () (sync (system-idle-evt)) (semaphore-post s)))
  (test p sync p)
  (test p sync p)
  (test p sync/timeout (lambda () 'nope) p)
  (test s sync s)
  (test #f sync/timeout 0 p)
  (thread (lambda () (sync/timeout 0 p) (channel-put ch 7)))
  (thread (lambda () (sync/timeout 0 p) (channel-put ch 7)))
  (thread (lambda () (sync/timeout 0 p) (channel-put ch 7)))
  (semaphore-post s)
  (test 7 channel-get ch)
  (test 7 channel-get ch)
  (test 7 channel-get ch)
  (test #f channel-try-get ch)
  (thread (lambda () (channel-put ch 9)))
  (sync (system-idle-evt))
  (test 9 channel-try-get ch)
  (test #f channel-try-get ch))

(arity-test semaphore-peek-evt 1 1)
(err/rt-test (semaphore-peek-evt #f))
(err/rt-test (semaphore-peek-evt (semaphore-peek-evt (make-semaphore))))

;; ----------------------------------------
;; Channels

(arity-test make-channel 0 0)

(let ([c (make-channel)]
      [v 'nope])
  (test #f sync/timeout 0 c)
  (thread (lambda () (sync (system-idle-evt)) (set! v (channel-get c))))
  (test (void) channel-put c 10)
  (sync (system-idle-evt))
  (test 10 'thread-v v)
  (thread (lambda () (sync (system-idle-evt)) (channel-put c 11)))
  (test #f sync/timeout 0 c)
  (test 11 sync c)
  (let ([p (channel-put-evt c 45)])
    (test #t channel-put-evt? p)
    (test #f channel-put-evt? c)
    (thread (lambda () (sync (system-idle-evt)) (set! v (sync c))))
    (test #f sync/timeout 0 p)
    (test p sync p)
    (test #f sync/timeout 0 p)
    (sync (system-idle-evt))
    (test 45 'thread-v v))
  ;;;;; Make sure break/kill before action => break/kill only
  ;; get:
  (let ([try (lambda (break-thread)
	       (let ([t (thread (lambda ()
				  (set! v (channel-get c))))])
		 (test #t thread-running? t)
		 (sync (system-idle-evt))
		 (test #t thread-running? t)
		 (test (void) break-thread t)
		 (test #f sync/timeout 0 (channel-put-evt c 32))
                 (sync (system-idle-evt))
		 (test #f thread-running? t)
		 (test 45 'old-v v)))])
    (try break-thread)
    (try kill-thread))
  ;; put:
  (let ([try (lambda (break-thread)
	       (let ([t (thread (lambda () (channel-put c 17)))])
		 (test #t thread-running? t)
		 (sync (system-idle-evt))
		 (test #t thread-running? t)
		 (test (void) break-thread t)
		 (test #f sync/timeout 0 c)
		 (sync (system-idle-evt))
		 (test #f thread-running? t)))])
    (try break-thread)
    (try kill-thread))
  ;; put in main thread:
  (let ([t (current-thread)])
    (thread (lambda () 
	      (sync (system-idle-evt))
	      (break-thread t) 
	      (set! v (channel-get c)))))
  (test 77
	'broken
	(with-handlers ([exn:break? (lambda (x) 77)])
	  (sync (channel-put-evt c 32))))
  (test 45 'old-v v)
  (channel-put c 89)
  (sleep)
  (test 89 'new-v v)
  ;; get in main thread:
  (let ([t (current-thread)])
    (thread (lambda () 
	      (sync (system-idle-evt))
	      (break-thread t) 
	      (channel-put c 66))))
  (test 99
	'broken
	(with-handlers ([exn:break? (lambda (x) 99)])
	  (sync c)))
  (test 66 sync/timeout 0 c)

  ;;; Can't sync with self!
  (test #f sync/timeout 0 c (channel-put-evt c 100))
  ;; Test cross sync:
  (let ([c2 (make-channel)]
	[ok-result? (lambda (r)
		      (or (eq? r 100) (evt? r)))])
    (thread (lambda () (channel-put c2 (sync c (channel-put-evt c 100)))))
    (thread (lambda () (channel-put c2 (sync c (channel-put-evt c 100)))))
    (test #t ok-result? (channel-get c2))
    (test #t ok-result? (channel-get c2))))

;; ----------------------------------------
;; Alarms

(test #f sync/timeout 0.1 (alarm-evt (+ (current-inexact-milliseconds) 200)))
(test 'ok sync/timeout 0.1 
      (wrap-evt
       (alarm-evt (+ (current-inexact-milliseconds) 50))
       (lambda (x) 'ok)))
(test 'ok sync/timeout 100
      (wrap-evt
       (alarm-evt (+ (current-inexact-milliseconds) 50))
       (lambda (x) 'ok)))

;; ----------------------------------------
;; Waitable sets

(err/rt-test (choice-evt 7))
(err/rt-test (choice-evt (make-semaphore) 7))

(arity-test choice-evt 0 -1)

(test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt))
(test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt) (choice-evt))
(test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt (choice-evt) (choice-evt)))

(let ([s1 (make-semaphore)]
      [s2 (make-semaphore)]
      [s3 (make-semaphore)])
  (test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 s2 s3))
  (semaphore-post s2)
  (test s2 sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 s2 s3))
  (test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 s2 s3))
  (let ([set (choice-evt s1 s2 s3)])
    (test #f sync/timeout SYNC-SLEEP-DELAY set)
    (semaphore-post s2)
    (test s2 sync/timeout SYNC-SLEEP-DELAY set)
    (test #f sync/timeout SYNC-SLEEP-DELAY set))
  (thread (lambda () (sleep) (semaphore-post s3)))
  (test s3 sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 s2 s3))
  (test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 s2 s3))
  (semaphore-post s3)
  (test s3 sync/timeout SYNC-SLEEP-DELAY s1 (choice-evt s2 s3))
  (test #f sync/timeout SYNC-SLEEP-DELAY s1 (choice-evt s2 s3))
  (semaphore-post s3)
  (test s3 sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 s2) s3)
  (test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 s2) s3)
  (let ([set (choice-evt s1 s2)])
    (test #f sync/timeout SYNC-SLEEP-DELAY s1 set s3)
    (semaphore-post s2)
    (test s2 sync/timeout SYNC-SLEEP-DELAY set s3)
    (test #f sync/timeout SYNC-SLEEP-DELAY set s3))
  (test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 (choice-evt s2 s3)))
  (semaphore-post s3)
  (test s3 sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 (choice-evt s2 s3)))
  (test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt s1 (choice-evt s2 s3)))
  (semaphore-post s3)
  (test s3 sync/timeout SYNC-SLEEP-DELAY (choice-evt (choice-evt s1 s2) s3))
  (test #f sync/timeout SYNC-SLEEP-DELAY (choice-evt (choice-evt s1 s2) s3))
  (let ([set (choice-evt s1 (choice-evt s2 s3))])
    (test #f sync/timeout SYNC-SLEEP-DELAY set)
    (semaphore-post s3)
    (test s3 sync/timeout SYNC-SLEEP-DELAY set)
    (test #f sync/timeout SYNC-SLEEP-DELAY set))
  
  (let* ([c (make-channel)]
	 [set (choice-evt s1 s2 c)])
    (test #f sync/timeout SYNC-SLEEP-DELAY set)
    (thread (lambda () (channel-put c 12)))
    (test 12 sync/timeout SYNC-SLEEP-DELAY set)
    (test #f sync/timeout SYNC-SLEEP-DELAY set)
    (let* ([p (channel-put-evt c 85)]
	   [set (choice-evt s1 s2 p)])
      (test #f sync/timeout SYNC-SLEEP-DELAY set)
      (thread (lambda () (channel-get c)))
      (test p sync/timeout SYNC-SLEEP-DELAY set)
      (test #f sync/timeout SYNC-SLEEP-DELAY set))))

(test 77 sync/timeout 
      #f 
      (wrap-evt (make-semaphore) void) 
      (guard-evt 
       (lambda () 
	 (choice-evt 
	  (make-semaphore) (make-semaphore) (make-semaphore) (make-semaphore) 
	  (make-semaphore) (make-semaphore) (make-semaphore) (make-semaphore) 
	  (let ([sema (make-semaphore 1)])
	    (wrap-evt sema (lambda (x)
                             (test sema values x)
                             77)))))))

;; More alarms:
(let ([make-delay
       (lambda (amt)
	 (guard-evt
	  (lambda ()
	    (wrap-evt
	     (alarm-evt (+ (current-inexact-milliseconds) (* 1000 amt)))
	     (lambda (v) amt)))))])
  (test #f sync/timeout 0.1 (make-delay 0.15) (make-delay 0.2))
  (test 0.15 sync/timeout 18 (make-delay 0.15) (make-delay 0.2))
  (test 0.15 sync/timeout 18 (make-delay 0.2) (make-delay 0.15))
  (test 0.15 sync/timeout 0.18 (make-delay 0.15) (make-delay 0.2))
  (test 0.15 sync/timeout 18 
	(choice-evt (make-delay 0.2) (make-delay 0.15))))

;;check flattening of choice evts returned by a guard:
(let ()
  (define s1 (make-semaphore))
  (define s2 (make-semaphore))
  (define s3 (make-semaphore))
  (define s4 (make-semaphore))
  
  (define evt1 (choice-evt s1 s2))
  (define evt2 (choice-evt s3 s4))

  (thread (lambda () 
            (sync (system-idle-evt))
            (semaphore-post
             (list-ref (list s1 s2 s3 s4)
                       (random 4)))))

  (test #t
        semaphore?
        (sync (guard-evt
               (lambda ()
                 (choice-evt evt1 evt2))))))

;; ----------------------------------------
;; Wrapped waitables

(arity-test wrap-evt 2 2)

(err/rt-test (wrap-evt 1 void))
(err/rt-test (wrap-evt (make-semaphore) 10))

(test 17 sync (wrap-evt (make-semaphore 1) (lambda (sema) 17)))
(test 17 sync (choice-evt
               (make-semaphore)
               (wrap-evt (make-semaphore 1) (lambda (sema) 17))))
(test #t sync (wrap-evt (make-semaphore 1) semaphore?))
(test 18 'sync
      (let ([n 17]
	    [s (make-semaphore)])
	(thread (lambda () (sync (system-idle-evt)) (semaphore-post s)))
	(sync 
         (wrap-evt 
          s 
          (lambda (sema) (set! n (add1 n)) n))
         (wrap-evt 
          s 
          (lambda (sema) (set! n (add1 n)) n)))))

(let ([c (make-channel)])
  (thread (lambda () (channel-put c 76)))
  (test 77 sync (wrap-evt c add1)))

(test 78 sync 
      (wrap-evt (choice-evt (make-semaphore 1) (make-semaphore 1))
			     (lambda (x) 78)))

(test-values '() (lambda () (sync (wrap-evt always-evt (lambda (x) (values))))))
(test-values '(1 2) (lambda () (sync (wrap-evt always-evt (lambda (x) (values 1 2))))))
(test-values '(1 2 3) (lambda () (sync (wrap-evt (wrap-evt always-evt
                                                           (lambda (_) (values 1 2)))
                                                 (lambda (a b) (values a b 3))))))

(err/rt-test (sync (wrap-evt always-evt (lambda () #f))))
(err/rt-test (sync (wrap-evt always-evt (lambda (a b) #f))))

;; ----------------------------------------
;; handle evt

(test 10 sync (handle-evt always-evt (lambda (x) 10)))
(test 11 sync (handle-evt (wrap-evt always-evt (lambda (x) 10)) add1))
(test-values '(1 2) (lambda () (sync (handle-evt always-evt (lambda (x) (values 1 2))))))
(test-values '(1 2 3) (lambda () (sync (handle-evt (wrap-evt always-evt
                                                             (lambda (_) (values 1 2)))
                                                   (lambda (a b) (values a b 3))))))
(err/rt-test (sync (handle-evt always-evt (lambda () #f))))
(err/rt-test (sync (handle-evt always-evt (lambda (a b) #f))))

;; check tail call via loop:
(test 'ok sync (let loop ([n 1000000])
                 (if (zero? n)
                     (handle-evt always-evt (lambda (x) 'ok))
                     (sync
                      (handle-evt always-evt (lambda (x) (loop (sub1 n))))))))

;; cac wrap a `handle-evt' with a wrap or another handle, although it
;; defeats tail behavior of the `handle-evt'.
(test (list (void)) sync (handle-evt (handle-evt always-evt void) list))
(test (box (void)) sync (wrap-evt (handle-evt always-evt void) box))
(test (vector (void)) sync (handle-evt (choice-evt (handle-evt always-evt void) never-evt) vector))
(test (void) sync (chaperone-evt (handle-evt always-evt void) (lambda (x) (values x values))))

;; can handle a wrap evt:
(test #t evt? (handle-evt (wrap-evt always-evt void) void))
(test #t evt? (handle-evt (choice-evt (wrap-evt always-evt void)
                                      (wrap-evt never-evt void))
                          void))

(test #t handle-evt? (handle-evt always-evt void))
(test #t handle-evt? (choice-evt (wrap-evt always-evt void) (handle-evt always-evt void)))
(test #f handle-evt? (wrap-evt always-evt void))
(test #f handle-evt? (choice-evt (wrap-evt always-evt void) (wrap-evt always-evt void)))

;; ----------------------------------------
;; Nack waitables

(arity-test nack-guard-evt 1 1)
(arity-test guard-evt 1 1)

(err/rt-test (nack-guard-evt 10))
(err/rt-test (nack-guard-evt (lambda () 10)))
(err/rt-test (guard-evt 10))
(err/rt-test (guard-evt (lambda (x) 10)))

(let ([s (make-semaphore 1)]
      [nack-try-wait? (lambda (n)
                        (unless (evt? n)
                          (error "NACK isn't ready for try-wait"))
			(let ([v (sync/timeout 0 n)])
			  (when v
			    (test #t void? v)
			    (test (void) sync n))
			  (and v #t)))])
  (test s sync (nack-guard-evt (lambda (nack) s)))
  (test #f semaphore-try-wait? s)
  (semaphore-post s)
  (let ([v #f])
    (test #f sync/timeout 0
	  (nack-guard-evt (lambda (nack) 
                            (set! v nack)
                            (make-semaphore))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test #f sync/timeout SYNC-SLEEP-DELAY
	  (nack-guard-evt (lambda (nack) 
                            (set! v nack)
                            (make-semaphore))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test #f sync/timeout 0
	  (nack-guard-evt (lambda (nack) 
                            (set! v nack)
                            (make-semaphore)))
	  (nack-guard-evt (lambda (nack) 
                            (set! v nack)
                            (make-semaphore))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test #f sync/timeout SYNC-SLEEP-DELAY
	  (nack-guard-evt (lambda (nack) 
                            (set! v nack)
                            (make-semaphore)))
	  (nack-guard-evt (lambda (nack) 
                            (set! v nack)
                            (make-semaphore))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test #f sync/timeout SYNC-SLEEP-DELAY
	  (choice-evt 
	   (nack-guard-evt (lambda (nack) 
                             (set! v nack)
                             (make-semaphore)))
	   (nack-guard-evt (lambda (nack) 
                             (set! v nack)
                             (make-semaphore)))))
    (test #t nack-try-wait? v)
    (set! v #f)
    (test s sync/timeout 0
	  (nack-guard-evt (lambda (nack) 
                            (set! v nack)
                            s)))
    (test #f nack-try-wait? v) ; ... but not an exception!
    (semaphore-post s)
    (set! v #f)
    (let loop ()
      (test s sync/timeout 0
	    (nack-guard-evt (lambda (nack) 
                              (set! v nack)
                              (make-semaphore)))
	    s)
      (if v
	  (test #t nack-try-wait? v)
	  (begin  ; tried the 2nd first, so do test again
	    (semaphore-post s)
	    (loop))))
    (set! v #f)
    (let loop ()
      (err/rt-test (sync/timeout 0
				 (nack-guard-evt (lambda (nack) 
						   (set! v nack)
						   (make-semaphore)))
				 (nack-guard-evt (lambda (nack)
						   (/ 1 0))))
		   exn:fail:contract:divide-by-zero?)
      (if v
	  (test #t nack-try-wait? v)
	  (loop)))
    (set! v #f)
    (let loop ()
      (err/rt-test (sync/timeout 0
				 (nack-guard-evt (lambda (nack)
						   (/ 10 0)))
				 (nack-guard-evt (lambda (nack) 
						   (set! v nack)
						   (make-semaphore))))
		   exn:fail:contract:divide-by-zero?)
      (if v
	  (begin
	    (set! v #f)
	    (loop))
	  (test #t not v)))
    (set! v null)
    (test #f sync/timeout 0
	  (nack-guard-evt (lambda (nack) 
			    (set! v (cons nack v))
			    (make-semaphore)))
	  (nack-guard-evt (lambda (nack) 
			    (set! v (cons nack v))
			    (make-semaphore))))
    (test '(#t #t) map nack-try-wait? v)

    ;; Check that thread kill also implies nack:
    (set! v #f)
    (let* ([ready (make-semaphore)]
	   [t (thread (lambda ()
			(sync/timeout 
			 #f
			 (nack-guard-evt
			  (lambda (nack)
			    (set! v nack)
			    (semaphore-post ready)
			    (make-semaphore))))))])
      (semaphore-wait ready)
      (kill-thread t)
      (test #t nack-try-wait? v))))
		       

(let ([s (make-semaphore 1)])
  (test s sync/timeout 0 (guard-evt (lambda () s))))

(let ([v #f])
  (test #f sync/timeout 0
	(nack-guard-evt
	 (lambda (nack)
	   (set! v nack)
	   (choice-evt (make-semaphore) (make-semaphore)))))
  (unless (evt? v) (error "the NACK isn't ready!"))
  (test (void) sync/timeout 0 v))

(let ([ch (make-channel)]
      [n #f])
  (let ([t (thread
	    (lambda ()
	      (sync
	       (nack-guard-evt
		(lambda (nack)
		  (set! n nack)
		  never-evt))
	       (channel-put-evt ch 10))))])
    (sync (system-idle-evt))
    (test 10 channel-get ch)
    (test (void) sync/timeout 0 n)))
	       
(let ()
  (define ok? 'not-ready)
  (thread-wait
   (thread
    (lambda ()
      (sync (nack-guard-evt 
             (lambda (nack)
               (thread (lambda () 
                         (sync nack) 
                         (set! ok? #f)))
               (sync (system-idle-evt))
               always-evt))))))
  (sync (system-idle-evt))
  (test 'not-ready values ok?))

;; ----------------------------------------
;; Poll waitables

(arity-test poll-guard-evt 1 1)

(err/rt-test (poll-guard-evt 10))
(err/rt-test (poll-guard-evt (lambda () 10)))

(let ([s (semaphore-peek-evt (make-semaphore 1))])
  (test s sync/timeout 0 (poll-guard-evt (lambda (poll?)
                                           (test #t values poll?)
                                           s)))
  (test s sync (poll-guard-evt (lambda (poll?)
                                 (test #f values poll?)
                                 s)))
  (test s sync/timeout 0 (choice-evt
                          (poll-guard-evt (lambda (poll?)
                                            (test #t values poll?)
                                            s))
                          (make-semaphore)))
  (test s sync (choice-evt
                (poll-guard-evt (lambda (poll?)
                                  (test #f values poll?)
                                  s))
                (make-semaphore))))

;; ----------------------------------------
;; Structures as waitables

;; Bad property value:
(err/rt-test (make-struct-type 'wt #f 2 0 #f (list (cons prop:evt -1))) exn:application:mismatch?)
;; slot index 1 not immutable:
(err/rt-test (make-struct-type 'wt #f 2 0 #f (list (cons prop:evt 1))) exn:application:mismatch?)

(define-values (struct:wt make-wt wt? wt-ref wt-set!)
  (make-struct-type 'wt #f 2 0 #f (list (cons prop:evt 1)) (make-inspector) #f '(1)))

(define-values (struct:wt2 make-wt2 wt2? wt2-ref wt2-set!)
  (make-struct-type 'wt2 #f 2 0 #f (list (cons prop:evt 1))
                    (make-inspector) 0 '(1)))

(let ([test-wt
       (lambda (make-wt)
         (let ([always-ready (make-wt (lambda () 10) (lambda (self) #t))]
               [always-stuck (make-wt 1 2)])
           (test always-ready sync always-ready)
           (test always-ready sync/timeout 0 always-ready)
           (test #f sync/timeout 0 always-stuck)
           (test #f sync/timeout SYNC-SLEEP-DELAY always-stuck)))])
  (test-wt make-wt)
  (test-wt make-wt2))

;; Test with multiple values
(let ([wt-v  (make-wt #f (lambda (_) (wrap-evt always-evt (lambda (_) (values 1 2)))))]
      [wt-fail (make-wt #f (lambda (_) (wrap-evt always-evt (lambda () #f))))])
  (test-values '(1 2) (lambda () (sync wt-v)))
  (err/rt-test (sync wt-fail)))

;; Check whether something that takes at least SYNC-SLEEP-DELAY
;;  seconds in fact takes roughly that much CPU time. We
;;  expect non-busy-wait takes to take a very small fraction
;;  of the time.
;; This test only works well if there are no other
;;  threads running and the underlying OS is not loaded.
(define (check-busy-wait go busy?)
  (collect-garbage) ; reduces false-positives in detecting busy wait
  (let ([msecs (current-process-milliseconds)]
	[gc-msecs (current-gc-milliseconds)]
	[real-msecs (current-milliseconds)])
    (go)
    (let ([took (/ (abs (- (current-process-milliseconds) msecs
			   (abs (- (current-gc-milliseconds) gc-msecs))))
		   1000.0)]
	  [real-took (/ (abs (- (current-milliseconds) real-msecs)) 1000.0)]
	  [boundary (/ SYNC-BUSY-DELAY 6)])
      ;; Hack.
      ;; The following test isn't reliable, so only Matthew should see it,
      ;; and only in non-parallel mode:
      (when (and (regexp-match #rx"(mflatt)|(matthewf)" (path->string (find-system-path 'home-dir)))
                 (equal? "" Section-prefix))
	(test busy? (lambda (a ax b c d) (> b c)) 'busy-wait? go took boundary real-took)))))

(define (test-good-waitable wrap-sema make-wt)
  (let ([sema (make-semaphore)])
    (letrec-values ([(sema-ready-part get-sema-result) (wrap-sema sema sema (lambda () sema-ready))]
		    [(sema-ready) (make-wt 1 sema-ready-part)])
      (test #f 'initial-sema (sync/timeout 0 sema-ready))
      (semaphore-post sema)
      (test (get-sema-result) sync/timeout 0 sema-ready)
      (test #f semaphore-try-wait? sema)
      (test #f sync/timeout 0 sema-ready)
      (semaphore-post sema)
      (let ()
	(define (non-busy-wait waitable get-result)
          (begin
            (thread (lambda ()
                      (sync (system-idle-evt))
                      (semaphore-post sema)))
            (test (get-result) sync waitable))
	  (test #f sync/timeout 0 waitable)
	  (semaphore-post sema)
	  (test (get-result) sync waitable)
	  (test #f sync/timeout 0 waitable)
	  (semaphore-post sema)
	  (test (get-result) sync waitable)
	  (test #f semaphore-try-wait? sema)
	  (test #f sync/timeout 0 waitable))
	(non-busy-wait sema-ready get-sema-result)
	(semaphore-post sema)
	(letrec-values ([(wrapped-part get-wrapped-result)
			 (wrap-sema (make-wt 2 (lambda (self) sema-ready))
				    (get-sema-result)
				    (lambda () sema-ready))]
			[(wrapped) (make-wt 3 wrapped-part)])
	  (non-busy-wait (get-wrapped-result) get-wrapped-result))))))

(map
 (lambda (make-wt)
   (test-good-waitable (lambda (x x-result get-self)
                         (values x (lambda () x-result)))
                       make-wt)
   (test-good-waitable (lambda (x x-result get-self)
                         (let ([ws (choice-evt
                                    x
                                    (make-wt 99 (lambda (self) (make-semaphore))))])
                           (values ws (lambda () x-result))))
                       make-wt))
 (list make-wt make-wt2))

(check-busy-wait
 (letrec ([s (make-semaphore)]
	  [wt (make-wt 1 (lambda (self) (unless (or (eq? wt s)
						    (eq? self wt) )
					  (error 'wt "yikes: ~s != ~s" self wt)) 
				 wt))])
   (thread (lambda () (sleep (/ SYNC-BUSY-DELAY 2)) (set! wt s)))
   (lambda ()
     (test #f sync/timeout SYNC-BUSY-DELAY wt)))
 #t)

;; ----------------------------------------

(define (test-stuck-port ready-waitable make-waitable-unready make-waitable-ready)
  (let* ([go? #f]
	 [bad-stuck-port (make-input-port
			  'name
			  (lambda (str)
			    (if go?
				(begin
				  (bytes-set! str 0 (char->integer #\x))
				  1)
				(if (zero? (random 2))
				    0
				    ready-waitable)))
			  #f
			  void)])
    (make-waitable-unready ready-waitable)
    (test #f char-ready? bad-stuck-port)
    (test #f sync/timeout SYNC-SLEEP-DELAY bad-stuck-port)
    (test 0 read-bytes-avail!* (make-bytes 10) bad-stuck-port)
    (set! go? #t)
    (test #t char-ready? bad-stuck-port)
    (test bad-stuck-port sync/timeout SYNC-SLEEP-DELAY bad-stuck-port)
    (test #t positive? (read-bytes-avail!* (make-bytes 10) bad-stuck-port))
    (set! go? #f)
    (test #f char-ready? bad-stuck-port)
    (test #f sync/timeout SYNC-SLEEP-DELAY bad-stuck-port)
    (test 0 read-bytes-avail!* (make-bytes 10) bad-stuck-port)
    (set! ready-waitable 0)
    (test #f sync/timeout 0 bad-stuck-port)
    (test #f sync/timeout 0 bad-stuck-port)
    (check-busy-wait
     (lambda ()
       (test #f sync/timeout SYNC-BUSY-DELAY bad-stuck-port))
     #t)
    (check-busy-wait
     (lambda ()
       (thread (lambda ()
		 (sleep SYNC-BUSY-DELAY)
		 (set! go? #t)))
       (test bad-stuck-port sync bad-stuck-port))
     #t)))

(map
 (lambda (make-wt)
   (test-stuck-port (make-semaphore 1) semaphore-try-wait? semaphore-post)
   (let ([ready? #t])
     (test-stuck-port (make-wt 77 (lambda (self)
                                    (if ready?
                                        #t
                                        (make-semaphore))))
                      (lambda (wt) (set! ready? #f))
                      (lambda (wt) (set! ready? #t))))
   (let ([s (make-semaphore 1)])
     (test-stuck-port (make-wt 77 s)
                      (lambda (wt) (semaphore-try-wait? s))
                      (lambda (wt) (semaphore-post s))))
   (let ([s (make-semaphore 1)])
     (test-stuck-port (make-wt 177 (lambda (self) s))
                      (lambda (wt) (semaphore-try-wait? s))
                      (lambda (wt) (semaphore-post s)))))
 (list make-wt make-wt2))

;; ----------------------------------------

;; In the current implemenation, a depth of 10 for 
;;  waitable chains is a magic number; it causes the scheduler to
;;  swap a thread in to check whether it can run, instead of
;;  checking in the thread. (For a well-behaved chain, this 
;;  swap in will lead to a more friendly semaphore wait, for
;;  example.)

(letrec ([stack-em (lambda (n s)
		     ;; This needs to be tail-recursive to
		     ;; the find-depth check below (to check
		     ;; blocking depth, not precdure depth)
		     (if (zero? n)
			 s
			 (stack-em (sub1 n) (make-wt n s))))])
  (let* ([s1 (make-semaphore 1)]
	 [s20 (make-semaphore 1)]
	 [wt1 (stack-em 1 s1)]
	 [wt20 (stack-em 20 s20)])
    (test s1 sync/timeout 0 wt1)
    (test s20 sync/timeout 0 wt20)
    (test #f semaphore-try-wait? s1)
    (test #f semaphore-try-wait? s20)
    (let ([t20
	   (thread (lambda ()
		     (test s20 sync/timeout 1.0 wt20)))])
      (let loop ([n 20])
	(unless (zero? n)
	  (sleep)
	  (loop (sub1 n))))
      (semaphore-post s20)
      (test (void) thread-wait t20))))


;; ----------------------------------------
;;  Thread suspend, resume, and dead waitables

(let ([d (thread-dead-evt (thread void))])
  (test d sync d))

(let* ([sema (make-semaphore)]
       [t (thread (lambda () (semaphore-wait sema)))]
       [r (thread-resume-evt t)]
       [s (thread-suspend-evt t)])
  (test #f sync/timeout 0 t s)
  (test t sync/timeout 0 t s r)
  (test t sync/timeout 0 r)
  (thread-suspend t)
  (test t sync/timeout 0 r)
  (test t sync/timeout 0 s)
  (let* ([r (thread-resume-evt t)]
	 [s (thread-suspend-evt t)])
    (test #f sync/timeout 0 t r)
    (test t sync/timeout 0 t s r)
    (test t sync/timeout 0 s)
    (thread-resume t)
    (test t sync/timeout 0 s)
    (test t sync/timeout 0 r)
    (let* ([s (thread-suspend-evt t)])
      (thread (lambda () (sync (system-idle-evt)) (thread-suspend t)))
      (test #f sync/timeout 0 s)
      (test t sync s)
      (let* ([r (thread-resume-evt t)]
	     [d (thread-dead-evt t)])
	(thread (lambda () (sync (system-idle-evt)) (thread-resume t)))
	(test #f sync/timeout 0 r)
	(test t sync r)

	(test #f sync/timeout 0 d)
	(semaphore-post sema)
	(test d sync d)
	(test t sync r)
	(test t sync s)
	(test #f sync/timeout 0 (thread-resume-evt t))
	(test #f sync/timeout 0 (thread-suspend-evt t))
	(test d thread-dead-evt t)))))

;; ----------------------------------------
;;  thread mbox

(test #f thread-try-receive)
(test #f sync/timeout 0 (thread-receive-evt))
(test (void) thread-send (current-thread) 10)
(let ([t (thread-receive-evt)])
  (test t sync/timeout 10 t))
(test 10 thread-try-receive)
(test #f thread-try-receive)
(let ([t (current-thread)])
  (thread (lambda ()
            (sync (system-idle-evt))
            (thread-send t 35))))
(test 35 thread-receive)
(test #f thread-try-receive)
(test (void) thread-rewind-receive '(1 2 3))
(test 3 thread-try-receive)
(test 2 thread-try-receive)
(test (void) thread-rewind-receive '(4))
(test 4 thread-try-receive)
(test 1 thread-try-receive)
(test #f thread-try-receive)
(test (void) thread-rewind-receive (vector->list (make-vector 500 'x)))
(let loop ([n 500])
  (unless (zero? n)
    (test 'x thread-try-receive)
    (loop (sub1 n))))
(test #f thread-try-receive)
(let* ([s #f]
       [t1 (let ([t (current-thread)])
             (thread (lambda ()
                       (set! s (thread-receive)))))])
  (sync (system-idle-evt))
  (thread-suspend t1)
  (thread-send t1 'apple)
  (sync (system-idle-evt))
  (test #f values s)
  (thread-resume t1)
  (sync (system-idle-evt))
  (test 'apple values s))
(let* ([s 0]
       [t (thread (lambda ()
                    (set! s (list (thread-receive)
                                  (thread-receive)
                                  (thread-receive)))))])
  (thread-send t 0)
  (thread-send t 1)
  (thread-send t 2)
  (sync (system-idle-evt))
  (test '(0 1 2) values s))
(let ([t (thread void)])
  (sync (system-idle-evt))
  (test 'z thread-send t 'x (lambda () 'z))
  (test-values '(a z) (lambda ()
                        (thread-send t 'x (lambda () (values 'a 'z)))))
  (err/rt-test (thread-send t 'x)))

;; make sure it's ok for rewind to be the first action:
(test (void) thread-wait (thread (lambda () (thread-rewind-receive '(1 2 3)))))

;; ----------------------------------------
;;  Garbage collection

(define (num-scheduled)
  (let ([v (make-vector 7)])
    (vector-set-performance-stats! v)
    (vector-ref v 6)))

(define (check-threads-gcable label blocking-thunk)
  ;; Actually, we approximate the gcable check as a num-scheduled check,
  ;;  even though there's still a lot of machinery here to try to check
  ;;  GCing. The explicit gc has been commented out.
  (define orig-scheduled (num-scheduled))
  (let ([l (let loop ([n 20][die? #f])
	     (if (zero? n)
		 null
		 (cons (make-weak-box (thread (if die? void blocking-thunk)))
		       (loop (if die? n (sub1 n)) (not die?)))))]
	[sl (lambda ()
	      (let loop ([n 20])
		(unless (zero? n) (sleep) (loop (sub1 n)))))]
	[ok-done? (lambda (r) 
                    (or (<= (list-ref r 3) orig-scheduled)
                        ;; If we're running parallel threads, 
                        ;; just give up on the comparison.
                        (not (equal? "" Section-prefix))))])
    (test #t
	  ok-done?
	  (let loop ([tries 0][n 100])
	    (if (or (= tries 3) (< n 10))
		(list tries n label (num-scheduled))
		(begin
		  (sl) 
		  ;; (collect-garbage)
		  (loop (add1 tries)
			(apply + (map (lambda (b) (if (weak-box-value b) 1 0)) l)))))))))

(check-threads-gcable 'sema (lambda () (semaphore-wait (make-semaphore))))
(define (check/combine c)
  (check-threads-gcable 'semaw (lambda () (sync (c (make-semaphore)))))
  (check-threads-gcable 'semap (lambda () (sync (c (semaphore-peek-evt (make-semaphore))))))
  (check-threads-gcable 'ch (lambda () (sync (c (make-channel)))))
  (check-threads-gcable 'chput (lambda () (sync (c (channel-put-evt (make-channel) 10)))))
  (check-threads-gcable 'wrapped (lambda () (sync (c (wrap-evt (make-semaphore) void)))))
  (check-threads-gcable 'guard (lambda () (sync (c (guard-evt (lambda () (make-semaphore)))))))
  (check-threads-gcable 'nack (lambda () (sync (c (nack-guard-evt (lambda (nack) (make-semaphore)))))))
  (check-threads-gcable 'poll (lambda () (sync (c (poll-guard-evt (lambda (poll?) (make-semaphore)))))))
  (check-threads-gcable 'never (lambda () (sync (c never-evt)))))
(check/combine values)
(check/combine (lambda (x) (choice-evt x (make-semaphore))))
(check/combine (lambda (x) (choice-evt (make-semaphore) x)))
(check/combine (lambda (x) (choice-evt (make-semaphore) x)))

(check-threads-gcable 'nested (lambda () (call-in-nested-thread (lambda () (semaphore-wait (make-semaphore))))))
(pseudo-random-generator? 10)
(check-threads-gcable 'suspended (lambda () (thread-suspend (current-thread))))
(check-threads-gcable 'nested-suspend (lambda () (call-in-nested-thread (lambda () (thread-suspend (current-thread))))))

(check-threads-gcable 'resume (lambda () (let ([t (thread (lambda () (sleep 10)))])
					   (thread-suspend t)
					   (sync (thread-resume-evt t)))))
(check-threads-gcable 'suspend (lambda () (let ([t (thread (lambda () (semaphore-wait (make-semaphore))))])
					    (sync (thread-suspend-evt t)))))
(check-threads-gcable 'suspend-self (lambda () (sync (thread-suspend-evt (current-thread)))))

;; ----------------------------------------
;;  Fairness in wait selection

(let ([try (lambda (t1 t2 r min max)
             (test #t 
                   < 
                   min
                   (let loop ([n 1000][r-n 0])
                     (if (zero? n)
                         r-n
                         (loop (sub1 n) (+ r-n
                                           (if (eq? r (sync t1 t2))
                                               1
                                               0)))))
                   max))])
  (let ([t1 (semaphore-peek-evt (make-semaphore 1))]
        [t2 (semaphore-peek-evt (make-semaphore 1))])
    (let-values ([(r w) (make-pipe)])
      (fprintf w "Hi!\n")
      ;; Between 20% and 80% is fair, and surely < 20% or > 80% is unlikely
      (try t1 t2 t1 200 800)
      (try t1 t2 t2 200 800)
      (try t1 w w 200 800)
      (try w t1 w 200 800)
      (try t1 (choice-evt t2 w) t1 100 500)
      (try t1 (choice-evt t2 w) w 100 500)
      (try (choice-evt t2 w) t1 w 100 500))))

;; ----------------------------------------
;;  No starvation, despite hack to increase throughput for
;;  semaphore-protected data structures:

(let ([s1 (make-semaphore)])
  (define t1
    (thread (lambda ()
	      (semaphore-wait s1)
	      (semaphore-post s1))))
  (let loop ()
    (sleep)
    (semaphore-post s1)
    (semaphore-wait s1)
    (when (thread-running? t1)
      (loop)))
  (test #t string? "No starvation - good!"))

(let ([s1 (make-semaphore)]
      [s2 (make-semaphore)])
  (define t1
    (thread (lambda ()
	      (semaphore-post (sync s1 s2)))))
  (define t2
    (thread (lambda ()
	      (semaphore-post (sync s1 s2)))))
  (let loop ()
    (sleep)
    (semaphore-post s1)
    (semaphore-wait s1)
    (semaphore-post s2)
    (semaphore-wait s2)
    (when (or (thread-running? t1)
	      (thread-running? t2))
      (loop)))
  (test #t string? "No starvation - good!"))

;; ----------------------------------------
;;  Breaks and dynamic-wind

(let ([s #f]
      [p #f]
      [/dev/null-for-err
       (make-output-port #f always-evt (lambda (s start end ? ??) (- end start)) void void)]
      [did-pre1 #f]
      [did-pre2 #f]
      [did-act1 #f]
      [did-act2 #f]
      [did-post1 #f]
      [did-post2 #f]
      [did-done #f]
      [break-on (lambda () (break-enabled #t))]
      [sw semaphore-wait])
  (let ([mk-t
	 (lambda (init ;; how to start
		  ;; functions that can capture the continuation:
		  capture-pre capture-act capture-post
		  ;; whether to start the thread with breaks off (imperatively)
		  break-off?
		  ;; things to do in pre, act, and post
		  pre-thunk act-thunk post-thunk
		  ;; sema-wait or sema-wait/enable-break:
		  pre-semaphore-wait act-semaphore-wait post-semaphore-wait)
	   ;; This reset function is called for a cptured continuation
	   ;;  to reset the effective arguments
	   (define (reset
		    -capture-pre -capture-act -capture-post
		    -break-off?
		    -pre-thunk -act-thunk -post-thunk
		    -pre-semaphore-wait -act-semaphore-wait -post-semaphore-wait)
	     (when -break-off?
	       (break-enabled #f))
	     (set! capture-pre -capture-pre)
	     (set! capture-act -capture-act)
	     (set! capture-post -capture-post)
	     (set! pre-thunk -pre-thunk)
	     (set! act-thunk -act-thunk)
	     (set! post-thunk -post-thunk)
	     (set! pre-semaphore-wait -pre-semaphore-wait)
	     (set! act-semaphore-wait -act-semaphore-wait)
	     (set! post-semaphore-wait -post-semaphore-wait))
	   ;; initially, thread hasn't gotten anywhere:
	   (set! did-pre1 #f) (set! did-pre2 #f)
	   (set! did-act1 #f) (set! did-act2 #f)
	   (set! did-post1 #f) (set! did-post2 #f)
	   (set! did-done #f)
	   (thread 
	    (lambda ()
	      (current-error-port /dev/null-for-err)
	      (when break-off?
		(break-enabled #f))
	      (init ;; init function gets to decide whether to do the normal body:
	       (lambda ()
           (printf "here ~s\n"  (procedure? capture-pre))
		 (dynamic-wind
		     (lambda ()
           (printf "here3 ~s\n" (procedure? capture-pre))
		       (capture-pre
			reset
			(lambda ()
           (printf "here4\n")
			  (set! did-pre1 #t)
			  (semaphore-post p)
			  (pre-thunk)
			  (pre-semaphore-wait s)
			  (set! did-pre2 #t))))
		     (lambda () 
           (printf "here2\n")
		       (capture-act
			reset
			(lambda ()
			  (set! did-act1 #t)
			  (semaphore-post p)
			  (act-thunk)
			  (act-semaphore-wait s)
			  (set! did-act2 #t))))
		     (lambda ()
		       (capture-post
			reset
			(lambda ()
			  (set! did-post1 #t)
			  (semaphore-post p)
			  (post-thunk)
			  (post-semaphore-wait s)
			  (set! did-post2 #t)))))
		 (set! did-done #t))))))])
    ;; `go' runs the tests, parameterized by when to break the other
    ;;  thread and when it should take effect in the other thread
    (define (go
	     mk-t* break-off?
	     pre-thunk act-thunk post-thunk
	     pre-semaphore-wait act-semaphore-wait post-semaphore-wait
	     try-pre-break 
	     should-pre-break?
	     should-preact-break?
	     try-act-break 
	     should-act-break?
	     try-post-break 
	     should-post-break?
	     should-done-break?)
      ;; print the state for this test:
      (test #t list? (list 'go 
			   pre-thunk act-thunk post-thunk
			   pre-semaphore-wait act-semaphore-wait post-semaphore-wait
			   try-pre-break 
			   should-pre-break?
			   should-preact-break?
			   try-act-break 
			   should-act-break?
			   try-post-break 
			   should-post-break?
			   should-done-break?))
      ;; create fresh semaphores
      (set! s (make-semaphore))
      (set! p (make-semaphore))
      (printf "mk ~s\n" mk-t*)
      ;; create the thread
      (let ([t (mk-t* break-off?
		      pre-thunk act-thunk post-thunk
		      pre-semaphore-wait act-semaphore-wait post-semaphore-wait)])
	(semaphore-wait p)
	(test #t 'pre1 did-pre1)
	(try-pre-break t)
	(semaphore-post s)
	(if should-pre-break?
	    (begin
	      (thread-wait t)
	      (test #f 'pre2 did-pre2))
	    (if should-preact-break?
		(begin
		  (semaphore-post s) ; for post
		  (thread-wait t)
		  (test #t 'pre2 did-pre2)
		  (test #f 'act1 did-act1))
		(begin
		  (semaphore-wait p)
		  (test #t 'pre2 did-pre2)
		  (test #t 'act1 did-act1)
		  (try-act-break t)
		  (semaphore-post s)
		  (if should-act-break?
		      (begin
			(semaphore-post s) ; for post
			(thread-wait t)
			(test #f 'act2 did-act2))
		      (begin
			(semaphore-wait p)
			(test #t 'act2 did-act2)
			(test #t 'post1 did-post1)
			(try-post-break t)
			(semaphore-post s)
			(if should-post-break?
			    (begin
			      (thread-wait t)
			      (test #f 'post2 did-post2))
			    (begin
			      (thread-wait t)
			      (test #t 'post2 did-post2)
			      (test (not should-done-break?) 'done did-done))))))))))
    (for-each 
     (lambda (mk-t)
       (for-each 
	(lambda (nada)
	  ;; Basic checks --- dynamic-wind thunks don't explicitly enable breaks
	  (go mk-t #f  nada nada nada  sw sw sw  void #f #f void #f void #f #f)
	  (go mk-t #f  nada nada nada  sw sw sw  break-thread #f 'pre-act void #f void #f #f)
	  (go mk-t #f  nada nada nada  sw sw sw  void #f #f break-thread 'act void #f #f)
	  (go mk-t #f  nada nada nada  sw sw sw  void #f #f void #f break-thread #f 'done)

	  ;; All dynamic-wind thunks enable breaks
	  (map (lambda (break-on sw)
		 (go mk-t #f  break-on break-on break-on  sw sw sw  void #f #f void #f void #f #f)
		 (go mk-t #f  break-on break-on break-on  sw sw sw  break-thread 'pre #f void #f void #f #f)
		 (go mk-t #f  break-on break-on break-on  sw sw sw  void #f #f break-thread 'act void #f #f)
		 (go mk-t #f  break-on break-on break-on  sw sw sw  void #f #f void #f break-thread 'post #f))
	       (list break-on void)
	       (list sw semaphore-wait/enable-break))

	  ;; Enable break in pre or act shouldn't affect post
	  (go mk-t #f  break-on nada nada  sw sw sw  void #f #f void #f break-thread #f 'done)
	  (go mk-t #f  nada break-on nada  sw sw sw  void #f #f void #f break-thread #f 'done)
	  
	  ;; Enable break in pre shouldn't affect act/done
	  (go mk-t #t  break-on nada nada  sw sw sw  void #f #f break-thread #f void #f #f)
	  (go mk-t #t  break-on nada nada  sw sw sw  void #f #f void #f break-thread #f #f))
	(list void sleep)))
     ;; We'll make threads in three modes: normal, restore a continuation into pre,
     ;;  and restore a continuation into act
     (let* ([no-capture (lambda (reset body) (body))]
	    [plain-mk-t (lambda args
			  (apply mk-t 
				 (lambda (f) (f))
				 no-capture no-capture no-capture
				 args))]
	    [mk-capturing (lambda (which)
			    (let* ([k+reset #f]
				   [capture (lambda (reset body)
					      (let/cc k 
						(set! k+reset (cons k reset)))
					      (body))])
			      ;; Grab a continuation for the dyn-wind's pre/act/post
			      (go (lambda args
                                    (printf "here???\n")
                                    (printf "??? ~s\n" k+reset)
                                    (printf "??? ~s\n" capture)
				    (apply mk-t 
					   (lambda (f) (f))
					   (if (eq? which 'pre) capture no-capture)
					   (if (eq? which 'act) capture no-capture)
					   (if (eq? which 'post) capture no-capture)
					   args))
				  #f  void void void  sw sw sw  void #f #f void #f void #f #f)
			      (lambda args
				(apply mk-t 
				       (lambda (f)
					 ;; First, reset the arguments that are in the continuation's
					 ;;  state
					 (apply (cdr k+reset) no-capture no-capture no-capture args)
					 ;; Now restore the continuation
					 ((car k+reset)))
				       no-capture no-capture no-capture
				       args))))])
       (list plain-mk-t
	     (mk-capturing 'pre)
	     (mk-capturing 'act))))))

;; ----------------------------------------
;; Check wrap-evt result superceded by internally
;;  installed constant (i.e., the input port):

(let ([p (make-input-port
	  'test
	  (lambda (bstr) never-evt)
	  (lambda (bstr skip-count progress-evt)
	    (wrap-evt always-evt (lambda (_) 17)))
	  void)])
  ;; Make sure we don't get 17
  (test p sync p))

;; ----------------------------------------
;; Check large `choice-evt' chain in reasonable time (e.g., not quadratic)

(let ([N 50000])
  (test
   #t
   (lambda (v) (< -1 v N))
   (sync
    (for/fold ([e (wrap-evt always-evt (lambda (x) 0))]) ([i (in-range N)])
      (choice-evt (wrap-evt always-evt (lambda (x) i)) e)))))

;; ----------------------------------------
 ;; box-cas! tests

;; successful cas
(let ()
  (define b (box #f))
  (test #true box-cas! b #f #true)
  (test #true unbox b))

;; unsuccessful cas
(let ()
  (define b (box #f))
  (test #f box-cas! b #true #f)
  (test #f unbox b))

;; cas using allocated data
(let ()
  (define b (box '()))
  (define x (cons 1 (unbox b)))
  (test #true box-cas! b '() x)
  (test x unbox b)
  (test #true box-cas! b x '())
  (test '() unbox b)
  (test #f box-cas! b x '())
  (test '() unbox b))

(let ([g (lambda (x y) y)])
  (err/rt-test (box-cas! (impersonate-box (box 1) g g) 1 2))
  (err/rt-test (box-cas! (chaperone-box (box 1) g g) 1 2))
  (err/rt-test (box-cas! (box-immutable 1) 1 2)))

;; ----------------------------------------

(err/rt-test (sync/enable-break #f (make-semaphore 1)))
(test #t semaphore? (sync/enable-break (make-semaphore 1)))
(test #t semaphore? (sync/timeout/enable-break #f (make-semaphore 1)))

;; ----------------------------------------

(report-errs)
