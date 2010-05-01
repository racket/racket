
(load-relative "loadtest.rktl")

(Section 'udp)

(define udp1 (udp-open-socket))
(define us1 (make-bytes 10))

(test #t udp? udp1)
(test #f udp-bound? udp1)
(test #f udp-connected? udp1)

;; not bound:
(err/rt-test (udp-receive! udp1 us1) exn:fail:network?)
(err/rt-test (udp-receive!* udp1 us1) exn:fail:network?)
(err/rt-test (udp-receive!/enable-break udp1 us1) exn:fail:network?)
;; not connected:
(err/rt-test (udp-send udp1 us1) exn:fail:network?)
(err/rt-test (udp-send* udp1 us1) exn:fail:network?)
(err/rt-test (udp-send/enable-break udp1 us1) exn:fail:network?)

(test (void) udp-send-to udp1 "127.0.0.1" 45678 #"knock knock")
(sleep 0.05)
(test-values '(#f #f #f) (lambda () 
			   ;; The send above might cause an error on the next
			   ;;  action, so we try up to 2 times:
			   (with-handlers ([exn:fail?
					    (lambda (x)
					      (udp-receive!* udp1 us1))])
			     (udp-receive!* udp1 us1))))

(test #t udp-bound? udp1)
(test #f udp-connected? udp1)

;; still not connected:
(err/rt-test (udp-send udp1 us1) exn:fail:network?)

(define udp2 (udp-open-socket "127.0.0.1"))

(define (local-bind-port x) 
  (let-values ([(la lp pa pp) (udp-addresses x #t)])
   lp))

(test (void) udp-bind! udp2 "127.0.0.1" 0)
(define port (local-bind-port udp2))
(test-values '(#f #f #f) (lambda () (udp-receive!* udp2 us1)))

(test (void) udp-send-to udp1 "127.0.0.1" port #"Hiya.")

(define recv-got (call-with-values (lambda () (udp-receive! udp2 us1)) list))
(test 5 car recv-got)
(test "127.0.0.1" cadr recv-got)
(define udp1-port (caddr recv-got))
(test #"Hiya.\0\0\0\0\0" values us1)

(test (void) udp-send-to udp1 "127.0.0.1" port  #"...another?..." 3 11)
(test-values (list 8 "127.0.0.1" udp1-port) (lambda () (udp-receive! udp2 us1 1)))
(test #"Hanother?\0" values us1)

(test (void) udp-connect! udp1 "127.0.0.1" port )
(test #t udp-connected? udp1)
(test #f udp-connected? udp2)

(test (void) udp-send udp1 #"truncate me")
(test-values (list 6 "127.0.0.1" udp1-port) (lambda () (udp-receive! udp2 us1 2 8)))
(test #"Hatrunca?\0" values us1)

(test #t udp-send* udp1 #"SKIPall of it" 4)
(test-values (list 9 "127.0.0.1" udp1-port) (lambda () (udp-receive! udp2 us1 0)))
(test #"all of it\0" values us1)

(define (flush-udp-errors udp)
  (let loop ()
    (with-handlers ([exn:fail?
		     (lambda (x) (loop))])
       (udp-receive!* udp1 us1))))

;; re-connect
(test (void) udp-connect! udp1 "127.0.0.1" 40008)
(test #t udp-connected? udp1)
(test (void) udp-send udp1 #"lots of stuff")
(sleep 0.05)
(flush-udp-errors udp1)
(test-values '(#f #f #f) (lambda () (udp-receive!* udp2 us1)))
(err/rt-test (udp-send-to udp1 "127.0.0.1" port  #"not ok -- currently connected") exn:fail:network?)
(test #t udp-send* udp1 #"lots of stuff")
(sleep 0.05)
(flush-udp-errors udp1)
(test-values '(#f #f #f) (lambda () (udp-receive!* udp2 us1)))

;; disconnect
(test (void) udp-connect! udp1 #f #f)
(test #f udp-connected? udp1)
(test (void) udp-connect! udp2 #f #f)
(test #f udp-connected? udp2)

;; waitables
(define udp1-s (udp-send-ready-evt udp1))
(test #t evt? udp1-s)
(test udp1-s sync udp1-s)

(define udp2-r (udp-receive-ready-evt udp2))
(test #t evt? udp2-r)
(test #f sync/timeout 0.05 udp2-r)

(test (void) sync (udp-send-to-evt udp1 "127.0.0.1" port #"here's more"))
(sleep 0.05)
(test udp2-r sync udp2-r)
(test udp2-r sync udp2-r)
(test (list 10 "127.0.0.1") 
      ;; Above disconnect may change udp1's auto-assigned port,
      ;;  so don't check the port number:
      (lambda (l)
        (if (and (list? l)
                 (= 3 (length l)))
            (list (car l) (cadr l))
            l))
      (sync (udp-receive!-evt udp2 us1)))
(test #f sync/timeout 0.05 udp2-r)
(test #f sync/timeout 0.05 (udp-receive!-evt udp2 us1))

;; break behavior
(let ([t (parameterize-break #f
           (thread (lambda ()
		     (udp-receive!/enable-break udp1 us1)
		     (set! udp1 #f))))])
  (sleep 0.05)
  (break-thread t)
  (thread-wait t)
  (test #t udp? udp1))
;; filling up an output queue is difficult; we don't even try here

(err/rt-test (udp-bind! udp1 #f #f))
(err/rt-test (udp-bind! udp1 "127.0.0.1" #f))
(err/rt-test (udp-connect! udp1 "127.0.0.1" #f) exn:application:mismatch?)
(err/rt-test (udp-connect! udp1 #f 5) exn:application:mismatch?)
(err/rt-test (udp-send-to udp1 #f 40000 #"hello"))
(err/rt-test (udp-send-to udp1 "127.0.0.1" #f #"hello"))
(err/rt-test (udp-send-to udp1 "127.0.0.1" 40000 'hello))
(err/rt-test (udp-send-to udp1 "127.0.0.1" 40000 "hello"))
(err/rt-test (udp-send-to udp1 "127.0.0.1" 40000 #"hello" #f))
(err/rt-test (udp-send-to udp1 "127.0.0.1" 40000 #"hello" 1 #f))
(err/rt-test (udp-send-to udp1 "127.0.0.1" 40000 #"hello" 10) exn:application:mismatch?)
(err/rt-test (udp-send-to udp1 "127.0.0.1" 40000 #"hello" 1 11) exn:application:mismatch?)
(err/rt-test (udp-send-to udp1 "127.0.0.1" 40000 #"hello" 1 0) exn:application:mismatch?)
(err/rt-test (udp-send udp1 'hello))
(err/rt-test (udp-send udp1 #"hello" #f))
(err/rt-test (udp-send udp1 #"hello" 2 #f))
(err/rt-test (udp-send udp1 #"hello" 12) exn:application:mismatch?)
(err/rt-test (udp-send udp1 #"hello" 1 13) exn:application:mismatch?)
(err/rt-test (udp-send udp1 #"hello" 1 0) exn:application:mismatch?)
(err/rt-test (udp-receive! udp1 "constant"))
(err/rt-test (udp-receive! udp1 'hello))
(err/rt-test (udp-receive!* udp1 (make-bytes 10) #f))
(err/rt-test (udp-receive!* udp1 (make-bytes 10) 2 #f))
(err/rt-test (udp-receive!* udp1 (make-bytes 10) 12) exn:application:mismatch?)
(err/rt-test (udp-receive!* udp1 (make-bytes 10) 2 12) exn:application:mismatch?)
(err/rt-test (udp-receive!* udp1 (make-bytes 10) 2 0) exn:application:mismatch?)


(test (void) udp-close udp1)
(test (void) udp-close udp2)

;; udp1 is now closed...
(err/rt-test (udp-bind! udp1 "127.0.0.1" 40008) exn:fail:network?)
(err/rt-test (udp-connect! udp1 "127.0.0.1" port) exn:fail:network?)
(err/rt-test (udp-send-to udp1 "127.0.0.1" 40000 #"hello") exn:fail:network?)
(err/rt-test (udp-send udp1 #"hello") exn:fail:network?)
(err/rt-test (udp-receive! udp1 (make-bytes 10)) exn:fail:network?)
(err/rt-test (udp-close udp1) exn:fail:network?)

;; Can still get waitable after closed:
(test #t evt? (udp-send-ready-evt udp1))
(test #t evt? (udp-receive-ready-evt udp1))
(let ([w (udp-send-ready-evt udp1)])
  (test w sync w))
(let ([w (udp-receive-ready-evt udp1)])
  (test w sync w))
(test #t evt? (udp-receive!-evt udp1 us1))
(test #t evt? (udp-send-to-evt udp1 "127.0.0.1" port #"here's more"))
