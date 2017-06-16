;;----------------------------------------------------------------------
;; UDP

(load-relative "loadtest.rktl")

(Section 'udp)

(test #f udp? 5)

;; more type tests in udp.rktl, where we have UDP socket values
(err/rt-test (udp-close 5))
(err/rt-test (udp-bound? 5))
(err/rt-test (udp-connected? 5))
(err/rt-test (udp-bind! 5 #f 40000))
(err/rt-test (udp-connect! 5 "localhost" 40000))
(err/rt-test (udp-send-to 5 "localhost" 40000 #"hello"))
(err/rt-test (udp-send-to* 5 "localhost" 40000 #"hello"))
(err/rt-test (udp-send-to/enable-break 5 "localhost" 40000 #"hello"))
(err/rt-test (udp-send 5 #"hello"))
(err/rt-test (udp-send* 5 #"hello"))
(err/rt-test (udp-send/enable-break 5 #"hello"))
(err/rt-test (udp-receive! 5 (make-bytes 10)))
(err/rt-test (udp-receive!* 5 (make-bytes 10)))
(err/rt-test (udp-receive!/enable-break 5 (make-bytes 10)))
(err/rt-test (udp-receive!-evt 5 (make-bytes 10)))
(err/rt-test (udp-send-ready-evt 5))
(err/rt-test (udp-receive-ready-evt 5))

(arity-test udp-open-socket 0 2)
(arity-test udp-close 1 1)
(arity-test udp? 1 1)
(arity-test udp-bound? 1 1)
(arity-test udp-connected? 1 1)
(arity-test udp-bind! 3 4)
(arity-test udp-connect! 3 3)
(arity-test udp-send-to 4 6)
(arity-test udp-send-to* 4 6)
(arity-test udp-send-to/enable-break 4 6)
(arity-test udp-send 2 4)
(arity-test udp-send* 2 4)
(arity-test udp-send/enable-break 2 4)
(arity-test udp-send-to-evt 4 6)
(arity-test udp-send-evt 2 4)
(arity-test udp-receive! 2 4)
(arity-test udp-receive!* 2 4)
(arity-test udp-receive!/enable-break 2 4)
(arity-test udp-receive!-evt 2 4)
(arity-test udp-send-ready-evt 1 1)
(arity-test udp-receive-ready-evt 1 1)

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
(test #f sync/timeout 0.05 (udp-receive!-evt udp2 (make-bytes 0)))

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


;; check that error-repoting doesn't crash:
(let ()
  (define (q)
    (define s (udp-open-socket #f #f))
    (udp-bind! s "127.0.0.1" 5999)
    s)
  
  (define s (q))
  (err/rt-test (q) exn:fail:network:errno?)
  (udp-close s))


;; UDP Multicast
(let ((s (udp-open-socket)))
  ;; On Windows XP, bind is required before multicast joins:
  (when (eq? 'windows (system-type))
    (test (void) udp-bind! s #f 0 #t))

  (test #t boolean? (udp-multicast-loopback? s))
  (test (void) udp-multicast-set-loopback! s #f)
  (test #f udp-multicast-loopback? s)
  (test (void) udp-multicast-set-loopback! s #t)
  (test #t udp-multicast-loopback? s)
  (test (void) udp-multicast-set-loopback! s 1234) ;; generalized schemely booleans
  (test #t udp-multicast-loopback? s)

  (test #t byte? (udp-multicast-ttl s))
  (test (void) udp-multicast-set-ttl! s 255)
  (test 255 udp-multicast-ttl s)
  (test (void) udp-multicast-set-ttl! s 0)
  (test 0 udp-multicast-ttl s)
  (test (void) udp-multicast-set-ttl! s 1)
  (test 1 udp-multicast-ttl s)
  (err/rt-test (udp-multicast-set-ttl! s 'spoon) exn:fail:contract?)

  (test #t string? (udp-multicast-interface s))
  ;; Using 8.8.8.0 here ought to fail except within a very specific network inside Google
  ;; (see also "whois 8.8.8.0")
  (err/rt-test (udp-multicast-set-interface! s "8.8.8.0") exn:fail:network?)
  (test (void) udp-multicast-set-interface! s "0.0.0.0")
  (test "0.0.0.0" udp-multicast-interface s)
  (test (void) udp-multicast-set-interface! s #f)
  (test "0.0.0.0" udp-multicast-interface s)

  ;; These will only work when the loopback interface is specifically
  ;; on 127.0.0.1. Other commonish possibilities include 127.0.1.1, so
  ;; we can't assume a blanket rule here. See also
  ;; https://github.com/tonyg/racket-nat-traversal/blob/b505a49835a832be98343f45d46f19f8d483edb9/interfaces.rkt#L28-34.
  ;;
  ;; (test (void) udp-multicast-set-interface! s "127.0.0.1")
  ;; (test "127.0.0.1" udp-multicast-interface s)
  ;;
  ;; Oh, but we can use DNS names, thanks to the way Racket's
  ;; address-lookup stuff works!
  (test (void) udp-multicast-set-interface! s "localhost")

  (err/rt-test (udp-multicast-join-group! 'bonk "233.252.0.0" "0.0.0.0") exn:fail:contract?)
  (err/rt-test (udp-multicast-join-group! s 'bonk "0.0.0.0") exn:fail:contract?)
  (err/rt-test (udp-multicast-join-group! s "233.252.0.0" 'bonk) exn:fail:contract?)

  (err/rt-test (udp-multicast-join-group! s "127.0.0.0" "0.0.0.0") exn:fail:network?)
  (err/rt-test (udp-multicast-join-group! s "233.252.0.0" "8.8.8.0") exn:fail:network?)
  ;; http://tools.ietf.org/html/rfc5771 section 9.2:
  ;;
  ;; The first /24 in [AD-HOC Block III], 233.252.0.0/24, is assigned
  ;; as "MCAST-TEST-NET" for use in documentation and example code.
  ;; 233.252.0.0/24 SHOULD be used in conjunction with the [RFC2606]
  ;; domain names example.com or example.net in vendor and protocol
  ;; documentation. Addresses within 233.252.0.0/24 MUST NOT appear on
  ;; the public Internet.
  (err/rt-test (udp-multicast-leave-group! s "233.252.0.0" "0.0.0.0") exn:fail:network?)
  (test (void) udp-multicast-join-group! s "233.252.0.0" "0.0.0.0")
  (test (void) udp-multicast-leave-group! s "233.252.0.0" "0.0.0.0")
  (err/rt-test (udp-multicast-leave-group! s "233.252.0.0" "0.0.0.0") exn:fail:network?)
  (test (void) udp-multicast-join-group! s "233.252.0.0" #f)
  (test (void) udp-multicast-leave-group! s "233.252.0.0" #f)
  (err/rt-test (udp-multicast-leave-group! s "233.252.0.0" #f) exn:fail:network?)
  (test (void) udp-multicast-join-group! s "233.252.0.0" "0.0.0.0")
  (test (void) udp-multicast-leave-group! s "233.252.0.0" #f)
  (test (void) udp-multicast-join-group! s "233.252.0.0" #f)
  (test (void) udp-multicast-leave-group! s "233.252.0.0" "0.0.0.0")

  (unless (eq? 'windows (system-type))
    (test (void) udp-bind! s #f 0 #t))
  (test (void) udp-multicast-join-group! s "233.252.0.0" "localhost")

  (let*-values (((la lp ra rp) (udp-addresses s #t))
		((s2) (udp-open-socket))
		((b) (make-bytes 8 0)))
    (test (void) udp-multicast-set-interface! s2 "localhost")
    (test (void) udp-send-to s2 "233.252.0.0" lp #"hi")
    (sleep 0.05) ; (sync (udp-receive-ready-evt s))
    (let-values (((packet-length ra1 rp1) (udp-receive!* s b)))
      (test 2 values packet-length)
      (test #"hi\0\0\0\0\0\0" values b))

    (test (void) udp-multicast-leave-group! s "233.252.0.0" "localhost")

    (test (void) udp-send-to s2 "233.252.0.0" lp #"hi")
    (sleep 0.05)
    (let-values (((packet-length ra1 rp1) (udp-receive!* s b)))
      (test #f values packet-length))

    (test (void) udp-close s2))

  (test (void) udp-close s)
  ;; It's closed
  (err/rt-test (udp-multicast-loopback? s) exn:fail:network?)
  (err/rt-test (udp-multicast-set-loopback! s #t) exn:fail:network?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
