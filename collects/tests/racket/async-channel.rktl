

(load-relative "loadtest.rktl")

(require scheme/async-channel)

(Section 'async-channel)

(arity-test make-async-channel 0 1)
(err/rt-test (make-async-channel 0) exn?)
(err/rt-test (make-async-channel 1.0) exn?)
(err/rt-test (make-async-channel -1) exn?)

(err/rt-test (async-channel-get #f) exn?)
(err/rt-test (async-channel-try-get #f) exn?)
(err/rt-test (async-channel-put #f 1) exn?)
(err/rt-test (async-channel-put-evt #f 1) exn?)

(let ([ch (make-async-channel)])
  (test #t async-channel? ch)
  (test #f async-channel-try-get ch)
  (test #f sync/timeout 0 ch)
  (test (void) async-channel-put ch 12)
  (test 12 async-channel-get ch)
  (test #f async-channel-try-get ch)
  (test (void) async-channel-put ch 1)
  (test (void) async-channel-put ch 2)
  (test 1 async-channel-get ch)
  (test 2 async-channel-get ch)
  (test #f async-channel-try-get ch)
  (test (void) async-channel-put ch 1)
  (test (void) async-channel-put ch 2)
  (test 1 async-channel-try-get ch)
  (test 2 async-channel-try-get ch)
  (test #f async-channel-try-get ch)
  (let ([p (async-channel-put-evt ch 10)])
    (test p sync/timeout #f p)
    (test 10 async-channel-get ch)
    (test p sync/timeout 0 p)
    (test p sync/timeout #f p p)
    (test 10 async-channel-get ch)
    (test 10 async-channel-get ch)
    (test #f async-channel-try-get ch)))

;; Make sure a channel isn't managed by a
;; custodian:
(for-each
 (lambda (try-after-shutdown)
   (let ([c (make-custodian)]
	 [ch2 #f])
     (parameterize ([current-custodian c])
       (thread-wait
	(thread (lambda ()
		  (set! ch2 (make-async-channel))
		  (async-channel-put ch2 42)))))
     (custodian-shutdown-all c)
     (try-after-shutdown ch2)))
 (list
  (lambda (ch2)
    (test 42 async-channel-get ch2))
  (lambda (ch2)
    (test 42 async-channel-try-get ch2))
  (lambda (ch2)
    (test (void) async-channel-put ch2 10))
  (lambda (ch2)
    (let ([p (async-channel-put-evt ch2 10)])
      (test p sync/timeout 0 p)))))

;; Limited channel:
(define (check-limited n)
  (let ([ch (make-async-channel n)])
    (async-channel-put ch 42)
    (test 42 async-channel-get ch)
    (let ([p (async-channel-put-evt ch 10)])
      (let loop ([n n])
        (unless (zero? n)
          (test p sync/timeout #f p)
          (loop (sub1 n))))
      (test #f sync/timeout 0.01 p)
      (test #f sync/timeout 0 p)
      (let ([s (make-semaphore)])
        (thread (lambda () 
                  (sync (system-idle-evt))
                  (semaphore-post s)))
        (test s sync p s))
      (test 10 async-channel-get ch)
      (test p sync/timeout #f p))))

(check-limited 1)
(check-limited 3)

(report-errs)

