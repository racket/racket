
(load-relative "loadtest.rktl")

(Section 'threadlib)

(require mzlib/thread)

(define sema (make-semaphore))
(define sema2 (make-semaphore))
(define c-out 0)
(define SLEEP-TIME 0.1)

;;; consumer-thread ;;;

(define-values (th g) (consumer-thread (case-lambda
					[(f arg) (set! c-out (f arg))
						 (semaphore-post sema)]
					[(f arg1 arg2) (set! c-out (f arg1 arg2))
						       (semaphore-post sema)])))
(g + 1 2)
(semaphore-wait sema)
(test 3 'consumer-thread c-out)

; queue 2
(g car '(4 5))
(g semaphore-wait sema2)
(semaphore-wait sema)
(test 4 'consumer-thread c-out)
(semaphore-post sema2)
(semaphore-wait sema)
(test (void) 'consumer-thread c-out)

; queue 3
(g / 2)
(g semaphore-wait sema2)
(g (lambda (s) (semaphore-wait s) 5) sema2)
(semaphore-wait sema)
(test 1/2 'consumer-thread c-out)
(semaphore-post sema2)
(semaphore-wait sema)
(test (void) 'consumer-thread c-out)
(semaphore-post sema2)
(semaphore-wait sema)
(test 5 'consumer-thread c-out)

; kill the consumer
(kill-thread th)
(g - 7)
(sleep SLEEP-TIME)
(test 5 'consumer-thread c-out)

(arity-test consumer-thread 1 2)
(err/rt-test (consumer-thread 9))
(arity-test g 2 3)


;; coroutines ----------------------------------------

(define cntr 0)
(define cntr-sema (make-semaphore))
(define w (coroutine (lambda (enable-stop)
		       (let loop ((i 0))
			 (enable-stop #f)
			 (set! cntr i)
                         (when (= cntr 1)
                           (semaphore-post cntr-sema))
			 (enable-stop #t)
			 (loop (add1 i))))))
(test #t coroutine? w)
(test #f coroutine-result w)
(test #f coroutine-run cntr-sema w)
(test #t positive? cntr)
(test (void) coroutine-kill w)
(test #t coroutine-run 100 w)

(define w2 (coroutine (lambda (enable-stop)
			(let loop ((i 100))
			  (cond 
			   ((< i 0) 13)
			   (else
			    (enable-stop #f)
			    (set! cntr i)
			    (enable-stop #t)
			    (loop (sub1 i))))))))
(test #t coroutine-run (system-idle-evt) w2)
(test 13 coroutine-result w2)
(test #t coroutine-run 100 w2)

(define w3 (coroutine (lambda (enable-stop)
			(raise 14))))
(err/rt-test (coroutine-run (system-idle-evt) w3) (lambda (x) (eq? x 14)))
(test #f coroutine-result w3)
(test #t coroutine-run 100 w3)

(define w4 (coroutine (lambda (enable-stop)
			(enable-stop #f)
			(raise 15))))
(test #f coroutine-result w4)
(err/rt-test (coroutine-run (system-idle-evt) w4) (lambda (x) (eq? x 15)))
(test #t coroutine-run 100 w4)

(report-errs)
