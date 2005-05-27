
(if (not (defined? 'SECTION))
    (load-relative "testing.ss"))

(SECTION 'mzlib-threads)

(require-library "thread.ss")

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
(error-test '(consumer-thread 9))
(arity-test g 2 3)

;;; semaphore-wait-multiple ;;;

(test #f semaphore-wait-multiple (list sema sema2) SLEEP-TIME)
(semaphore-post sema)
(test sema semaphore-wait-multiple (list sema sema2))
(test #f semaphore-wait-multiple (list sema sema2) SLEEP-TIME)
(semaphore-post sema2)
(test sema2 semaphore-wait-multiple (list sema sema2))
(test #f semaphore-wait-multiple (list sema sema2) SLEEP-TIME)
(semaphore-post sema)
(semaphore-post sema2)
(let ([first (semaphore-wait-multiple (list sema sema2))])
  (test #t semaphore? first)
  (test (if (eq? first sema) sema2 sema) semaphore-wait-multiple (list sema sema2)))
(test #f semaphore-wait-multiple (list sema sema2) SLEEP-TIME)

(arity-test semaphore-wait-multiple 1 3)

(report-errs)
