#lang racket/base
(require ffi/unsafe
         ffi/unsafe/os-thread
         ffi/unsafe/os-async-channel)

(when (os-thread-enabled?)
  (for ([i 100])
    (printf "Try ~s\n" i)
    (define ch (make-os-async-channel))
    (define N 4)
    (define M 100)
    ;; one 0 per thread:
    (for ([j N])
      (os-async-channel-put ch 0))
    ;; each thread increments M times:
    (for ([j N])
      (call-in-os-thread
       (lambda ()
         (for ([k M])
           (define val
             (cond
               [(even? k)
                ;; spins
                (let loop ()
                  (or (os-async-channel-try-get ch)
                      (loop)))]
               [else
                ;; blocks
                (os-async-channel-get ch)]))
           (os-async-channel-put ch (add1 val))))))
    ;; main thread loop; consume numbers from the queue
    ;; until they add up to the number of threads time M;
    ;; put a 0 back each time we're not done, yet
    (let loop ([counter (* N M)] [steps 1])
      (define new-counter (- counter (sync ch)))
      (cond
        [(zero? new-counter)
         (printf "Done in ~a gets\n" steps)]
        [else
         (os-async-channel-put ch 0)
         (loop new-counter (add1 steps))])))

  ;; ----------------------------------------

  ;; make sure an os-async-channel can be used in a finalizer
  
  (define finalized? #f)
  
  (let ()  ;; if `begin`, no problem
    (define c (make-os-async-channel))
    (register-finalizer (list c)
                        (lambda (cs)
                          (os-async-channel-put (car cs) #f)
                          (set! finalized? #t))))

  (for ([i 100])
    (unless finalized?
      (sync (system-idle-evt))
      (collect-garbage)))

  (unless finalized?
    (error "finalizer never called")))
