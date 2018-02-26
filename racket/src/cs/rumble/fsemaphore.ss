;; future semaphores

;; just copied from expander-compat.scm
(define-record-type (fsemaphore create-fsemaphore fsemaphore?)
  (fields sema))

(define (make-fsemaphore init)
  (create-fsemaphore (make-semaphore init)))

(define (fsemaphore-post fsema)
  (semaphore-post (fsemaphore-sema f)))

(define (fsemaphore-wait fsema)
  (semaphore-wait (fsemaphore-sema f)))

(define (fsemaphore-try-wait fsema)
  (semaphore-try-wait? (fsemaphore-sema f)))

(define (fsemaphore-count fsema)
  (semaphore-count (fsemaphore-sema f)))
