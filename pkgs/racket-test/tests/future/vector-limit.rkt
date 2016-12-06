#lang racket
(require racket/future)

;; A test for `make-vector` in futures, especially the interaction
;; with memory limits.

(define (try size)
  (define c (make-custodian))
  (custodian-limit-memory c (* 4 #x1000000))
  (define cb (make-custodian-box c 'running))
  (define ok? #f)
  (thread-wait
   (parameterize ([current-custodian c])
     (thread
      (lambda ()
        (define (go n w)
          (let loop ([v w])
            (loop (cons #;(if v (lambda (x y) y) cons) (make-vector n) v))))
        
        ;; Will executors are here as memory-debugging canaries:
        (define f1 (future (let ([w (make-will-executor)])
                             (lambda () (let ([v w])
                                     (set! w #f)
                                     (go size v))))))
        (define f2 (future (lambda () (let loop () (loop)))))
        
        
        (define (check f)
          (with-handlers ([exn:fail:out-of-memory? (lambda (exn)
                                                     (set! ok? #t))])
            (touch f)))

        (sleep 1)
        
        (check f1)))))
  (unless (or ok?
              (not (custodian-box-value cb)))
    (error "didn't get out-of-memory or shutdown as expected"))
  (printf "ok at ~a\n" size))

(try #x10000)
(try #x100000)
(try #x1000000)
(try #x10000000)
