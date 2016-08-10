#lang racket/base
;; with thanks to "More: Systems Programming with Racket"
(provide serve current-listen-port)

(require racket/tcp)

(define current-listen-port (make-parameter 12345))

(define (accept-and-handle listener handler)
  (define cust (make-custodian))
  (define handler-thread
    (parameterize ([current-custodian cust])
      (define-values (in out) (tcp-accept listener))
      (file-stream-buffer-mode in 'none)
      (file-stream-buffer-mode out 'none)
      (thread (lambda ()
                (handler in out)
                (close-output-port out)
                (close-input-port in)))))
  (thread (lambda ()
            (thread-wait handler-thread)
            (custodian-shutdown-all cust))))

(define (serve handler)
  (define serving-sem (make-semaphore))
  (define main-cust (make-custodian))
  (define server-thread
    (parameterize ([current-custodian main-cust])
      (define listener (tcp-listen (current-listen-port) 5 #t))
      (semaphore-post serving-sem) ; listening... so caller is ready to continue
      (define (loop)
        (accept-and-handle listener handler)
        (loop))
      (thread loop)))
  (values server-thread (lambda () (custodian-shutdown-all main-cust))))

;; tested via the echo-server (in this directory)
;; (module+ test)
