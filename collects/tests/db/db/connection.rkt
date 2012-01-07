#lang racket/unit
(require (for-syntax racket/base)
         racket/class
         rackunit
         "../config.rkt"
         db/base
         (only-in db/private/generic/common locking%))
(import config^ database^)
(export test^)

(define test
  (test-suite "managing connections"
    (test-case "connection?"
      (call-with-connection
       (lambda (c)
         (check-true (connection? c)))))
    (test-case "connected, disconnect"
      (call-with-connection
       (lambda (c)
         (check-true (connected? c))
         (disconnect c)
         (check-false (connected? c)))))
    (test-case "double disconnect okay"
      (call-with-connection
       (lambda (c)
         (disconnect c)
         (disconnect c))))
    (test-case "dbsystem"
      (call-with-connection
       (lambda (c)
         (let ([sys (connection-dbsystem c)])
           (check-true (dbsystem? sys))
           (check-pred symbol? (dbsystem-name sys))))))

    (test-case "connected?, disconnect work w/ custodian damage"
      (let ([c0 (current-custodian)]
            [c1 (make-custodian)])
        (let ([cx (parameterize ((current-custodian c1))
                    (connect-for-test))])
          ;; cx's ports (if applicable) are managed by c1
          (check-true (connected? cx))
          (custodian-shutdown-all c1)
          (check-completes (lambda () (connected? cx)) "connected?")
          (when (memq dbsys '(mysql postgresql))
            ;; wire-based connection is disconnected; it had better know it
            (check-false (connected? cx)))
          (check-completes (lambda () (disconnect cx)) "disconnect")
          (check-false (connected? cx)))))

    (test-case "connected?, disconnect work w/ kill-thread damage"
      (let ([cx (connect-for-test)])
        (when (is-a? cx locking%)
          (check-true (connected? cx))
          (let ([thd
                 (thread
                  (lambda ()
                    (send cx call-with-lock 'test (lambda () (sync never-evt)))))])
            (kill-thread thd)
            (check-completes (lambda () (connected? cx)) "connected?")
            (check-completes (lambda () (disconnect cx)) "disconnect")
            (check-false (connected? cx))))))
    ))

(define TIMEOUT 2) ;; seconds

(define (check-completes thunk [msg #f])
  (let ([t (thread thunk)])
    (check-equal? (sync/timeout TIMEOUT (wrap-evt t (lambda _ 'completed)))
                  'completed
                  msg)))
