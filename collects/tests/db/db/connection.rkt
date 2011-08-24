#lang racket/unit
(require (for-syntax racket/base)
         rackunit
         "../config.rkt"
         db/base)
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

    (test-case "connected?, disconnect work w/ custodian 'damage'"
      (let ([c0 (current-custodian)]
            [c1 (make-custodian)])
        (let ([cx (parameterize ((current-custodian c1))
                    (connect-for-test))])
          ;; cx's ports (if applicable) are controlled by c1
          (check-true (connected? cx))
          (custodian-shutdown-all c1)
          (check-completes (lambda () (connected? cx)) "connected?")
          (when (memq dbsys '(mysql postgresql))
            (check-false (connected? cx)))
          (check-completes (lambda () (disconnect cx)) "disconnect"))))

    ;; FIXME: Still need to test the disconnect works on cx left locked
    ;; because of kill-thread (currently probably doesn't for sqlite3,odbc).
    ;; ie: "connected?, disconnect work w/ kill-thread 'damage'"
    ))

(define TIMEOUT 2) ;; seconds

(define (check-completes thunk [msg #f])
  (let ([t (thread thunk)])
    (check-equal? (sync/timeout TIMEOUT (wrap-evt t (lambda _ 'completed)))
                  'completed
                  msg)))
