#lang racket/unit
(require racket/class
         rackunit
         db/base
         "../config.rkt")
(import database^ config^)
(export test^)

(define (test-concurrency workers)
  (test-case (format "lots of threads (~s)" workers)
    (unless (ANYFLAGS 'isora 'isdb2)
      (call-with-connection
       (lambda (c)
         (query-exec c "create temporary table play_numbers (n integer)")
         ;; transaction speeds up test by a factor of 6 on postgresql
         (query-exec c "begin")
         (let ([exns null])
           (parameterize ((uncaught-exception-handler
                           (lambda (e) (set! exns (cons e exns)) ((error-escape-handler)))))
             (for-each thread-wait
                       (map thread
                            (map (mk-worker c 100) (build-list workers add1)))))
           (when (pair? exns)
             (raise (make-exn (string-append "exception in thread: " (exn-message (car exns)))
                              (exn-continuation-marks (car exns)))))))))))

(define (((mk-worker c iterations) tid))
  (define insert-pst
    (prepare c (sql "insert into play_numbers (n) values ($1)")))
  (define (insert x) (query-exec c insert-pst x))
  (define (add-to-max n)
    (let* ([m0 (query-value c "select max(n) from play_numbers")]
           [m (if (string? m0) (string->number m0) m0)])
      (insert (+ n m))))
  (for-each insert (build-list iterations add1))
  (for-each add-to-max (build-list iterations add1))
  (when NOISY?
    (printf "~s: ~s\n"
            tid
            (query-value c "select max(n) from play_numbers"))))

(define (kill-safe-test proxy?)
  (test-case (format "kill-safe test~a" (if proxy? " (proxy)" ""))
    (unless (ANYFLAGS 'isora 'isdb2)
    (call-with-connection
     (lambda (c0)
       (let ([c (if proxy?
                    (kill-safe-connection c0)
                    c0)])
         (query-exec c "create temporary table ks_numbers (n integer)")
         (for ([i (in-range 1000)])
           (query-exec c (sql "insert into ks_numbers (n) values ($1)") i))
         (define (do-interactions)
           (for ([i (in-range 10)])
             (query-list c "select n from ks_numbers")))
         (define threads (make-hasheq))

         (for ([i (in-range 20)])
           (let ([t (thread do-interactions)])
             (hash-set! threads (thread do-interactions) #t)
             (kill-thread t)))
         (for ([t (in-hash-keys threads)])
           (sync t))))))))

;; ----

(define pool-test
  (test-suite "connection pools"
    (test-case "lease, limit, release"
      (let* ([counter 0]
             [p (connection-pool (lambda () (set! counter (+ 1 counter)) (connect-for-test))
                                 #:max-connections 2)]
             [c1 (connection-pool-lease p)]
             [c2 (connection-pool-lease p)])
        ;; Two created
        (check-equal? counter 2)
        ;; Can't create new one yet
        (check-exn exn:fail? (lambda () (connection-pool-lease p)))
        ;; But if we free one...
        (disconnect c2)
        (check-equal? (connected? c2) #f)
        (let ([c3 (connection-pool-lease p)])
          (check-equal? counter 2 "not new") ;; used idle, not new connection
          (check-equal? (connected? c3) #t))))
    (test-case "release on evt"
      (let* ([p (connection-pool connect-for-test #:max-connections 2)]
             [sema (make-semaphore 0)]
             [c1 (connection-pool-lease p sema)])
        (check-equal? (connected? c1) #t)
        ;; Closes when evt ready
        (begin (semaphore-post sema) (sleep 0.1))
        (check-equal? (connected? c1) #f)))
    (test-case "release on custodian"
      (let* ([p (connection-pool connect-for-test #:max-connections 2)]
             [cust (make-custodian)]
             [c1 (connection-pool-lease p cust)])
        (check-equal? (connected? c1) #t)
        ;; Closes when custodian shutdown
        (begin (custodian-shutdown-all cust) (sleep 0.1))
        (check-equal? (connected? c1) #f)))))

;; ----

(define test
  (test-suite "Concurrency"
    ;; Tests whether connections are properly locked.
    (test-concurrency 1)
    (test-concurrency 2)
    (test-concurrency 20)
    (kill-safe-test #t)
    pool-test))
