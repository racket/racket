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

(define (async-test)
  (test-case "asynchronous execution"
    (unless (ANYFLAGS 'ismy 'isora 'isdb2)
      (call-with-connection
       (lambda (c)
         (query-exec c "create temporary table nums (n integer)")
         (for ([i (in-range 40)])
           (query-exec c (sql "insert into nums (n) values ($1)") i))
         (let* ([the-sql "select cast(max(a.n * b.n *c.n * d.n) as varchar) \
                          from nums a, nums b, nums c, nums d"]
                [pst (prepare c the-sql)]
                [sema (make-semaphore 0)]
                [peek (semaphore-peek-evt sema)]
                [counter 0]
                [thd
                 (thread (lambda ()
                           (let loop ()
                             (sync peek)
                             (set! counter (add1 counter))
                             (sleep 0.01)
                             (loop))))])
           (let ([start (current-inexact-milliseconds)])
             (semaphore-post sema)
             (query-value c pst)
             (semaphore-wait sema)
             (let ([end (current-inexact-milliseconds)])
               (when (ANYFLAGS 'postgresql 'mysql 'async)
                 (when #f
                   (printf "counter = ~s\n" counter)
                   (printf "time elapsed = ~s\n" (- end start)))
                 ;; If c does not execute asynchronously, expect counter to be about 0.
                 (check-pred positive? counter)
                 (let ([expected-counter (/ (- end start) (* 0.01 1000))])
                   (check > counter (* 0.5 expected-counter))))))))))))

;; ----

(define test
  (test-suite "Concurrency"
    (async-test)
    ;; Tests whether connections are properly locked.
    (test-concurrency 1)
    (test-concurrency 2)
    (test-concurrency 20)
    (kill-safe-test #t)))
